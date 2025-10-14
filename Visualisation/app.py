# app_streamlit.py
import numpy as np
import pandas as pd
import plotly.graph_objects as go
import streamlit as st
from pathlib import Path

CSV_PATH = Path("/Users/jagdishsingh/Downloads/new_final_species.csv")

@st.cache_data
def load_data(path: Path) -> pd.DataFrame:
    if not path.exists():
        st.error(f"Couldn't find file at {path}. Update CSV_PATH.")
        st.stop()
    df_raw = pd.read_csv(path, low_memory=False)
    df = df_raw.copy()
    df.columns = [
        c.strip().lower().replace(" ", "_").replace("/", "_").replace("-", "_")
        for c in df.columns
    ]
    return df

def first_existing(cols, df_cols):
    for col in cols:
        if col in df_cols:
            return col
    return None

def prep_stats(df: pd.DataFrame) -> pd.DataFrame:
    col_tmin = first_existing(["tempmin","temp_min","temperature_min"], df.columns)
    col_tmax = first_existing(["tempmax","temp_max","temperature_max"], df.columns)
    col_species = first_existing(["species","species_canonical","scientificname","scientific_name"], df.columns)
    col_status = first_existing(["sequencing_status","sequencial_status","sequence_status","status_sequencing"], df.columns)
    col_aphia = first_existing(["aphia_id","aphiaid"], df.columns)

    if not col_aphia or not col_species or not (col_tmin or col_tmax):
        st.error("Missing required columns in CSV.")
        st.stop()

    tmin = pd.to_numeric(df[col_tmin], errors="coerce") if col_tmin else None
    tmax = pd.to_numeric(df[col_tmax], errors="coerce") if col_tmax else None

    if tmin is not None and tmax is not None:
        temp = np.where(~tmin.isna() & ~tmax.isna(), (tmin + tmax)/2.0,
                        np.where(~tmin.isna(), tmin, tmax))
    elif tmin is not None:
        temp = tmin.to_numpy()
    else:
        temp = tmax.to_numpy()

    base = pd.DataFrame({
        "aphia_id": df[col_aphia].astype(str),
        "species": df[col_species].astype(str),
        "temp": pd.to_numeric(temp, errors="coerce")
    })
    if col_status:
        base["status"] = (
            df[col_status].astype(str)
            .replace(["nan","None",""," "], np.nan)
            .fillna("unknown")
        )
    else:
        base["status"] = "All"

    base = base.dropna(subset=["temp"])
    base = base[(base["temp"] > -5) & (base["temp"] < 60)]

    stats = (
        base.groupby(["status","aphia_id","species"])["temp"]
        .agg(tmin="min", tavg="mean", tmax="max", n="count")
        .reset_index()
    )
    return stats

def make_bins(series: pd.Series, width=2.0):
    lo = np.floor(series.min() / width) * width
    hi = np.ceil(series.max() / width) * width
    edges = np.arange(lo, hi + width + 1e-9, width)
    return edges

def binned_counts(stats, bin_edges, status_filter="All", aphia_filter=None):
    df = stats if status_filter == "All" else stats[stats["status"] == status_filter]
    if aphia_filter:
        df = df[df["aphia_id"].astype(str) == str(aphia_filter).strip()]

    df = df.copy()
    df["bin_idx"] = np.digitize(df["tavg"], bin_edges, right=True) - 1
    df["bin_idx"] = df["bin_idx"].clip(0, len(bin_edges)-2)

    counts = df.groupby("bin_idx")["aphia_id"].nunique()
    counts = counts.reindex(range(len(bin_edges)-1), fill_value=0)

    out = []
    for i in range(len(bin_edges)-1):
        out.append({
            "bin": f"{int(bin_edges[i])}–{int(bin_edges[i+1])}",
            "left": float(bin_edges[i]),
            "right": float(bin_edges[i+1]),
            "count": int(counts.iloc[i])
        })
    return pd.DataFrame(out)

def make_chart(stats, bin_edges, status_sel="All", aphia_sel=None):
    data = binned_counts(stats, bin_edges, status_sel, aphia_sel)
    figure = go.Figure(go.Bar(
        x=data["bin"],
        y=data["count"],
        customdata=np.c_[data["left"], data["right"]],
        hovertemplate=(
            "Temperature bin: %{customdata[0]:.0f}–%{customdata[1]:.0f} °C<br>"
            "Distinct AphiaIDs: %{y}"
        )
    ))
    title_suffix = f"Status={status_sel}, AphiaID={aphia_sel or 'All'}"
    figure.update_layout(
        title=f"Species (by AphiaID) per 2 °C bin — {title_suffix}",
        xaxis_title="Average temperature bin (°C)",
        yaxis_title="Distinct AphiaIDs",
        margin=dict(l=10, r=10, t=40, b=10)
    )

    if aphia_sel:
        row = stats[
            (stats["aphia_id"].astype(str) == str(aphia_sel)) &
            ((stats["status"] == status_sel) | (status_sel == "All"))
        ]
        if not row.empty:
            r = row.iloc[0]
            idx = int(np.digitize([r["tavg"]], bin_edges, right=True) - 1)
            idx = max(0, min(idx, len(bin_edges)-2))
            overlay_custom = [[r["species"], float(r["tmin"]), float(r["tavg"]), float(r["tmax"])]]
            figure.add_bar(
                x=[data.loc[idx, "bin"]],
                y=[1],
                customdata=overlay_custom,
                hovertemplate=(
                    "Species: %{customdata[0]}<br>"
                    "Min–Avg–Max: %{customdata[1]:.2f} – %{customdata[2]:.2f} – %{customdata[3]:.2f} °C"
                ),
                marker_opacity=0.0,
                showlegend=False
            )
            figure.add_annotation(
                x=data.loc[idx, "bin"],
                y=data.loc[idx, "count"],
                text=f"{r['species']}<br>{r['tmin']:.1f}–{r['tmax']:.1f} °C",
                showarrow=False,
                yshift=12,
                font=dict(size=12)
            )
    return figure

def main():
    st.title("Species by Average Temperature (2 °C bins)")

    df = load_data(CSV_PATH)
    stats = prep_stats(df)

    statuses = ["All"] + sorted([s for s in stats["status"].unique() if s != "All"])
    status_sel = st.selectbox("Status", statuses, index=0)

    aphia_options = ["All"] + sorted(
        stats[stats["status"].eq(status_sel) | (status_sel == "All")]["aphia_id"].astype(str).unique(),
        key=lambda x: (len(x), x)
    )
    aphia_sel = st.selectbox("AphiaID", aphia_options, index=0)

    selected_aphia = None if aphia_sel == "All" else aphia_sel

    st.markdown(
        f"<div style='font-size:13px;'>"
        f"<b>Current filters:</b> Status = <code>{status_sel}</code>, AphiaID = <code>{selected_aphia or 'All'}</code>"
        f"</div>",
        unsafe_allow_html=True
    )

    bin_edges = make_bins(stats["tavg"], width=2.0)
    fig = make_chart(stats, bin_edges, status_sel, selected_aphia)
    st.plotly_chart(fig, use_container_width=True)

if __name__ == "__main__":
    main()
