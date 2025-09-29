"""
Ocean Genomes – minimal Python helper for Shiny (via reticulate)

Exports:
- load_csv(path) -> pd.DataFrame
- add_features(df) -> pd.DataFrame
- family_progress_table(df) -> (pd.DataFrame, pd.DataFrame)
- plot_progress_funnel(df) -> plotly.graph_objs.Figure
- plot_family_activity(fam_active) -> plotly.graph_objs.Figure
- plot_habitat_progress_share(df) -> plotly.graph_objs.Figure
- build_taxonomy_sunburst(df) -> plotly.graph_objs.Figure
- fig_to_html(fig) -> str  (interactive HTML, Plotly CDN)

Convenience:
- render_all_html(csv_path) -> dict: {plot1..plot4} (HTML strings)
"""

from dataclasses import dataclass
from typing import Tuple, Dict, List
import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import plotly.io as pio


# ============================
# Schema & constants
# ============================

@dataclass(frozen=True)
class COLS:
    TAXON_ID: str = "ncbi_taxon_id"
    SPECIES: str = "species_canonical"
    GENUS: str = "Genus"
    FAMILY: str = "family_x"
    ORDER: str = "order"
    TARGET_LIST_STATUS: str = "target_list_status"
    SEQUENCING_STATUS: str = "sequencing_status"
    RANK: str = "rank"
    MODIFIED: str = "modified"
    IS_MARINE: str = "isMarine"
    IS_BRACKISH: str = "isBrackish"
    IS_FRESHWATER: str = "isFreshwater"
    IS_TERRESTRIAL: str = "isTerrestrial"
    IS_EXTINCT: str = "isExtinct"
    DEPTH_MIN: str = "depth_min_in_m"
    DEPTH_MAX: str = "depth_max_in_m"
    LENGTH_MAX: str = "length_max_in_cm"

STATUS_ORDER: List[str] = [
    "not_started",
    "sample_acquired",
    "data_generation",
    "in_assembly",
    "insdc_open",
]

STATUS_LABELS: Dict[str, str] = {
    "not_started": "Not started",
    "sample_acquired": "Sample acquired",
    "data_generation": "Data generation",
    "in_assembly": "In assembly",
    "insdc_open": "Open in INSDC",
}


# ============================
# Utilities
# ============================

def load_csv(path: str) -> pd.DataFrame:
    """Read a CSV from disk."""
    return pd.read_csv(path)


def _norm_bool(s: pd.Series) -> pd.Series:
    """Coerce mixed boolean-ish values to 0/1 ints."""
    return s.fillna(0).replace({"True": 1, "False": 0, True: 1, False: 0}).astype(int)


def add_features(df: pd.DataFrame) -> pd.DataFrame:
    """
    Clean/augment columns:
      - normalize sequencing_status
      - normalize habitat flags + derive habitat_primary
      - depth_range_m
      - numeric length
      - fill NA in taxonomy
    """
    d = df.copy()

    # sequencing_status
    if COLS.SEQUENCING_STATUS in d.columns:
        d[COLS.SEQUENCING_STATUS] = (
            d[COLS.SEQUENCING_STATUS]
            .fillna("-")
            .replace({"-": "not_started"})
            .astype(str)
            .str.strip()
            .str.lower()
        )

    # habitat flags
    for c in [COLS.IS_MARINE, COLS.IS_BRACKISH, COLS.IS_FRESHWATER, COLS.IS_TERRESTRIAL, COLS.IS_EXTINCT]:
        if c in d.columns:
            d[c] = _norm_bool(pd.to_numeric(d[c], errors="coerce"))

    # primary habitat (simple priority; mark mixed if >1 flag)
    def primary_hab(row) -> str:
        flags = [
            (COLS.IS_MARINE, "marine"),
            (COLS.IS_BRACKISH, "brackish"),
            (COLS.IS_FRESHWATER, "freshwater"),
            (COLS.IS_TERRESTRIAL, "terrestrial"),
        ]
        on = [name for col, name in flags if row.get(col, 0) == 1]
        if not on:
            return "unknown"
        if len(on) == 1:
            return on[0]
        # multiple -> tag as "<first>_mixed"
        return f"{on[0]}_mixed"

    d["habitat_primary"] = d.apply(primary_hab, axis=1)

    # depth range
    if COLS.DEPTH_MIN in d.columns and COLS.DEPTH_MAX in d.columns:
        d[COLS.DEPTH_MIN] = pd.to_numeric(d[COLS.DEPTH_MIN], errors="coerce")
        d[COLS.DEPTH_MAX] = pd.to_numeric(d[COLS.DEPTH_MAX], errors="coerce")
        d["depth_range_m"] = d[COLS.DEPTH_MAX] - d[COLS.DEPTH_MIN]

    # length
    if COLS.LENGTH_MAX in d.columns:
        d[COLS.LENGTH_MAX] = pd.to_numeric(d[COLS.LENGTH_MAX], errors="coerce")

    # taxonomy null-safety
    for c in [COLS.ORDER, COLS.FAMILY, COLS.GENUS]:
        if c in d.columns:
            d[c] = d[c].fillna("Unknown")

    if COLS.SPECIES in d.columns:
        d[COLS.SPECIES] = d[COLS.SPECIES].astype(str)

    return d


# ============================
# Table helpers
# ============================

def family_progress_table(d: pd.DataFrame) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """
    Summary of progress by family; returns (fam_all, fam_active).
    fam_active keeps only families with in_progress or completed > 0.
    """
    req = [COLS.FAMILY, COLS.SPECIES, COLS.SEQUENCING_STATUS]
    if not all(c in d.columns for c in req):
        return pd.DataFrame(), pd.DataFrame()

    fam = (
        d.groupby(COLS.FAMILY, dropna=False)
        .agg(
            total=(COLS.SPECIES, "count"),
            completed=(COLS.SEQUENCING_STATUS, lambda s: int((s == "insdc_open").sum())),
            in_progress=(COLS.SEQUENCING_STATUS, lambda s: int(s.isin(["sample_acquired", "data_generation", "in_assembly"]).sum())),
        )
        .reset_index()
    )

    fam["coverage_pct"] = np.where(fam["total"] > 0, fam["completed"] / fam["total"] * 100.0, 0.0)

    fam_active = fam[(fam["completed"] > 0) | (fam["in_progress"] > 0)].copy()
    fam_active.sort_values(["completed", "in_progress", "total"], ascending=[False, False, True], inplace=True)

    return fam, fam_active


# ============================
# Plots
# ============================

def plot_progress_funnel(d: pd.DataFrame) -> go.Figure:
    counts = (
        d[COLS.SEQUENCING_STATUS]
        .value_counts()
        .reindex(STATUS_ORDER)
        .fillna(0)
        .astype(int)
    )
    fig = px.bar(
        counts.reset_index(),
        x=counts.values,
        y=[STATUS_LABELS.get(s, s) for s in counts.index],
        orientation="h",
        text=counts.values,
    )
    fig.update_layout(
        xaxis_title="Number of species",
        yaxis_title="Research stage",
        showlegend=False,
        margin=dict(l=0, r=10, t=10, b=10),
        height=360,
    )
    fig.update_traces(textposition="outside", cliponaxis=False)
    return fig


def plot_family_activity(fam_active: pd.DataFrame) -> go.Figure:
    if fam_active is None or fam_active.empty:
        fig = go.Figure()
        fig.update_layout(title="No families with activity.", height=80, margin=dict(l=0, r=0, t=30, b=0))
        return fig

    top = fam_active.head(12).melt(
        id_vars=[COLS.FAMILY, "total", "coverage_pct"],
        value_vars=["in_progress", "completed"],
        var_name="stage",
        value_name="count",
    )

    fig = px.bar(
        top,
        x="count",
        y=COLS.FAMILY,
        color="stage",
        orientation="h",
        barmode="stack",
        category_orders={COLS.FAMILY: list(top.sort_values("total", ascending=True)[COLS.FAMILY])},
        labels={"count": "Species", COLS.FAMILY: "Family"},
    )
    fig.update_layout(legend_title="Stage", margin=dict(l=0, r=10, t=10, b=10), height=420)
    return fig


def plot_habitat_progress_share(d: pd.DataFrame) -> go.Figure:
    if "habitat_primary" not in d.columns:
        return go.Figure()

    keep = ["marine", "marine_mixed", "brackish", "freshwater"]
    tmp = d.copy()
    tmp["habitat_display"] = np.where(tmp["habitat_primary"].isin(keep), tmp["habitat_primary"], "other/unknown")

    pivot = (
        tmp.pivot_table(index="habitat_display", columns=COLS.SEQUENCING_STATUS, values=COLS.SPECIES, aggfunc="count")
        .reindex(columns=STATUS_ORDER)
        .fillna(0)
    )
    share = (
        pivot.div(pivot.sum(axis=1), axis=0)
        .reset_index()
        .melt(id_vars="habitat_display", var_name="stage", value_name="share")
    )
    share["stage"] = share["stage"].map(STATUS_LABELS).fillna(share["stage"])

    fig = px.bar(
        share,
        x="habitat_display",
        y="share",
        color="stage",
        barmode="stack",
        labels={"habitat_display": "Habitat (primary)", "share": "Within-habitat share"},
    )
    fig.update_layout(yaxis_tickformat=".0%", margin=dict(l=0, r=10, t=10, b=10), height=360)
    return fig


def build_taxonomy_sunburst(d: pd.DataFrame) -> go.Figure:
    levels = [c for c in [COLS.ORDER, COLS.FAMILY, COLS.GENUS, COLS.SPECIES] if c in d.columns]
    if len(levels) < 2:
        fig = go.Figure()
        fig.update_layout(title="Need at least two taxonomy levels.", height=120, margin=dict(l=0, r=0, t=30, b=0))
        return fig

    def completed_mask(s: pd.Series) -> pd.Series:
        return s.eq("insdc_open")

    def inprog_mask(s: pd.Series) -> pd.Series:
        return s.isin(["sample_acquired", "data_generation", "in_assembly"])

    parts = []
    for depth in range(1, len(levels) + 1):
        group_cols = levels[:depth]
        grp = (
            d.groupby(group_cols, dropna=False)[COLS.SEQUENCING_STATUS]
            .agg(
                total="size",
                completed=lambda s: int(completed_mask(s).sum()),
                in_progress=lambda s: int(inprog_mask(s).sum()),
            )
            .reset_index()
        )
        grp["level"] = depth
        parts.append(grp)

    nodes = pd.concat(parts, ignore_index=True)
    if nodes.empty:
        fig = go.Figure()
        fig.update_layout(title="No taxonomy nodes.", height=120, margin=dict(l=0, r=0, t=30, b=0))
        return fig

    def make_id(row) -> str:
        return " / ".join([str(row.get(col, "Unknown")) for col in levels[: int(row["level"])]])

    def make_parent(row) -> str:
        if int(row["level"]) == 1:
            return ""
        return " / ".join([str(row.get(col, "Unknown")) for col in levels[: int(row["level"]) - 1]])

    nodes = nodes.assign(
        id=nodes.apply(make_id, axis=1).astype(str).to_numpy(),
        parent=nodes.apply(make_parent, axis=1).astype(str).to_numpy(),
    )

    nodes["coverage_pct"] = (nodes["completed"] / nodes["total"].replace(0, np.nan) * 100.0).fillna(0).round(2)
    nodes["label"] = nodes.apply(lambda r: str(r[levels[int(r["level"]) - 1]]), axis=1)
    nodes["value"] = nodes["total"]
    nodes["hover"] = (
        nodes["id"].astype(str)
        + "<br><b>Total</b>: " + nodes["total"].astype(int).astype(str)
        + "<br><b>Completed</b>: " + nodes["completed"].astype(int).astype(str)
        + "<br><b>In progress</b>: " + nodes["in_progress"].astype(int).astype(str)
        + "<br><b>Completion</b>: " + nodes["coverage_pct"].astype(str) + "%"
    )

    fig = go.Figure(
        go.Sunburst(
            ids=nodes["id"],
            labels=nodes["label"],
            parents=nodes["parent"],
            values=nodes["value"],
            branchvalues="total",
            hovertext=nodes["hover"],
            hoverinfo="text",
            maxdepth=None,
            insidetextorientation="radial",
            marker=dict(line=dict(width=0.5)),
        )
    )
    fig.update_traces(
        marker=dict(colors=nodes["coverage_pct"], colorbar=dict(title="Completion %")),
        selector=dict(type="sunburst"),
    )
    fig.update_layout(margin=dict(l=0, r=0, t=10, b=10), height=520)
    return fig


# ============================
# HTML helpers
# ============================

def fig_to_html(fig) -> str:
    """Return a self-contained HTML snippet (uses Plotly CDN)."""
    return pio.to_html(fig, include_plotlyjs="cdn", full_html=False)


def render_all_html(csv_path: str) -> Dict[str, str]:
    """
    Read CSV, build all four figs, and return HTML snippets.
    Keys: plot1 (funnel), plot2 (family activity), plot3 (habitat share), plot4 (sunburst)
    """
    df = add_features(load_csv(csv_path))
    fam_all, fam_active = family_progress_table(df)

    figs = {
        "plot1": plot_progress_funnel(df),
        "plot2": plot_family_activity(fam_active),
        "plot3": plot_habitat_progress_share(df),
        "plot4": build_taxonomy_sunburst(df),
    }
    return {k: fig_to_html(v) for k, v in figs.items()}
