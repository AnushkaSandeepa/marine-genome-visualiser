import streamlit as st
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go

st.title("Species Visualization Dashboard 🌿")

# --- Dataset selection ---
dataset_choice = st.selectbox(
    "Choose dataset",
    ["Basic Species Dataset", "Species with Sequencing Status"]
)

if dataset_choice == "Basic Species Dataset":
    df = pd.read_csv("new_final_species_cleaned.csv")
else:
    df = pd.read_csv("new_final_species_cleaned_with_seq.csv")

# --- 1️⃣ Grouped Bar Chart (only if sequencing_status exists) ---
if "sequencing_status" in df.columns:
    st.header("Species Count per Order grouped by Sequencing Status")
    order_seq_counts = (
        df.groupby(['order', 'sequencing_status'])['species_canonical']
          .nunique()
          .reset_index()
    )

    fig_bar = px.bar(
        order_seq_counts,
        x='order',
        y='species_canonical',
        color='sequencing_status',
        barmode='group',
        labels={'species_canonical': 'Number of Species', 'order': 'Order'},
        title='Species Count per Order grouped by Sequencing Status'
    )
    fig_bar.update_layout(bargap=0.1)
    st.plotly_chart(fig_bar, use_container_width=True)
else:
    st.info("Grouped Bar Chart requires the dataset with sequencing_status.")

# --- 2️⃣ Interactive Sankey Diagram ---
st.header("Species Flow: Order → Family → Genus → Species")

# Dropdown selectors
order_options = ['Select Order'] + sorted(df['order'].dropna().unique())
selected_order = st.selectbox("Select Order", order_options)

family_options = ['Select Family']
if selected_order != 'Select Order':
    family_options += sorted(df[df['order'] == selected_order]['family_x'].dropna().unique())
selected_family = st.selectbox("Select Family", family_options)

genus_options = ['Select Genus']
if selected_family != 'Select Family':
    genus_options += sorted(df[df['family_x'] == selected_family]['genus'].dropna().unique())
selected_genus = st.selectbox("Select Genus", genus_options)

# Function to create Sankey diagram
def create_sankey(order, family, genus):
    if order in ['Select Order', None] or family in ['Select Family', None] or genus in ['Select Genus', None]:
        fig = go.Figure()
        fig.update_layout(title_text="Please select Order, Family, and Genus", font_size=12)
        return fig

    filtered = df[(df['order'] == order) &
                  (df['family_x'] == family) &
                  (df['genus'] == genus)]

    species_list = filtered['species_canonical'].unique()
    if len(species_list) == 0:
        fig = go.Figure()
        fig.update_layout(title_text="No species found for selection", font_size=12)
        return fig

    # Nodes
    nodes = [order, family, genus] + list(species_list)
    node_indices = {name: i for i, name in enumerate(nodes)}

    # Links
    sources = [node_indices[order], node_indices[family]] + [node_indices[genus]]*len(species_list)
    targets = [node_indices[family], node_indices[genus]] + [node_indices[s] for s in species_list]
    values = [len(filtered), len(filtered)] + [1]*len(species_list)

    fig = go.Figure(go.Sankey(
        node=dict(
            pad=20,
            thickness=25,
            line=dict(color="black", width=0.7),
            label=nodes,
            color="rgba(58, 71, 80, 0.8)"
        ),
        link=dict(
            source=sources,
            target=targets,
            value=values,
            color="rgba(63, 81, 181, 0.4)"
        )
    ))

    fig.update_layout(
        title_text=f"Species Flowchart for {order} → {family} → {genus}",
        font=dict(size=14, color="black"),
        plot_bgcolor="white",
        paper_bgcolor="white"
    )
    return fig

fig_sankey = create_sankey(selected_order, selected_family, selected_genus)
st.plotly_chart(fig_sankey, use_container_width=True)
