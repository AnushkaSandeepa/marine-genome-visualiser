# ===== File: app.R =====

library(shiny)
library(bslib)
library(DT)

# -- Source shared helpers and page modules --
source("R/commons.R", local = TRUE)
source("R/pages_map_search.R", local = TRUE)
source("R/pages_id_search.R", local = TRUE)
source("R/pages_visualizations.R", local = TRUE)

# ---- UI ----
ui <- page_navbar(
  title = "Ocean Genome Explorer",
  theme = bs_theme(bootswatch = "flatly"),
  window_title = "OG Explorer",
  nav_panel("Map Search", icon = "globe", MapSearchUI("map")),
  nav_panel("ID Search", icon = "search", IDSearchUI("idsearch")),
  nav_panel("Visualizations", icon = "bar-chart", VizUI("viz")),
  nav_spacer(),
  nav_item(a(href = "https://www.uwa.edu.au/", target = "_blank", "UWA"))
)

# ---- Server ----
server <- function(input, output, session) {
  MapSearchServer("map")
  IDSearchServer("idsearch")
  VizServer("viz")
}

shinyApp(ui, server)