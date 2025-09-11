# app.R
# install.packages(c(
#   "shiny","leaflet","leaflet.extras","robis","dplyr","purrr",
#   "lubridate","DT","scales","memoise","tibble","stringr","htmltools","sf"
# ))

library(shiny)
library(leaflet)
library(leaflet.extras)
library(robis)
library(dplyr)
library(purrr)
library(lubridate)
library(DT)
library(scales)
library(memoise)
library(tibble)
library(stringr)
library(htmltools)
library(sf)

POLITE_DELAY <- 0.35

# ---- Client demo species list (for name-based search) ----
client_species <- tibble::tribble(
  ~common_name, ~species,
  "Blackspot snapper", "Lutjanus fulviflamma",
  "Stripey snapper", "Lutjanus carponotatus",
  "Brownstripe snapper", "Lutjanus vitta",
  "Moses snapper", "Lutjanus russellii",
  "Grass emperor", "Lethrinus laticaudis",
  "Fivelined snapper", "Lutjanus quinquelineatus",
  "Chinamanfish", "Symphorus nematophorus",
  "Red emperor", "Lutjanus sebae",
  "Darktail snapper", "Lutjanus lemniscatus",
  "Spangled emperor", "Lethrinus nebulosus",
  "Sharptooth snapper", "Pristipomoides typus",
  "Crimson snapper", "Lutjanus erythropterus",
  "Goldflag jobfish", "Pristipomoides auricilla",
  "Ruby snapper", "Etelis carbunculus",
  "Flame snapper", "Etelis coruscans",
  "Oblique-banded snapper", "Pristipomoides zonatus",
  "Smalltooth jobfish", "Aphareus furca",
  "Blue fusilier", "Caesio teres",
  "Rusty jobfish", "Aphareus rutilans",
  "Blacktail snapper", "Lutjanus fulvus",
  "Red bass", "Lutjanus bohar",
  "Green jobfish", "Aprion virescens",
  "Midnight snapper", "Macolor macularis",
  "Mangrove jack", "Lutjanus argentimaculatus"
) |>
  mutate(across(everything(), str_squish))

# ---- Helpers ----
bbox_wkt <- function(w, s, e, n) {
  sprintf("POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
          w, s, e, s, e, n, w, n, w, s)
}

safe_event_date <- function(x) {
  d1 <- suppressWarnings(ymd_hms(x, quiet = TRUE, tz = "UTC"))
  d2 <- suppressWarnings(ymd(x, quiet = TRUE))
  d3 <- suppressWarnings(ymd(substr(x, 1, 10), quiet = TRUE))
  out <- d1; out[is.na(out)] <- d2[is.na(out)]; out[is.na(out)] <- d3[is.na(out)]
  as.Date(out)
}

# Parse "12345, 67890" -> integer vector
parse_ids <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return(integer(0))
  ids <- suppressWarnings(as.integer(strsplit(gsub("\\s+","", txt), ",")[[1]]))
  ids[is.finite(ids)]
}

# Ensure required columns exist; coerce types; drop bad coords
normalize_obis <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(tibble())

  need_chr <- c("scientificName", "datasetID", "basisOfRecord", "eventDate")
  need_num <- c("decimalLongitude", "decimalLatitude", "depth", "individualCount")
  for (nm in c(need_chr, need_num)) if (!nm %in% names(df)) df[[nm]] <- NA

  df |>
    transmute(
      scientificName   = as.character(scientificName),
      decimalLongitude = suppressWarnings(as.numeric(decimalLongitude)),
      decimalLatitude  = suppressWarnings(as.numeric(decimalLatitude)),
      eventDate        = safe_event_date(eventDate),
      depth            = suppressWarnings(as.numeric(depth)),
      individualCount  = suppressWarnings(as.numeric(individualCount)),
      datasetID        = as.character(datasetID),
      basisOfRecord    = as.character(basisOfRecord)
    ) |>
    filter(
      !is.na(decimalLongitude), !is.na(decimalLatitude),
      between(decimalLongitude, -180, 180),
      between(decimalLatitude,  -90,   90)
    )
}

# ---- OBIS fetchers (memoised) ----
.fetch_obis_name <- function(species, wkt = NULL, start = NULL, end = NULL) {
  Sys.sleep(POLITE_DELAY)
  occurrence(
    scientificname = species,
    geometry = wkt,
    startdate = if (!is.null(start)) as.Date(start) else NULL,
    enddate   = if (!is.null(end))   as.Date(end)   else NULL
    # , fields = c("scientificName","decimalLongitude","decimalLatitude",
    #              "eventDate","depth","individualCount","datasetID","basisOfRecord")
  )
}
.fetch_obis_aphia <- function(aphia, wkt = NULL, start = NULL, end = NULL) {
  Sys.sleep(POLITE_DELAY)
  occurrence(
    aphiaid = aphia,
    geometry = wkt,
    startdate = if (!is.null(start)) as.Date(start) else NULL,
    enddate   = if (!is.null(end))   as.Date(end)   else NULL
    # , fields = c("scientificName","decimalLongitude","decimalLatitude",
    #              "eventDate","depth","individualCount","datasetID","basisOfRecord")
  )
}
fetch_obis_name  <- memoise(.fetch_obis_name)
fetch_obis_aphia <- memoise(.fetch_obis_aphia)

# Palette helper (robust to duplicate breaks)
make_bin_pal <- function(vals,
                         mode = c("equal","quantile","custom"),
                         cuts = c(0,5,10,20),
                         n = 7) {
  mode <- match.arg(mode)
  vals_ok <- vals[is.finite(vals)]
  if (length(vals_ok) == 0 || length(unique(vals_ok)) <= 1) {
    return(colorNumeric("viridis", domain = vals_ok, na.color = "#F0F0F0"))
  }
  if (mode == "quantile") {
    q <- quantile(vals_ok, probs = seq(0, 1, length.out = n), na.rm = TRUE)
    brks <- unique(as.numeric(q))
  } else if (mode == "custom") {
    brks <- sort(unique(cuts))
  } else {
    rng <- range(vals_ok, na.rm = TRUE)
    brks <- unique(pretty(rng, n = n))
  }
  brks <- sort(unique(c(-Inf, brks, Inf)))
  if (length(brks) < 3) {
    rng <- range(vals_ok, na.rm = TRUE)
    brks <- sort(unique(c(-Inf, pretty(rng, n = 4), Inf)))
  }
  colorBin("viridis", domain = vals_ok, bins = brks, na.color = "#F0F0F0")
}

parse_cuts <- function(txt) as.numeric(strsplit(gsub("\\s+", "", txt), ",")[[1]])

# ---- UI ----
ui <- fluidPage(
  titlePanel("OBIS Coverage – Name/AphiaID + Per-species metrics"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("search_mode", "Search by",
                   choices = c("Species name(s)" = "name", "AphiaID(s)" = "aphia"),
                   selected = "name", inline = TRUE),
      conditionalPanel(
        "input.search_mode == 'name'",
        selectizeInput(
          "sp", "Species",
          choices = setNames(client_species$species,
                             paste(client_species$common_name, "—", client_species$species)),
          selected = client_species$species[1:3], multiple = TRUE
        )
      ),
      conditionalPanel(
        "input.search_mode == 'aphia'",
        textInput("aphia_txt", "AphiaID(s) (comma-separated)", "212381, 212127")
      ),

      checkboxInput("aus_only", "Limit to Australia region", value = TRUE),
      conditionalPanel(
        "input.aus_only == false",
        sliderInput("lng_range", "Longitude (°)", min = -180, max = 180, value = c(110, 160), step = 1),
        sliderInput("lat_range", "Latitude (°)", min = -90,  max = 90,  value = c(-45, -10), step = 1)
      ),
      dateRangeInput("date_range", "Date range",
                     start = "2000-01-01", end = Sys.Date(),
                     min = "1900-01-01", max = Sys.Date()),
      hr(),

      sliderInput("cell_km", "Cell size (km)", min = 10, max = 250, value = 50, step = 10),
      selectInput("metric", "Summary metric", choices = c(
        "Occurrence count" = "n_occ",
        "Species richness" = "spp_rich",
        "Mean individual count (non-missing)" = "mean_abund",
        "Sum individual count (non-missing)"  = "sum_abund"
      ), selected = "n_occ"),

      # Scope for count/abundance metrics
      conditionalPanel(
        "input.metric != 'spp_rich'",
        radioButtons("metric_scope", "Scope for count/abundance",
                     choices = c("All selected taxa" = "all",
                                 "Single taxon" = "one"),
                     selected = "all", inline = TRUE)
      ),
      conditionalPanel(
        "input.metric != 'spp_rich' && input.metric_scope == 'one'",
        selectInput("metric_taxon", "Taxon for metric", choices = character(0))
      ),
      conditionalPanel(
        "input.metric == 'spp_rich' && input.search_mode == 'name' && input.sp.length <= 1",
        tags$small(style="color:#666;",
                   "Richness = 1 when only one species is selected. Select multiple species to see variation.")
      ),
      hr(),

      radioButtons("bin_mode", "Legend binning",
                   choices = c("Equal width" = "equal",
                               "Quantiles" = "quantile",
                               "Custom cuts" = "custom"),
                   selected = "quantile"),
      conditionalPanel("input.bin_mode == 'custom'",
        textInput("custom_cuts", "Custom cuts (comma-separated)", "0,5,10,20")
      ),
      conditionalPanel("input.bin_mode != 'custom'",
        sliderInput("n_bins", "Number of bins", min = 3, max = 9, value = 7, step = 1)
      ),
      checkboxInput("show_centroids", "Show cell centroids (labels)", value = FALSE),
      actionButton("refresh", "Fetch & Summarise", class = "btn-primary"),
      br(), br(),
      downloadButton("dl_csv", "Download summary CSV")
    ),
    mainPanel(
      leafletOutput("map", height = 620),
      br(),
      DTOutput("tbl"),
      br(),
      tags$small(HTML("<b>Note:</b> <code>individualCount</code> = number of individuals reported in a record.
      Missing (NA) means 'not reported' and is not treated as zero; mean/sum use only non-missing counts."))
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  geom_wkt <- reactive({
    if (isTRUE(input$aus_only)) bbox_wkt(110, -45, 160, -10) else {
      bbox_wkt(input$lng_range[1], input$lat_range[1],
               input$lng_range[2], input$lat_range[2])
    }
  })

  # Fetch OBIS occurrences (by name OR by AphiaID) and NORMALIZE columns
  obis_df <- eventReactive(input$refresh, {
    req(input$date_range)
    withProgress(message = "Querying OBIS …", value = 0, {
      if (input$search_mode == "name") {
        sp_vec <- input$sp
        if (length(sp_vec) == 0) return(tibble())
        n <- length(sp_vec)
        dfs <- map(seq_along(sp_vec), function(i) {
          incProgress(i / n, detail = sp_vec[i])
          df <- tryCatch({
            fetch_obis_name(
              species = sp_vec[i], wkt = geom_wkt(),
              start = input$date_range[1], end = input$date_range[2]
            )
          }, error = function(e) NULL)
          normalize_obis(df)
        })
        bind_rows(dfs)

      } else {  # search_mode == "aphia"
        ids <- parse_ids(input$aphia_txt)
        if (length(ids) == 0) return(tibble())
        n <- length(ids)
        dfs <- map(seq_along(ids), function(i) {
          incProgress(i / n, detail = paste0("AphiaID ", ids[i]))
          df <- tryCatch({
            fetch_obis_aphia(
              aphia = ids[i], wkt = geom_wkt(),
              start = input$date_range[1], end = input$date_range[2]
            )
          }, error = function(e) NULL)
          normalize_obis(df)
        })
        bind_rows(dfs)
      }
    })
  }, ignoreInit = TRUE)

  # Update per-taxon selector after fetching
  observeEvent(obis_df(), {
    df <- obis_df()
    taxa <- sort(unique(df$scientificName[!is.na(df$scientificName)]))
    updateSelectInput(session, "metric_taxon", choices = taxa,
                      selected = if (length(taxa)) taxa[1] else character(0))
  })

  # Build grid and aggregate; return both ALL and BY-TAXON summaries
  grid_data <- reactive({
    df <- obis_df()
    if (nrow(df) == 0) return(NULL)

    pts <- st_as_sf(df, coords = c("decimalLongitude","decimalLatitude"),
                    crs = 4326, remove = FALSE) |>
           st_transform(3857)

    cell_m <- input$cell_km * 1000
    bbox <- st_bbox(pts)
    grd <- st_make_grid(st_as_sfc(bbox), cellsize = cell_m, square = TRUE)
    if (length(grd) == 0) return(NULL)
    grid_sf <- st_sf(cell_id = seq_along(grd), geometry = grd)

    joined <- st_join(pts, grid_sf, left = FALSE)
    if (nrow(joined) == 0) return(NULL)

    # All-selected-taxa summary per cell
    summ_all <- joined |>
      st_drop_geometry() |>
      group_by(cell_id) |>
      summarise(
        n_occ = n(),
        spp_rich = n_distinct(na.omit(scientificName)),
        n_with_abund = sum(!is.na(individualCount)),
        mean_abund = ifelse(n_with_abund > 0, mean(individualCount, na.rm = TRUE), NA_real_),
        sum_abund  = ifelse(n_with_abund > 0, sum(individualCount,  na.rm = TRUE), NA_real_),
        .groups = "drop"
      )

    # Per-taxon summary per cell
    summ_taxon <- joined |>
      st_drop_geometry() |>
      filter(!is.na(scientificName)) |>
      group_by(cell_id, taxon = scientificName) |>
      summarise(
        n_occ = n(),
        n_with_abund = sum(!is.na(individualCount)),
        mean_abund = ifelse(n_with_abund > 0, mean(individualCount, na.rm = TRUE), NA_real_),
        sum_abund  = ifelse(n_with_abund > 0, sum(individualCount,  na.rm = TRUE), NA_real_),
        .groups = "drop"
      )

    list(
      grid = st_transform(grid_sf, 4326),
      all  = summ_all,
      by_taxon = summ_taxon
    )
  })

  # Base map
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = 120, lat = -20, zoom = 3)
  })

  # Draw choropleth (with single-value legend guard)
  observeEvent(list(grid_data(), input$metric, input$metric_scope,
                    input$metric_taxon, input$bin_mode, input$n_bins,
                    input$custom_cuts, input$show_centroids), {

    gd <- grid_data()
    proxy <- leafletProxy("map")
    proxy |> clearShapes() |> clearControls() |> clearMarkers()
    if (is.null(gd)) return()

    # selection box
    if (isTRUE(input$aus_only)) {
      proxy |> addRectangles(lng1 = 110, lat1 = -45, lng2 = 160, lat2 = -10,
                             fill = FALSE, weight = 1, color = "#888888")
    } else {
      proxy |> addRectangles(lng1 = input$lng_range[1], lat1 = input$lat_range[1],
                             lng2 = input$lng_range[2], lat2 = input$lat_range[2],
                             fill = FALSE, weight = 1, color = "#888888")
    }

    # Compose the data for the chosen metric/scope
    if (input$metric == "spp_rich") {
      dat <- gd$grid |>
        inner_join(gd$all |> transmute(cell_id, value = spp_rich), by = "cell_id")
      leg_title <- "Species richness"
    } else {
      metric_field <- switch(input$metric,
        n_occ = "n_occ", mean_abund = "mean_abund", sum_abund = "sum_abund"
      )
      if (input$metric_scope == "all") {
        dat <- gd$grid |>
          inner_join(gd$all |> transmute(cell_id, value = .data[[metric_field]]), by = "cell_id")
        leg_title <- switch(metric_field,
          n_occ = "Occurrence count (all taxa)",
          mean_abund = "Mean individual count (all taxa)",
          sum_abund = "Sum individual count (all taxa)"
        )
      } else {
        req(input$metric_taxon)
        tax_df <- gd$by_taxon |>
          filter(taxon == input$metric_taxon) |>
          transmute(cell_id, value = .data[[metric_field]])
        if (nrow(tax_df) == 0) return()
        dat <- gd$grid |> inner_join(tax_df, by = "cell_id")
        leg_title <- switch(metric_field,
          n_occ = paste0("Occurrence count (", input$metric_taxon, ")"),
          mean_abund = paste0("Mean individual count (", input$metric_taxon, ")"),
          sum_abund = paste0("Sum individual count (", input$metric_taxon, ")")
        )
      }
    }

    if (nrow(dat) == 0) return()

    vals <- dat$value
    vals_ok <- vals[is.finite(vals)]
    if (length(vals_ok) == 0) return()

    cent <- st_centroid(dat$geometry)
    cent_xy <- st_coordinates(cent)

    popup <- paste0(
      "<b>Cell ID:</b> ", dat$cell_id, "<br/>",
      "<b>Centroid:</b> ", round(cent_xy[,1], 3), ", ", round(cent_xy[,2], 3), "<br/>",
      "<b>", leg_title, ":</b> ", ifelse(is.na(vals), "NA", round(vals, 2))
    )

    # special case: all values identical
    if (length(unique(vals_ok)) == 1) {
      single <- unique(vals_ok)
      pal_single <- colorFactor("viridis", domain = single)
      proxy |>
        addPolygons(
          data = dat,
          weight = 0.3, color = "#666666",
          fillOpacity = 0.8,
          fillColor = pal_single(vals),
          popup = popup,
          label = paste0(leg_title, ": ", single)
        ) |>
        addLegend("bottomright",
                  colors = pal_single(single),
                  labels = as.character(single),
                  title  = leg_title, opacity = 1)
      if (isTRUE(input$show_centroids)) {
        proxy |> addCircleMarkers(lng = cent_xy[,1], lat = cent_xy[,2],
                                  radius = 2, stroke = FALSE, fillOpacity = 0.9,
                                  color = "#000000", label = paste0("Cell ", dat$cell_id))
      }
      return(invisible(NULL))
    }

    # general case: binned palette
    pal <- if (input$bin_mode == "custom") {
      cuts <- parse_cuts(input$custom_cuts)
      if (all(is.na(cuts)) || length(cuts) == 0) cuts <- c(0,5,10,20)
      make_bin_pal(vals, mode = "custom", cuts = cuts)
    } else if (input$bin_mode == "quantile") {
      make_bin_pal(vals, mode = "quantile", n = input$n_bins)
    } else {
      make_bin_pal(vals, mode = "equal", n = input$n_bins)
    }

    proxy |>
      addPolygons(
        data = dat,
        weight = 0.3, color = "#666666",
        fillOpacity = ifelse(is.na(vals) | vals == 0, 0, 0.8),
        fillColor   = ifelse(is.na(vals) | vals == 0, "transparent", pal(vals)),
        popup = popup,
        label = paste0(leg_title, ": ", ifelse(is.na(vals), "NA", round(vals, 2)))
      ) |>
      addLegend("bottomright", pal = pal, values = vals,
                title = leg_title, opacity = 1)

    if (isTRUE(input$show_centroids)) {
      proxy |> addCircleMarkers(lng = cent_xy[,1], lat = cent_xy[,2],
                                radius = 2, stroke = FALSE, fillOpacity = 0.9,
                                color = "#000000", label = paste0("Cell ", dat$cell_id))
    }
  })

  # Summary table (ALL-taxa metrics for a quick overview)
  output$tbl <- renderDT({
    gd <- grid_data()
    if (is.null(gd) || nrow(gd$all) == 0)
      return(datatable(tibble(msg = "No records found for selection."), rownames = FALSE))
    dat <- gd$grid |> inner_join(gd$all, by = "cell_id")
    cent <- st_centroid(dat$geometry)
    coords <- st_coordinates(cent)
    out <- dat |> st_drop_geometry() |>
      mutate(centroid_lon = round(coords[,1], 5),
             centroid_lat = round(coords[,2], 5)) |>
      select(cell_id, centroid_lon, centroid_lat,
             n_occ, spp_rich, n_with_abund, mean_abund, sum_abund)
    datatable(out, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  # Download summary (ALL-taxa grid)
  output$dl_csv <- downloadHandler(
    filename = function() {
      if (input$search_mode == "name") {
        paste0("obis_grid_summary_", gsub("[^A-Za-z0-9]+","_", paste(input$sp, collapse="_")), ".csv")
      } else {
        paste0("obis_grid_summary_aphia_", gsub("\\s+","", gsub(",","_", input$aphia_txt)), ".csv")
      }
    },
    content  = function(file) {
      gd <- grid_data()
      if (is.null(gd) || nrow(gd$all) == 0) { readr::write_csv(tibble(), file); return() }
      dat <- gd$grid |> inner_join(gd$all, by = "cell_id")
      cent <- st_centroid(dat$geometry)
      coords <- st_coordinates(cent)
      out <- dat |> st_drop_geometry() |>
        mutate(centroid_lon = coords[,1], centroid_lat = coords[,2])
      readr::write_csv(out, file)
    }
  )
}

shinyApp(ui, server)