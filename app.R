# app.R

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
library(readr)   # for write_csv() in download handler

POLITE_DELAY <- 0.35

# ---- Client species list ----
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

# Robust date parser for OBIS eventDate
safe_event_date <- function(x) {
  d1 <- suppressWarnings(ymd_hms(x, quiet = TRUE, tz = "UTC"))
  d2 <- suppressWarnings(ymd(x, quiet = TRUE))
  d3 <- suppressWarnings(ymd(substr(x, 1, 10), quiet = TRUE))
  out <- d1
  out[is.na(out)] <- d2[is.na(out)]
  out[is.na(out)] <- d3[is.na(out)]
  as.Date(out)
}

# Memoised OBIS fetcher to avoid repeated network calls
.fetch_obis <- function(species, wkt = NULL, start = NULL, end = NULL) {
  Sys.sleep(POLITE_DELAY)
  base <- "https://api.obis.org/v3/occurrence"
  qs <- list(
    scientificname = species,
    geometry = wkt,
    startdate = as.character(as.Date(start)),
    enddate   = as.character(as.Date(end)),
    size      = 10000           # increase if you really need more; consider paging
  )
  enc <- function(x) utils::URLencode(as.character(x), reserved = TRUE)
  qs <- qs[!vapply(qs, is.null, logical(1))]
  url <- paste0(base, "?", paste0(names(qs), "=", vapply(qs, enc, ""), collapse = "&"))
  j <- jsonlite::fromJSON(url)
  tibble::as_tibble(j$results)
}


# ---- UI ----
ui <- fluidPage(
  titlePanel("Demo Species – Interactive Distribution (OBIS)"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "sp", "Species",
        choices = setNames(client_species$species,
                           paste(client_species$common_name, "—", client_species$species)),
        selected = client_species$species[1]
      ),
      checkboxInput("aus_only", "Limit to Australia region", value = TRUE),
      conditionalPanel(
        "input.aus_only == false",
        sliderInput("lng_range", "Longitude (°)", min = -180, max = 180, value = c(110, 160), step = 1),
        sliderInput("lat_range", "Latitude (°)", min = -90,  max = 90,  value = c(-45, -10), step = 1)
      ),
      dateRangeInput(
        "date_range", "Date range",
        start = "2000-01-01", end = Sys.Date(),
        min = "1900-01-01", max = Sys.Date()
      ),
      checkboxInput("show_heat", "Show heatmap (density)", value = FALSE),
      checkboxInput("cluster", "Cluster points", value = TRUE),
      sliderInput("pt_size", "Point size", min = 3, max = 12, value = 5),
      actionButton("refresh", "Fetch / Refresh", class = "btn-primary"),
      br(), br(),
      downloadButton("dl_csv", "Download filtered CSV")
    ),
    mainPanel(
      leafletOutput("map", height = 620),
      br(),
      DTOutput("tbl")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  geom_wkt <- reactive({
    if (isTRUE(input$aus_only)) {
      # Australia-ish bounding box
      bbox_wkt(110, -45, 160, -10)
    } else {
      w <- input$lng_range[1]; e <- input$lng_range[2]
      s <- input$lat_range[1]; n <- input$lat_range[2]
      bbox_wkt(w, s, e, n)
    }
  })
  
  obis_df <- eventReactive(input$refresh, {
    req(input$sp, input$date_range)
    withProgress(message = "Querying OBIS …", value = 0, {
      incProgress(0.2)
      df <- tryCatch({
        fetch_obis(
          species = input$sp,
          wkt = geom_wkt(),
          start = input$date_range[1],
          end   = input$date_range[2]
        )
      }, error = function(e) NULL)
      incProgress(0.6)
      if (is.null(df) || nrow(df) == 0) return(tibble())
      
      # Keep useful columns; clean types; guard coords; parse dates safely
      df |>
        transmute(
          scientificName,
          decimalLongitude = suppressWarnings(as.numeric(decimalLongitude)),
          decimalLatitude  = suppressWarnings(as.numeric(decimalLatitude)),
          eventDate        = safe_event_date(eventDate),
          depth            = suppressWarnings(as.numeric(depth)),
          datasetID        = datasetID,
          basisOfRecord    = basisOfRecord
        ) |>
        filter(
          !is.na(decimalLongitude), !is.na(decimalLatitude),
          between(decimalLongitude, -180, 180),
          between(decimalLatitude,  -90,   90)
        )
    })
  }, ignoreInit = TRUE)
  
  # Depth palette (handles all-NA or zero range)
  pal <- reactive({
    df <- obis_df()
    rng <- range(df$depth, na.rm = TRUE)
    if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) rng <- c(0, 200)
    colorNumeric("viridis", domain = rng, na.color = "#999999")
  })
  
  # Base map
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = 120, lat = -20, zoom = 3)
  })
  
  # Update map when data arrives
  observeEvent(obis_df(), {
    df <- obis_df()
    proxy <- leafletProxy("map")
    proxy |> clearMarkers() |> clearShapes() |> clearControls() |> clearHeatmap()
    
    # Show bbox rectangle
    if (isTRUE(input$aus_only)) {
      proxy |> addRectangles(lng1 = 110, lat1 = -45, lng2 = 160, lat2 = -10,
                             fill = FALSE, weight = 1, color = "#888888")
    } else {
      lng1 <- input$lng_range[1]; lng2 <- input$lng_range[2]
      lat1 <- input$lat_range[1]; lat2 <- input$lat_range[2]
      proxy |> addRectangles(lng1 = lng1, lat1 = lat1, lng2 = lng2, lat2 = lat2,
                             fill = FALSE, weight = 1, color = "#888888")
    }
    
    if (nrow(df) == 0) return()
    
    pal_fun <- pal()
    fmt_depth <- function(x) ifelse(is.na(x), "NA", paste0(round(x, 1), " m"))
    fmt_date  <- function(x) ifelse(is.na(x), "NA", format(x, "%Y-%m-%d"))
    
    popup <- paste0(
      "<b>", htmlEscape(df$scientificName), "</b><br/>",
      "Date: ", fmt_date(df$eventDate), "<br/>",
      "Depth: ", fmt_depth(df$depth), "<br/>",
      "Lon/Lat: ", round(df$decimalLongitude, 4), ", ", round(df$decimalLatitude, 4), "<br/>",
      "Basis: ", htmlEscape(df$basisOfRecord), "<br/>",
      "DatasetID: ", htmlEscape(df$datasetID)
    )
    
    if (isTRUE(input$show_heat)) {
      proxy |>
        addHeatmap(lng = df$decimalLongitude, lat = df$decimalLatitude,
                   blur = 20, max = 0.05, radius = 12)
    }
    
    proxy |>
      addCircleMarkers(
        lng = df$decimalLongitude, lat = df$decimalLatitude,
        radius = input$pt_size,
        stroke = FALSE, fillOpacity = 0.7,
        color = pal_fun(df$depth),
        label = paste0("Depth: ", fmt_depth(df$depth)),
        popup = popup,
        clusterOptions = if (isTRUE(input$cluster)) markerClusterOptions() else NULL
      ) |>
      addLegend(position = "bottomright", pal = pal_fun,
                values = df$depth, title = "Depth (m)", opacity = 1)
  })
  
  # Table
  output$tbl <- renderDT({
    df <- obis_df()
    if (nrow(df) == 0) {
      return(datatable(tibble(msg = "No records found for selection."), rownames = FALSE))
    }
    datatable(
      df |>
        mutate(depth_m = depth) |>
        select(scientificName, eventDate, depth_m,
               decimalLongitude, decimalLatitude, basisOfRecord, datasetID),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # Download
  output$dl_csv <- downloadHandler(
    filename = function() paste0(gsub(" ", "_", input$sp), "_obis_records.csv"),
    content  = function(file) readr::write_csv(obis_df(), file)
  )
}

shinyApp(ui, server)
