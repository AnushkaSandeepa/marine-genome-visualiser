# ===== File: R/pages_map_search.R =====
# Map Search (module) — OBIS grid summary with Species search and metrics
# Relies on helpers defined in R/commons.R:
#  bbox_wkt(), safe_event_date(), normalize_obis(), fetch_obis(),
#  make_bin_pal(), parse_cuts(), year_range_from_occ(), client_species

MapSearchUI <- function(id) {
  ns <- NS(id)
  shiny::fluidPage(
    shiny::titlePanel("Ocean Genome Explorer"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectizeInput(
          ns("sp"), "Species",
          choices = client_species$species,
          selected = client_species$species[1:3],
          multiple = TRUE
        ),
        shiny::checkboxInput(ns("aus_only"), "Limit to Australia region", value = TRUE),
        shiny::conditionalPanel(
          sprintf("input['%s'] == false", ns("aus_only")),
          shiny::sliderInput(ns("lng_range"), "Longitude (°)",
                             min = -180, max = 180, value = c(110, 160), step = 1),
          shiny::sliderInput(ns("lat_range"), "Latitude (°)",
                             min = -90, max = 90, value = c(-45, -10), step = 1)
        ),
        shiny::dateRangeInput(
          ns("date_range"), "Date range",
          start = "2000-01-01", end = Sys.Date(),
          min = "1900-01-01", max = Sys.Date()
        ),
        shiny::hr(),
        shiny::sliderInput(ns("cell_km"), "Cell size (km)", min = 10, max = 250, value = 50, step = 10),
        shiny::selectInput(ns("metric"), "Summary metric",
                           choices = c(
                             "Occurrence count" = "n_occ",
                             "Species richness" = "spp_rich",
                             "Mean individual count (non-missing)" = "mean_abund",
                             "Sum individual count (non-missing)"  = "sum_abund"
                           ),
                           selected = "n_occ"),
        shiny::conditionalPanel(
          sprintf("input['%s'] != 'spp_rich'", ns("metric")),
          shiny::radioButtons(
            ns("metric_scope"), "Scope for count/abundance",
            choices = c("All selected taxa" = "all", "Single taxon" = "one"),
            selected = "all", inline = TRUE
          )
        ),
        shiny::conditionalPanel(
          sprintf("input['%s'] != 'spp_rich' && input['%s'] == 'one'", ns("metric"), ns("metric_scope")),
          shiny::selectInput(ns("metric_taxon"), "Taxon for metric", choices = character(0))
        ),
        shiny::hr(),
        shiny::radioButtons(
          ns("bin_mode"), "Legend binning",
          choices = c("Equal width" = "equal", "Quantiles" = "quantile", "Custom cuts" = "custom"),
          selected = "quantile"
        ),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'custom'", ns("bin_mode")),
          shiny::textInput(ns("custom_cuts"), "Custom cuts (comma-separated)", "0,5,10,20")
        ),
        shiny::conditionalPanel(
          sprintf("input['%s'] != 'custom'", ns("bin_mode")),
          shiny::sliderInput(ns("n_bins"), "Number of bins", min = 3, max = 9, value = 7, step = 1)
        ),
        shiny::checkboxInput(ns("show_centroids"), "Show cell centroids (labels)", value = FALSE),
        shiny::actionButton(ns("refresh"), "Fetch & Summarise", class = "btn-primary"),
        htmltools::br(), htmltools::br(),
        shiny::downloadButton(ns("dl_csv"), "Download summary CSV")
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Map & Table",
                          leaflet::leafletOutput(ns("map"), height = 620),
                          htmltools::br(),
                          DT::DTOutput(ns("tbl"))
          ),
          shiny::tabPanel("Species Info",
                          shiny::uiOutput(ns("species_info")),
                          plotOutput(ns("records_over_time"), height = 250),
                          plotOutput(ns("depth_hist"), height = 250),
                          plotOutput(ns("temp_hist"), height = 250),
                          plotOutput(ns("sal_hist"), height = 250)
          )
        )
      )
    )
  )
}

MapSearchServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Inputs -> geometry -------------------------------------------------
    geom_wkt <- reactive({
      if (isTRUE(input$aus_only)) {
        bbox_wkt(110, -45, 160, -10)
      } else {
        bbox_wkt(input$lng_range[1], input$lat_range[1],
                 input$lng_range[2], input$lat_range[2])
      }
    })
    
    # ---- Fetch + normalize observations ------------------------------------
    obis_df <- eventReactive(input$refresh, {
      req(input$date_range)
      shiny::withProgress(message = "Querying OBIS …", value = 0, {
        sp_vec <- input$sp
        if (length(sp_vec) == 0) return(tibble::tibble())
        n <- length(sp_vec)
        dfs <- purrr::map(seq_along(sp_vec), function(i) {
          shiny::incProgress(i / n, detail = sp_vec[i])
          df_raw <- tryCatch({
            fetch_obis(  # <<— uses commons.R memoised fetcher
              species = sp_vec[i], wkt = geom_wkt(),
              start = input$date_range[1], end = input$date_range[2]
            )
          }, error = function(e) NULL)
          normalize_obis(df_raw)  # <<— uses commons.R normaliser
        })
        dplyr::bind_rows(dfs)
      })
    }, ignoreInit = TRUE)
    
    # Update metric taxon choices
    observeEvent(obis_df(), {
      df <- obis_df()
      taxa <- sort(unique(df$scientificName[!is.na(df$scientificName)]))
      shiny::updateSelectInput(session, "metric_taxon", choices = taxa,
                               selected = if (length(taxa)) taxa[1] else character(0))
    })
    
    # ---- Build grid + aggregate --------------------------------------------
    grid_data <- reactive({
      df <- obis_df()
      if (nrow(df) == 0) return(NULL)
      
      pts <- sf::st_as_sf(df, coords = c("decimalLongitude","decimalLatitude"),
                          crs = 4326, remove = FALSE) |>
        sf::st_transform(3857)
      
      cell_m <- input$cell_km * 1000
      bbox <- sf::st_bbox(pts)
      bb_sfc <- sf::st_as_sfc(bbox)
      sf::st_crs(bb_sfc) <- sf::st_crs(pts)   # ensure CRS is set
      grd <- sf::st_make_grid(bb_sfc, cellsize = cell_m, square = TRUE)
      
      if (length(grd) == 0) return(NULL)
      grid_sf <- sf::st_sf(cell_id = seq_along(grd), geometry = grd)
      
      joined <- sf::st_join(pts, grid_sf, left = FALSE)
      if (nrow(joined) == 0) return(NULL)
      
      summ_all <- joined |>
        sf::st_drop_geometry() |>
        dplyr::group_by(.data$cell_id) |>
        dplyr::summarise(
          n_occ = dplyr::n(),
          spp_rich = dplyr::n_distinct(stats::na.omit(.data$scientificName)),
          n_with_abund = sum(!is.na(.data$individualCount)),
          mean_abund = ifelse(n_with_abund > 0, mean(.data$individualCount, na.rm = TRUE), NA_real_),
          sum_abund  = ifelse(n_with_abund > 0, sum(.data$individualCount,  na.rm = TRUE), NA_real_),
          .groups = "drop"
        )
      
      summ_taxon <- joined |>
        sf::st_drop_geometry() |>
        dplyr::filter(!is.na(.data$scientificName)) |>
        dplyr::group_by(.data$cell_id, taxon = .data$scientificName) |>
        dplyr::summarise(
          n_occ = dplyr::n(),
          n_with_abund = sum(!is.na(.data$individualCount)),
          mean_abund = ifelse(n_with_abund > 0, mean(.data$individualCount, na.rm = TRUE), NA_real_),
          sum_abund  = ifelse(n_with_abund > 0, sum(.data$individualCount,  na.rm = TRUE), NA_real_),
          .groups = "drop"
        )
      
      list(
        grid = sf::st_transform(grid_sf, 4326),
        all  = summ_all,
        by_taxon = summ_taxon
      )
    })
    
    # ---- Map ---------------------------------------------------------------
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        leaflet::setView(lng = 120, lat = -20, zoom = 3)
    })
    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)
    
    observeEvent(list(grid_data(), input$metric, input$metric_scope,
                      input$metric_taxon, input$bin_mode, input$n_bins,
                      input$custom_cuts, input$show_centroids), {
                        gd <- grid_data()
                        proxy <- leaflet::leafletProxy("map", session = session)
                        proxy |> leaflet::clearShapes() |> leaflet::clearControls() |> leaflet::clearMarkers()
                        if (is.null(gd)) return()
                        
                        # selection rectangle
                        if (isTRUE(input$aus_only)) {
                          proxy |> leaflet::addRectangles(lng1 = 110, lat1 = -45, lng2 = 160, lat2 = -10,
                                                          fill = FALSE, weight = 1, color = "#888888")
                        } else {
                          proxy |> leaflet::addRectangles(lng1 = input$lng_range[1], lat1 = input$lat_range[1],
                                                          lng2 = input$lng_range[2], lat2 = input$lat_range[2],
                                                          fill = FALSE, weight = 1, color = "#888888")
                        }
                        
                        # compose metric
                        if (input$metric == "spp_rich") {
                          dat <- gd$grid |>
                            dplyr::inner_join(dplyr::transmute(gd$all, cell_id, value = .data$spp_rich), by = "cell_id")
                          leg_title <- "Species richness"
                        } else {
                          metric_field <- switch(input$metric,
                                                 n_occ = "n_occ", mean_abund = "mean_abund", sum_abund = "sum_abund")
                          if (input$metric_scope == "all") {
                            dat <- gd$grid |>
                              dplyr::inner_join(dplyr::transmute(gd$all, cell_id, value = .data[[metric_field]]), by = "cell_id")
                            leg_title <- metric_field
                          } else {
                            req(input$metric_taxon)
                            tax_df <- gd$by_taxon |>
                              dplyr::filter(.data$taxon == input$metric_taxon) |>
                              dplyr::transmute(cell_id, value = .data[[metric_field]])
                            if (nrow(tax_df) == 0) return()
                            dat <- gd$grid |> dplyr::inner_join(tax_df, by = "cell_id")
                            leg_title <- paste(metric_field, input$metric_taxon)
                          }
                        }
                        
                        if (nrow(dat) == 0) return()
                        vals <- dat$value
                        vals_ok <- vals[is.finite(vals)]
                        if (length(vals_ok) == 0) return()
                        
                        cent <- sf::st_centroid(dat$geometry)
                        cent_xy <- sf::st_coordinates(cent)
                        popup <- paste0("<b>Cell:</b> ", dat$cell_id, "<br/>Value: ", round(vals, 2))
                        
                        pal <- make_bin_pal(vals, mode = input$bin_mode,
                                            cuts = parse_cuts(input$custom_cuts), n = input$n_bins)
                        
                        proxy |>
                          leaflet::addPolygons(
                            data = dat,
                            weight = 0.3, color = "#666666",
                            fillOpacity = ifelse(is.na(vals) | vals == 0, 0, 0.8),
                            fillColor   = ifelse(is.na(vals) | vals == 0, "transparent", pal(vals)),
                            popup = popup
                          ) |>
                          leaflet::addLegend("bottomright", pal = pal, values = vals,
                                             title = leg_title, opacity = 1)
                        
                        if (isTRUE(input$show_centroids)) {
                          proxy |> leaflet::addCircleMarkers(lng = cent_xy[,1], lat = cent_xy[,2],
                                                             radius = 2, stroke = FALSE, fillOpacity = 0.9,
                                                             color = "#000000")
                        }
                      })
    
    # ---- Summary table ------------------------------------------------------
    output$tbl <- DT::renderDT({
      gd <- grid_data()
      if (is.null(gd) || nrow(gd$all) == 0)
        return(DT::datatable(tibble::tibble(msg = "No records found."), rownames = FALSE))
      dat <- gd$grid |> dplyr::inner_join(gd$all, by = "cell_id")
      cent <- sf::st_centroid(dat$geometry)
      coords <- sf::st_coordinates(cent)
      out <- dat |> sf::st_drop_geometry() |>
        dplyr::mutate(centroid_lon = round(coords[,1], 5),
                      centroid_lat = round(coords[,2], 5))
      DT::datatable(out, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
    
    # ---- CSV download -------------------------------------------------------
    output$dl_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("obis_grid_summary_", gsub("[^A-Za-z0-9]+","_", paste(input$sp, collapse="_")), ".csv")
      },
      content  = function(file) {
        gd <- grid_data()
        if (is.null(gd) || nrow(gd$all) == 0) { readr::write_csv(tibble::tibble(), file); return() }
        dat <- gd$grid |> dplyr::inner_join(gd$all, by = "cell_id")
        cent <- sf::st_centroid(dat$geometry)
        coords <- sf::st_coordinates(cent)
        out <- dat |> sf::st_drop_geometry() |>
          dplyr::mutate(centroid_lon = coords[,1], centroid_lat = coords[,2])
        readr::write_csv(out, file)
      }
    )
    
    # ---- Species Info -------------------------------------------------------
    safe_val <- function(df, col) {
      if (!is.null(df) && col %in% names(df)) as.character(df[[col]][1]) else NA
    }
    
    species_info <- reactive({
      req(input$sp)
      if (length(input$sp) != 1) return(NULL)
      sp <- input$sp
      info <- tryCatch({ robis::taxon(sp) }, error = function(e) NULL)
      checklist <- tryCatch({ robis::checklist(scientificname = sp) }, error = function(e) NULL)
      list(info = info, checklist = checklist)
    })
    
    output$species_info <- renderUI({
      dat <- species_info()
      if (is.null(dat)) return(htmltools::HTML("<p>Select a single species to see details.</p>"))
      info <- dat$info
      checklist <- dat$checklist
      if ((is.null(info) || nrow(info) == 0) &&
          (is.null(checklist) || nrow(checklist) == 0)) {
        return(htmltools::HTML("<p><b>No background information available from OBIS.</b></p>"))
      }
      
      yrange <- if (!is.null(checklist) && all(c("startyear","endyear") %in% names(checklist))) {
        paste0(safe_val(checklist, "startyear"), " – ", safe_val(checklist, "endyear"))
      } else {
        year_range_from_occ(input$sp)
      }
      
      htmltools::tagList(
        htmltools::h4("Taxonomy"),
        if (!is.null(info) && nrow(info) > 0) {
          htmltools::HTML(paste0(
            "<b>Scientific name:</b> ", safe_val(info, "scientificname"), "<br/>",
            "<b>AphiaID:</b> ", safe_val(info, "aphiaid"), "<br/>",
            "<b>Status:</b> ", safe_val(info, "status"), "<br/>",
            "<b>Environment:</b> ", safe_val(info, "environment"), "<br/>"
          ))
        },
        htmltools::h4("Checklist summary"),
        if (!is.null(checklist) && nrow(checklist) > 0) {
          htmltools::HTML(paste0(
            "<b>Total records:</b> ", safe_val(checklist, "records"), "<br/>",
            "<b>Year range:</b> ", yrange, "<br/>",
            "<b>Phylum:</b> ", safe_val(checklist, "phylum"), "<br/>",
            "<b>Class:</b> ", safe_val(checklist, "class"), "<br/>",
            "<b>Order:</b> ", safe_val(checklist, "order"), "<br/>",
            "<b>Family:</b> ", safe_val(checklist, "family"), "<br/>",
            "<b>Genus:</b> ", safe_val(checklist, "genus")
          ))
        }
      )
    })
    
    # ---- Species Plots ------------------------------------------------------
    output$records_over_time <- renderPlot({
      df <- obis_df()
      if (nrow(df) == 0 || all(is.na(df$eventDate))) return(NULL)
      ggplot2::ggplot(df, ggplot2::aes(lubridate::year(eventDate))) +
        ggplot2::geom_bar(fill = "steelblue") +
        ggplot2::labs(x = "Year", y = "Records", title = "Records over time") +
        ggplot2::theme_minimal()
    })
    
    output$depth_hist <- renderPlot({
      df <- obis_df()
      if (nrow(df) == 0 || all(is.na(df$depth))) return(NULL)
      ggplot2::ggplot(df, ggplot2::aes(depth)) +
        ggplot2::geom_histogram(fill = "darkorange", bins = 30) +
        ggplot2::labs(x = "Depth (m)", y = "Count", title = "Depth distribution") +
        ggplot2::theme_minimal()
    })
    
    output$temp_hist <- renderPlot({
      df <- obis_df()
      if (nrow(df) == 0 || all(is.na(df$temperature))) return(NULL)
      ggplot2::ggplot(df, ggplot2::aes(temperature)) +
        ggplot2::geom_histogram(fill = "firebrick", bins = 20) +
        ggplot2::labs(x = "Temperature (°C)", y = "Count", title = "Sea surface temperature") +
        ggplot2::theme_minimal()
    })
    
    output$sal_hist <- renderPlot({
      df <- obis_df()
      if (nrow(df) == 0 || all(is.na(df$salinity))) return(NULL)
      ggplot2::ggplot(df, ggplot2::aes(salinity)) +
        ggplot2::geom_histogram(fill = "darkgreen", bins = 20) +
        ggplot2::labs(x = "Salinity (PSU)", y = "Count", title = "Sea surface salinity") +
        ggplot2::theme_minimal()
    })
  })
}
