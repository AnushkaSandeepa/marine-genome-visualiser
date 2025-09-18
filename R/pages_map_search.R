# ===== File: R/pages_map_search.R =====
# Map Search (module) — OBIS grid summary with Species/AphiaID search and metrics

MapSearchUI <- function(id) {
  ns <- NS(id)
  shiny::fluidPage(
    shiny::titlePanel("Ocean Genome Explorer"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::radioButtons(
          ns("search_mode"), "Search by",
          choices = c("Species name(s)" = "name", "AphiaID(s)" = "aphia"),
          selected = "name", inline = TRUE
        ),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'name'", ns("search_mode")),
          shiny::selectizeInput(
            ns("sp"), "Species",
            choices = stats::setNames(client_species$species,
                                      paste(client_species$common_name, "—", client_species$species)),
            selected = client_species$species[1:3], multiple = TRUE
          )
        ),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'aphia'", ns("search_mode")),
          shiny::textInput(ns("aphia_txt"), "AphiaID(s) (comma-separated)", "212381, 212127")
        ),
        
        shiny::checkboxInput(ns("aus_only"), "Limit to Australia region", value = TRUE),
        shiny::conditionalPanel(
          sprintf("input['%s'] == false", ns("aus_only")),
          shiny::sliderInput(ns("lng_range"), "Longitude (°)", min = -180, max = 180, value = c(110, 160), step = 1),
          shiny::sliderInput(ns("lat_range"), "Latitude (°)", min = -90,  max = 90,  value = c(-45, -10), step = 1)
        ),
        shiny::dateRangeInput(
          ns("date_range"), "Date range",
          start = "2000-01-01", end = Sys.Date(),
          min = "1900-01-01", max = Sys.Date()
        ),
        shiny::hr(),
        
        shiny::sliderInput(ns("cell_km"), "Cell size (km)", min = 10, max = 250, value = 50, step = 10),
        shiny::selectInput(ns("metric"), "Summary metric", choices = c(
          "Occurrence count" = "n_occ",
          "Species richness" = "spp_rich",
          "Mean individual count (non-missing)" = "mean_abund",
          "Sum individual count (non-missing)"  = "sum_abund"
        ), selected = "n_occ"),
        
        shiny::conditionalPanel(
          sprintf("input['%s'] != 'spp_rich'", ns("metric")),
          shiny::radioButtons(ns("metric_scope"), "Scope for count/abundance",
                              choices = c("All selected taxa" = "all", "Single taxon" = "one"),
                              selected = "all", inline = TRUE)
        ),
        shiny::conditionalPanel(
          sprintf("input['%s'] != 'spp_rich' && input['%s'] == 'one'", ns("metric"), ns("metric_scope")),
          shiny::selectInput(ns("metric_taxon"), "Taxon for metric", choices = character(0))
        ),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'spp_rich' && input['%s'] == 'name' && (input['%s'] || []).length <= 1",
                  ns("metric"), ns("search_mode"), ns("sp")),
          htmltools::tags$small(style="color:#666;",
                                "Richness = 1 when only one species is selected. Select multiple species to see variation.")
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
        leaflet::leafletOutput(ns("map"), height = 620),
        htmltools::br(),
        DT::DTOutput(ns("tbl")),
        htmltools::br(),
        htmltools::tags$small(htmltools::HTML(
          "<b>Note:</b> <code>individualCount</code> = number of individuals reported in a record.
           Missing (NA) means 'not reported' and is not treated as zero; mean/sum use only non-missing counts."
        ))
      )
    )
  )
}

MapSearchServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Helpers specific to this module ------------------------------------
    POLITE_DELAY <- 0.35
    
    parse_ids <- function(txt) {
      if (is.null(txt) || !nzchar(txt)) return(integer(0))
      ids <- suppressWarnings(as.integer(strsplit(gsub("\\s+","", txt), ",")[[1]]))
      ids[is.finite(ids)]
    }
    
    normalize_obis <- function(df) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(tibble::tibble())
      need_chr <- c("scientificName", "datasetID", "basisOfRecord", "eventDate")
      need_num <- c("decimalLongitude", "decimalLatitude", "depth", "individualCount")
      for (nm in c(need_chr, need_num)) if (!nm %in% names(df)) df[[nm]] <- NA
      
      df |>
        dplyr::transmute(
          scientificName   = as.character(.data$scientificName),
          decimalLongitude = suppressWarnings(as.numeric(.data$decimalLongitude)),
          decimalLatitude  = suppressWarnings(as.numeric(.data$decimalLatitude)),
          eventDate        = safe_event_date(.data$eventDate),
          depth            = suppressWarnings(as.numeric(.data$depth)),
          individualCount  = suppressWarnings(as.numeric(.data$individualCount)),
          datasetID        = as.character(.data$datasetID),
          basisOfRecord    = as.character(.data$basisOfRecord)
        ) |>
        dplyr::filter(
          !is.na(.data$decimalLongitude), !is.na(.data$decimalLatitude),
          dplyr::between(.data$decimalLongitude, -180, 180),
          dplyr::between(.data$decimalLatitude,  -90,   90)
        )
    }
    
    make_bin_pal <- function(vals,
                             mode = c("equal","quantile","custom"),
                             cuts = c(0,5,10,20), n = 7) {
      mode <- match.arg(mode)
      vals_ok <- vals[is.finite(vals)]
      if (length(vals_ok) == 0 || length(unique(vals_ok)) <= 1) {
        return(leaflet::colorNumeric("viridis", domain = vals_ok, na.color = "#F0F0F0"))
      }
      if (mode == "quantile") {
        q <- stats::quantile(vals_ok, probs = seq(0, 1, length.out = n), na.rm = TRUE)
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
      leaflet::colorBin("viridis", domain = vals_ok, bins = brks, na.color = "#F0F0F0")
    }
    
    parse_cuts <- function(txt) as.numeric(strsplit(gsub("\\s+", "", txt), ",")[[1]])
    
    .fetch_obis_name <- function(species, wkt = NULL, start = NULL, end = NULL) {
      Sys.sleep(POLITE_DELAY)
      robis::occurrence(
        scientificname = species,
        geometry = wkt,
        startdate = if (!is.null(start)) as.Date(start) else NULL,
        enddate   = if (!is.null(end))   as.Date(end)   else NULL
      )
    }
    .fetch_obis_aphia <- function(aphia, wkt = NULL, start = NULL, end = NULL) {
      Sys.sleep(POLITE_DELAY)
      robis::occurrence(
        aphiaid = aphia,
        geometry = wkt,
        startdate = if (!is.null(start)) as.Date(start) else NULL,
        enddate   = if (!is.null(end))   as.Date(end)   else NULL
      )
    }
    fetch_obis_name  <- memoise::memoise(.fetch_obis_name)
    fetch_obis_aphia <- memoise::memoise(.fetch_obis_aphia)
    
    # --- Inputs -> geometry --------------------------------------------------
    geom_wkt <- reactive({
      if (isTRUE(input$aus_only)) {
        bbox_wkt(110, -45, 160, -10)
      } else {
        bbox_wkt(input$lng_range[1], input$lat_range[1],
                 input$lng_range[2], input$lat_range[2])
      }
    })
    
    # --- Fetch + normalize observations -------------------------------------
    obis_df <- eventReactive(input$refresh, {
      req(input$date_range)
      shiny::withProgress(message = "Querying OBIS …", value = 0, {
        if (input$search_mode == "name") {
          sp_vec <- input$sp
          if (length(sp_vec) == 0) return(tibble::tibble())
          n <- length(sp_vec)
          dfs <- purrr::map(seq_along(sp_vec), function(i) {
            shiny::incProgress(i / n, detail = sp_vec[i])
            df <- tryCatch({
              fetch_obis_name(
                species = sp_vec[i], wkt = geom_wkt(),
                start = input$date_range[1], end = input$date_range[2]
              )
            }, error = function(e) NULL)
            normalize_obis(df)
          })
          dplyr::bind_rows(dfs)
          
        } else { # AphiaIDs
          ids <- parse_ids(input$aphia_txt)
          if (length(ids) == 0) return(tibble::tibble())
          n <- length(ids)
          dfs <- purrr::map(seq_along(ids), function(i) {
            shiny::incProgress(i / n, detail = paste0("AphiaID ", ids[i]))
            df <- tryCatch({
              fetch_obis_aphia(
                aphia = ids[i], wkt = geom_wkt(),
                start = input$date_range[1], end = input$date_range[2]
              )
            }, error = function(e) NULL)
            normalize_obis(df)
          })
          dplyr::bind_rows(dfs)
        }
      })
    }, ignoreInit = TRUE)
    
    # Update metric taxon choices after data arrives
    observeEvent(obis_df(), {
      df <- obis_df()
      taxa <- sort(unique(df$scientificName[!is.na(df$scientificName)]))
      shiny::updateSelectInput(session, "metric_taxon", choices = taxa,
                               selected = if (length(taxa)) taxa[1] else character(0))
    })
    
    # --- Build grid + aggregate ---------------------------------------------
    grid_data <- reactive({
      df <- obis_df()
      if (nrow(df) == 0) return(NULL)
      
      pts <- sf::st_as_sf(df, coords = c("decimalLongitude","decimalLatitude"),
                          crs = 4326, remove = FALSE) |>
        sf::st_transform(3857)
      
      cell_m <- input$cell_km * 1000
      bbox <- sf::st_bbox(pts)
      grd <- sf::st_make_grid(sf::st_as_sfc(bbox), cellsize = cell_m, square = TRUE)
      if (length(grd) == 0) return(NULL)
      grid_sf <- sf::st_sf(cell_id = seq_along(grd), geometry = grd)
      
      joined <- sf::st_join(pts, grid_sf, left = FALSE)
      if (nrow(joined) == 0) return(NULL)
      
      # All taxa per cell
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
      
      # Per-taxon per cell
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
    
    # --- Map base ------------------------------------------------------------
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        leaflet::setView(lng = 120, lat = -20, zoom = 3)
    })
    
    # --- Choropleth drawing --------------------------------------------------
    observeEvent(list(grid_data(), input$metric, input$metric_scope,
                      input$metric_taxon, input$bin_mode, input$n_bins,
                      input$custom_cuts, input$show_centroids), {
                        
                        gd <- grid_data()
                        proxy <- leaflet::leafletProxy(ns("map"))
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
                                                 n_occ = "n_occ", mean_abund = "mean_abund", sum_abund = "sum_abund"
                          )
                          if (input$metric_scope == "all") {
                            dat <- gd$grid |>
                              dplyr::inner_join(dplyr::transmute(gd$all, cell_id, value = .data[[metric_field]]), by = "cell_id")
                            leg_title <- switch(metric_field,
                                                n_occ = "Occurrence count (all taxa)",
                                                mean_abund = "Mean individual count (all taxa)",
                                                sum_abund = "Sum individual count (all taxa)"
                            )
                          } else {
                            req(input$metric_taxon)
                            tax_df <- gd$by_taxon |>
                              dplyr::filter(.data$taxon == input$metric_taxon) |>
                              dplyr::transmute(cell_id, value = .data[[metric_field]])
                            if (nrow(tax_df) == 0) return()
                            dat <- gd$grid |> dplyr::inner_join(tax_df, by = "cell_id")
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
                        
                        cent <- sf::st_centroid(dat$geometry)
                        cent_xy <- sf::st_coordinates(cent)
                        
                        popup <- paste0(
                          "<b>Cell ID:</b> ", dat$cell_id, "<br/>",
                          "<b>Centroid:</b> ", round(cent_xy[,1], 3), ", ", round(cent_xy[,2], 3), "<br/>",
                          "<b>", leg_title, ":</b> ", ifelse(is.na(vals), "NA", round(vals, 2))
                        )
                        
                        # single-value legend guard
                        if (length(unique(vals_ok)) == 1) {
                          single <- unique(vals_ok)
                          pal_single <- leaflet::colorFactor("viridis", domain = single)
                          proxy |>
                            leaflet::addPolygons(
                              data = dat, weight = 0.3, color = "#666666",
                              fillOpacity = 0.8, fillColor = pal_single(vals),
                              popup = popup, label = paste0(leg_title, ": ", single)
                            ) |>
                            leaflet::addLegend("bottomright",
                                               colors = pal_single(single), labels = as.character(single),
                                               title = leg_title, opacity = 1
                            )
                          if (isTRUE(input$show_centroids)) {
                            proxy |> leaflet::addCircleMarkers(
                              lng = cent_xy[,1], lat = cent_xy[,2],
                              radius = 2, stroke = FALSE, fillOpacity = 0.9,
                              color = "#000000", label = paste0("Cell ", dat$cell_id)
                            )
                          }
                          return(invisible(NULL))
                        }
                        
                        # general binned palette
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
                          leaflet::addPolygons(
                            data = dat, weight = 0.3, color = "#666666",
                            fillOpacity = ifelse(is.na(vals) | vals == 0, 0, 0.8),
                            fillColor   = ifelse(is.na(vals) | vals == 0, "transparent", pal(vals)),
                            popup = popup,
                            label = paste0(leg_title, ": ", ifelse(is.na(vals), "NA", round(vals, 2)))
                          ) |>
                          leaflet::addLegend("bottomright", pal = pal, values = vals,
                                             title = leg_title, opacity = 1)
                        
                        if (isTRUE(input$show_centroids)) {
                          proxy |> leaflet::addCircleMarkers(
                            lng = cent_xy[,1], lat = cent_xy[,2],
                            radius = 2, stroke = FALSE, fillOpacity = 0.9,
                            color = "#000000", label = paste0("Cell ", dat$cell_id)
                          )
                        }
                      })
    
    # --- Summary table (ALL taxa) -------------------------------------------
    output$tbl <- DT::renderDT({
      gd <- grid_data()
      if (is.null(gd) || nrow(gd$all) == 0) {
        return(DT::datatable(tibble::tibble(msg = "No records found for selection."), rownames = FALSE))
      }
      dat <- gd$grid |> dplyr::inner_join(gd$all, by = "cell_id")
      cent <- sf::st_centroid(dat$geometry)
      coords <- sf::st_coordinates(cent)
      out <- dat |> sf::st_drop_geometry() |>
        dplyr::mutate(centroid_lon = round(coords[,1], 5),
                      centroid_lat = round(coords[,2], 5)) |>
        dplyr::select(.data$cell_id, .data$centroid_lon, .data$centroid_lat,
                      .data$n_occ, .data$spp_rich, .data$n_with_abund,
                      .data$mean_abund, .data$sum_abund)
      DT::datatable(out, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
    
    # --- CSV download (ALL taxa grid) ---------------------------------------
    output$dl_csv <- shiny::downloadHandler(
      filename = function() {
        if (input$search_mode == "name") {
          paste0("obis_grid_summary_",
                 gsub("[^A-Za-z0-9]+","_", paste(input$sp, collapse="_")), ".csv")
        } else {
          paste0("obis_grid_summary_aphia_",
                 gsub("\\s+","", gsub(",","_", input$aphia_txt)), ".csv")
        }
      },
      content  = function(file) {
        gd <- grid_data()
        if (is.null(gd) || nrow(gd$all) == 0) {
          readr::write_csv(tibble::tibble(), file); return()
        }
        dat <- gd$grid |> dplyr::inner_join(gd$all, by = "cell_id")
        cent <- sf::st_centroid(dat$geometry)
        coords <- sf::st_coordinates(cent)
        out <- dat |> sf::st_drop_geometry() |>
          dplyr::mutate(centroid_lon = coords[,1], centroid_lat = coords[,2])
        readr::write_csv(out, file)
      }
    )
  })
}
