# ===== File: R/pages_map_search.R =====
suppressPackageStartupMessages({
  library(shiny); library(leaflet); library(DT)
  library(sf); library(purrr); library(dplyr); library(readr); library(stringr)
})

# ---- helpers that read from full_species / COL_* (defined in commons.R) ----
csv_ids_for_aphia <- function(aph) {
  if (is.na(aph)) return(list(ncbi = NA_integer_, sci = NA_character_))
  row <- tryCatch({
    full_species %>%
      filter(.data[[COL_APHIA]] == aph) %>%
      slice(1)
  }, error = function(e) tibble())
  list(
    ncbi = if (nrow(row) && !is.na(COL_NCBI) && COL_NCBI %in% names(row))
      suppressWarnings(as.integer(row[[COL_NCBI]])) else NA_integer_,
    sci  = if (nrow(row) && "species_canonical" %in% names(row))
      as.character(row$species_canonical[[1]]) else NA_character_
  )
}

make_taxon_label <- function(aph, name = NA_character_) {
  ids <- csv_ids_for_aphia(aph)
  nm  <- ifelse(!is.na(name) && nzchar(name), name,
                ifelse(!is.na(ids$sci) && nzchar(ids$sci), ids$sci, "(unknown)"))
  paste0(nm,
         "  (AphiaID:", ifelse(is.na(aph), "-", aph),
         ", NCBI:", ifelse(is.na(ids$ncbi), "-", ids$ncbi), ")")
}

MapSearchUI <- function(id) {
  ns <- NS(id)
  shiny::fluidPage(
    shiny::titlePanel("Ocean Genome Explorer"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectizeInput(
          ns("sp"),
          "Species / AphiaID / NCBI",
          choices = NULL, multiple = TRUE,
          options = list(
            create = TRUE, createOnBlur = TRUE, persist = TRUE, selectOnTab = TRUE,
            placeholder = "Type a name (incl. synonyms), aphia:12345, ncbi:67890, or a number"
          )
        ),
        shiny::checkboxInput(ns("aus_only"), "Limit to Australia region", value = TRUE),
        shiny::conditionalPanel(
          sprintf("input['%s'] == false", ns("aus_only")),
          shiny::sliderInput(ns("lng_range"), "Longitude (°)", min = -180, max = 180, value = c(110, 160), step = 1),
          shiny::sliderInput(ns("lat_range"), "Latitude (°)",  min = -90,  max =  90, value = c(-45, -10), step = 1)
        ),
        shiny::dateRangeInput(
          ns("date_range"), "Date range",
          start = "2000-01-01", end = Sys.Date(),
          min = "1900-01-01", max = Sys.Date()
        ),
        shiny::hr(),
        shiny::sliderInput(ns("depth_range"), "Depth (m)", min = 0, max = 6000, value = c(0, 200), step = 50),
        shiny::hr(),
        shiny::sliderInput(ns("cell_km"), "Cell size (km)", min = 10, max = 250, value = 50, step = 10),
        shiny::selectInput(ns("metric"), "Summary metric",
                           choices = c(
                             "Occurrence count" = "n_occ",
                             "Species richness" = "spp_rich",
                             "Mean individual count (non-missing)" = "mean_abund",
                             "Sum individual count (non-missing)"  = "sum_abund"
                           ),
                           selected = "n_occ"
        ),
        shiny::conditionalPanel(
          sprintf("input['%s'] != 'spp_rich'", ns("metric")),
          shiny::radioButtons(ns("metric_scope"), "Scope for count/abundance",
                              choices  = c("All selected taxa" = "all", "Single taxon" = "one"),
                              selected = "all", inline = TRUE
          )
        ),
        # values = AphiaIDs; labels = "Name (AphiaID, NCBI)"
        shiny::conditionalPanel(
          sprintf("input['%s'] != 'spp_rich' && input['%s'] == 'one'", ns("metric"), ns("metric_scope")),
          shiny::selectInput(ns("metric_taxon"), "Taxon for metric", choices = character(0))
        ),
        shiny::hr(),
        shiny::radioButtons(ns("bin_mode"), "Legend binning",
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
    
    # suggestions = species names (IDs can still be typed in)
    shiny::updateSelectizeInput(
      session, "sp",
      choices  = client_species$species,
      selected = head(client_species$species, 1),
      server   = TRUE
    )
    
    geom_wkt <- reactive({
      if (isTRUE(input$aus_only)) {
        bbox_wkt(110, -45, 160, -10)
      } else {
        bbox_wkt(input$lng_range[1], input$lat_range[1],
                 input$lng_range[2], input$lat_range[2])
      }
    })
    
    # ---- Fetch + normalize (stamp resolved Aphia AFTER normalization) ------
    obis_df <- eventReactive(input$refresh, {
      req(input$date_range, input$depth_range)
      withProgress(message = "Querying OBIS …", value = 0, {
        sp_vec <- input$sp
        if (length(sp_vec) == 0) return(empty_obis_norm())
        n <- length(sp_vec)
        
        dfs <- purrr::map(seq_along(sp_vec), function(i) {
          incProgress(i / n, detail = sp_vec[i])
          
          df_raw <- get_obis_by_query(
            q = sp_vec[i],
            geometry  = geom_wkt(),
            startdate = input$date_range[1],
            enddate   = input$date_range[2]
          )
          if (!is.data.frame(df_raw) || nrow(df_raw) == 0) return(empty_obis_norm())
          
          # ✅ keep column by adding it AFTER normalization
          df_norm <- normalize_obis(df_raw)
          df_norm$resolvedAphia <- resolve_to_aphia(sp_vec[i])
          df_norm
        })
        
        df_all <- dplyr::bind_rows(dfs)
        if (nrow(df_all) == 0) return(empty_obis_norm())
        if (!"depth" %in% names(df_all)) df_all$depth <- NA_real_
        if (!"resolvedAphia" %in% names(df_all)) df_all$resolvedAphia <- NA_integer_
        
        df_all |>
          dplyr::filter(is.na(.data$depth) |
                          dplyr::between(.data$depth, input$depth_range[1], input$depth_range[2]))
      })
    }, ignoreInit = TRUE)
    
    # ---- Build grid + aggregate (BY cell × AphiaID) ------------------------
    grid_data <- reactive({
      df <- obis_df()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      if (!all(c("decimalLongitude", "decimalLatitude") %in% names(df))) return(NULL)
      if (all(is.na(df$decimalLongitude)) || all(is.na(df$decimalLatitude))) return(NULL)
      
      pts <- tryCatch({
        sf::st_as_sf(df, coords = c("decimalLongitude","decimalLatitude"),
                     crs = 4326, remove = FALSE) |>
          sf::st_transform(3857)
      }, error = function(e) NULL)
      if (is.null(pts) || nrow(pts) == 0) return(NULL)
      
      cell_m <- input$cell_km * 1000
      bbox <- sf::st_bbox(pts)
      if (any(is.na(bbox)) || diff(bbox[c("xmin","xmax")]) == 0 || diff(bbox[c("ymin","ymax")]) == 0) return(NULL)
      
      grd <- tryCatch({ sf::st_make_grid(sf::st_as_sfc(bbox), cellsize = cell_m, square = TRUE) }, error = function(e) NULL)
      if (is.null(grd) || length(grd) == 0) return(NULL)
      
      grid_sf <- sf::st_sf(cell_id = seq_along(grd), geometry = grd)
      joined  <- sf::st_join(pts, grid_sf, left = FALSE)
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
      
      by_taxon <- joined |>
        sf::st_drop_geometry() |>
        dplyr::mutate(aphia = suppressWarnings(as.integer(.data$resolvedAphia))) |>
        dplyr::filter(!is.na(.data$aphia)) |>
        dplyr::group_by(.data$cell_id, .data$aphia) |>
        dplyr::summarise(
          taxon_name   = dplyr::first(na.omit(.data$scientificName)),
          n_occ        = dplyr::n(),
          n_with_abund = sum(!is.na(.data$individualCount)),
          mean_abund   = ifelse(n_with_abund > 0, mean(.data$individualCount, na.rm = TRUE), NA_real_),
          sum_abund    = ifelse(n_with_abund > 0, sum(.data$individualCount,  na.rm = TRUE), NA_real_),
          latest_date  = suppressWarnings(max(as.Date(.data$eventDate), na.rm = TRUE)),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          label = vapply(seq_len(n()), function(i) make_taxon_label(aphia[i], taxon_name[i]), character(1))
        )
      
      list(grid = sf::st_transform(grid_sf, 4326), all = summ_all, by_taxon = by_taxon)
    })
    
    # ---- Taxon-for-metric (choices = named vector; values = AphiaIDs) ------
    observeEvent(list(grid_data(), obis_df()), ignoreInit = FALSE, {
      gd <- grid_data()
      choices <- character(0)
      if (!is.null(gd) && !is.null(gd$by_taxon) &&
          all(c("aphia","label") %in% names(gd$by_taxon))) {
        choices_vec <- gd$by_taxon %>% distinct(aphia, label) %>% arrange(label)
        choices <- setNames(as.character(choices_vec$aphia), choices_vec$label)
      }
      updateSelectInput(session, "metric_taxon",
                        choices = choices,
                        selected = if (length(choices)) choices[[1]] else character(0))
    })
    
    # ---- Map ---------------------------------------------------------------
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        leaflet::setView(lng = 120, lat = -20, zoom = 3)
    })
    
    observeEvent(list(grid_data(), input$metric, input$metric_scope,
                      input$metric_taxon, input$bin_mode, input$n_bins,
                      input$custom_cuts, input$show_centroids), {
                        gd <- grid_data()
                        proxy <- leaflet::leafletProxy(ns("map"))
                        proxy |> leaflet::clearShapes() |> leaflet::clearControls() |> leaflet::clearMarkers()
                        
                        if (is.null(gd)) {
                          showNotification("No mappable records for current selection.", type = "message", duration = 4)
                          return()
                        }
                        
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
                        metric_field <- NULL
                        if (input$metric == "spp_rich") {
                          dat <- gd$grid |>
                            dplyr::inner_join(gd$all |> dplyr::transmute(cell_id, value = .data$spp_rich), by = "cell_id")
                          leg_title <- "Species richness"
                          metric_field <- "spp_rich"
                        } else {
                          metric_field <- switch(input$metric, n_occ = "n_occ", mean_abund = "mean_abund", sum_abund = "sum_abund")
                          if (input$metric_scope == "all") {
                            dat <- gd$grid |>
                              dplyr::inner_join(gd$all |> dplyr::transmute(cell_id, value = .data[[metric_field]]), by = "cell_id")
                            leg_title <- metric_field
                          } else {
                            req(input$metric_taxon)
                            sel_aphia  <- suppressWarnings(as.integer(input$metric_taxon))
                            sel_label  <- gd$by_taxon$label[match(sel_aphia, gd$by_taxon$aphia)][1]
                            tax_df <- gd$by_taxon |>
                              dplyr::filter(.data$aphia == sel_aphia) |>
                              dplyr::transmute(cell_id, value = .data[[metric_field]])
                            if (nrow(tax_df) == 0) {
                              showNotification("No per-taxon records found for this cell/metric.", type = "warning", duration = 4)
                              return()
                            }
                            dat <- gd$grid |> dplyr::inner_join(tax_df, by = "cell_id")
                            leg_title <- paste(metric_field, sel_label)
                          }
                        }
                        
                        if (nrow(dat) == 0) {
                          showNotification("No cells to display for current selection.", type = "message", duration = 4)
                          return()
                        }
                        
                        vals <- dat$value
                        vals_ok <- vals[is.finite(vals)]
                        if (length(vals_ok) == 0) {
                          showNotification("All metric values are NA/zero.", type = "message", duration = 4)
                          return()
                        }
                        
                        # popup: "Name (AphiaID:xxx, NCBI:yyy): n_occ — latest: YYYY-MM-DD"
                        species_summary <- gd$by_taxon |>
                          dplyr::filter(.data$cell_id %in% dat$cell_id) |>
                          dplyr::mutate(
                            label_full = paste0(
                              .data$label, ": ", .data$n_occ,
                              " — latest: ", ifelse(is.finite(.data$latest_date), as.character(.data$latest_date), "NA")
                            )
                          ) |>
                          dplyr::group_by(.data$cell_id) |>
                          dplyr::summarise(
                            popup_txt = paste0("<ul>", paste0("<li>", .data$label_full, "</li>", collapse = ""), "</ul>"),
                            .groups = "drop"
                          )
                        if (nrow(species_summary) == 0) {
                          species_summary <- tibble(cell_id = integer(), popup_txt = character())
                        }
                        dat <- dat |> dplyr::left_join(species_summary, by = "cell_id")
                        dat$popup_txt[is.na(dat$popup_txt)] <- "<ul><li>(no species detail)</li></ul>"
                        
                        popup <- paste0(
                          "<b>Cell:</b> ", dat$cell_id,
                          "<br/><b>", leg_title, ":</b> ", round(dat$value, 2),
                          "<br/><b>Species breakdown:</b>", dat$popup_txt
                        )
                        
                        make_bin_pal <- function(vals, mode = c("equal","quantile","custom"), cuts = c(0,5,10,20), n = 7) {
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
                        
                        pal <- make_bin_pal(vals, mode = input$bin_mode, cuts = parse_cuts(input$custom_cuts), n = input$n_bins)
                        
                        proxy |>
                          leaflet::addPolygons(
                            data = dat,
                            weight = 0.3, color = "#666666",
                            fillOpacity = ifelse(is.na(vals) | vals == 0, 0, 0.8),
                            fillColor   = ifelse(is.na(vals) | vals == 0, "transparent", pal(vals)),
                            popup = popup
                          ) |>
                          leaflet::addLegend("bottomright", pal = pal, values = vals, title = leg_title, opacity = 1)
                        
                        if (isTRUE(input$show_centroids)) {
                          cent <- sf::st_centroid(dat$geometry)
                          cent_xy <- sf::st_coordinates(cent)
                          proxy |> leaflet::addCircleMarkers(lng = cent_xy[,1], lat = cent_xy[,2],
                                                             radius = 2, stroke = FALSE, fillOpacity = 0.9, color = "#000000")
                        }
                      })
    
    # ---- Table --------------------------------------------------------------
    output$tbl <- DT::renderDT({
      gd <- grid_data()
      validate(need(!is.null(gd), "No records found."))
      
      if (nrow(gd$all) == 0) {
        return(DT::datatable(tibble::tibble(msg = "No records found."), rownames = FALSE))
      }
      
      dat <- gd$grid |> dplyr::inner_join(gd$all, by = "cell_id")
      if (!"geometry" %in% names(dat)) {
        return(DT::datatable(tibble::tibble(msg = "No geometry to tabulate."), rownames = FALSE))
      }
      
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
    
    # ---- Species Info (uses detected CSV column names) ----------------------
    species_info <- reactive({
      req(input$sp)
      if (length(input$sp) != 1) return(NULL)
      sp_txt <- input$sp
      
      aphia_used <- resolve_to_aphia(sp_txt)
      row_csv    <- match_species_row(sp_txt)
      
      aphia_csv <- if (nrow(row_csv) && !is.na(COL_APHIA) && COL_APHIA %in% names(row_csv))
        suppressWarnings(as.integer(row_csv[[COL_APHIA]])) else NA_integer_
      ncbi_csv  <- if (nrow(row_csv) && !is.na(COL_NCBI)  && COL_NCBI  %in% names(row_csv))
        suppressWarnings(as.integer(row_csv[[COL_NCBI ]])) else NA_integer_
      
      syns <- collapse_synonyms(row_csv)
      
      worms_rec <- tryCatch(if (!is.na(aphia_used)) worrms::wm_record(aphia_used) else NULL, error = function(e) NULL)
      checklist <- tryCatch(if (!is.na(aphia_used)) robis::checklist(taxonid = aphia_used) else NULL, error = function(e) NULL)
      
      yrange <- tryCatch({
        df <- get_obis_by_query(sp_txt, fields = c("eventDate"))
        if (!is.data.frame(df) || !"eventDate" %in% names(df) || nrow(df) == 0) "NA – NA" else {
          yrs <- suppressWarnings(lubridate::year(lubridate::ymd(df$eventDate)))
          yrs <- yrs[!is.na(yrs)]
          if (!length(yrs)) "NA – NA" else paste0(min(yrs), " – ", max(yrs))
        }
      }, error = function(e) "NA – NA")
      
      list(aphia_used = aphia_used, aphia_csv = aphia_csv, ncbi_csv = ncbi_csv,
           syns = syns, worms = worms_rec, checklist = checklist, yrange = yrange)
    })
    
    output$species_info <- renderUI({
      dat <- species_info()
      if (is.null(dat)) return(htmltools::HTML("<p>Select a single species to see details.</p>"))
      safe <- function(x, nm) if (!is.null(x) && nm %in% names(x)) as.character(x[[nm]][1]) else NA
      
      htmltools::tagList(
        htmltools::h4("Identifiers"),
        htmltools::HTML(paste0(
          "<b>AphiaID (used for OBIS):</b> ", ifelse(is.na(dat$aphia_used), "None", dat$aphia_used), "<br/>",
          "<b>AphiaID (from CSV row):</b> ",  ifelse(is.na(dat$aphia_csv),  "None", dat$aphia_csv),  "<br/>",
          "<b>NCBI Taxon ID (CSV):</b> ",     ifelse(is.na(dat$ncbi_csv),   "None", dat$ncbi_csv),   "<br/>",
          "<b>Synonyms (CSV):</b> ",          dat$syns
        )),
        htmltools::h4("Taxonomy"),
        htmltools::HTML(paste0(
          "<b>Scientific name:</b> ", safe(dat$worms, "scientificname"), "<br/>",
          "<b>Status:</b> ",          safe(dat$worms, "status"), "<br/>",
          "<b>Environment:</b> ",     safe(dat$worms, "environment")
        )),
        htmltools::h4("Checklist summary"),
        if (!is.null(dat$checklist) && nrow(dat$checklist) > 0) {
          htmltools::HTML(paste0(
            "<b>Total records:</b> ", safe(dat$checklist, "records"), "<br/>",
            "<b>Year range:</b> ",    dat$yrange, "<br/>",
            "<b>Phylum:</b> ",        safe(dat$checklist, "phylum"), "<br/>",
            "<b>Class:</b> ",         safe(dat$checklist, "class"), "<br/>",
            "<b>Order:</b> ",         safe(dat$checklist, "order"), "<br/>",
            "<b>Family:</b> ",        safe(dat$checklist, "family"), "<br/>",
            "<b>Genus:</b> ",         safe(dat$checklist, "genus")
          ))
        } else {
          htmltools::HTML("<p>No checklist summary available for this query.</p>")
        }
      )
    })
  })
}
