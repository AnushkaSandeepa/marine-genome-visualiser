# ===== File: R/pages_id_search.R =====

requireNamespace("memoise")

IDSearchUI <- function(id) {
  ns <- NS(id)
  shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::titlePanel("Search by Different IDs "),
    htmltools::tags$style("
      .worms-card { border:1px solid #e5e7eb; border-radius:12px; padding:12px; }
      .worms-card h4 { margin:0 0 8px 0; font-size:16px; }
      .sci-name { color:#2563eb; font-size:20px; font-weight:600; }
      .rank-badge { display:inline-block; padding:2px 8px; border-radius:999px; font-size:12px;
                    background:#6b72801a; color:#6b7280; border:1px solid #6b728033; margin-left:8px; }
      .kv-grid { display:grid; grid-template-columns: 180px 1fr; gap:8px 14px; align-items:start; }
      .kv-grid .k { font-weight:600; color:#374151; }
      .kv-grid .v { color:#111827; overflow-wrap:anywhere; }
      .worms-badges { display:flex; flex-wrap:wrap; gap:6px; margin-bottom:8px; }
      .badge { display:inline-block; padding:2px 8px; border-radius:999px; font-size:12px; border:1px solid transparent; }
      .badge.ok { background:#16a34a1a; color:#16a34a; border-color:#16a34a33; }
      .badge.err{ background:#dc26261a; color:#dc2626; border-color:#dc262633; }
      .badge.none{ background:#6b72801a; color:#6b7280; border-color:#6b728033; }
      .badge.blue{ background:#2563eb1a; color:#2563eb; border-color:#2563eb33; }
      .worms-link { margin-top:8px; }
      .extra-h { font-weight:700; margin-top:10px; font-size:14px; color:#374151; }
      .muted { color:#6b7280; font-size:12px; }
    "),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(
          ns("id_type"), "ID type",
          choices = c(
            "AphiaID"              = "aphia",
            "NCBI Taxonomy ID"     = "ncbi",
            "AlgaeBase species ID" = "algaebase",
            "BOLD TaxID"           = "bold",
            "Dyntaxa ID"           = "dyntaxa",
            "FishBase species ID"  = "fishbase",
            "IUCN Red List ID"     = "iucn",
            "LSID"                 = "lsid",
            "ITIS TSN"             = "tsn",
            "GISD ID"              = "gisd"
          ),
          selected = "aphia"
        ),
        shiny::textInput(
          ns("ext_id"),
          "ID(s) (comma or space separated)",
          placeholder = "e.g. AphiaID: 275731  (or multiple: 275731, 141433, 1080)"
        ),
        shiny::actionButton(ns("go"), "Search", class = "btn-primary")
      ),
      shiny::mainPanel(
        htmltools::div(
          style = "display:flex; align-items:center; gap:10px; margin-bottom:8px;",
          htmltools::span("Status:"),
          htmltools::strong(textOutput(ns("status"), inline = TRUE))
        ),
        shinycssloaders::withSpinner(
          uiOutput(ns("cards")),
          type = 6, color = "#2563eb", size = 0.75
        ),
        htmltools::br(),
        shiny::downloadButton(ns("dl_csv"), "Download results CSV")
      )
    )
  )
}

IDSearchServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    requireNamespace("httr")
    requireNamespace("jsonlite")
    requireNamespace("dplyr")
    requireNamespace("tibble")
    requireNamespace("purrr")
    requireNamespace("stringr")
    requireNamespace("readr")
    requireNamespace("memoise")
    requireNamespace("shinyjs")
    requireNamespace("tidyr")
    
    # ---------------- Extra CSV handling (auto-load only; upload removed) ----------------
    # EXCLUDES: rank, order, Genus (to avoid overriding WoRMS)
    EXTRA_KEEP <- c(
      "aphia_id","ncbi_taxon_id","species_canonical","all_synonyms","publication_id",
      "family_x","subspecies","authority","modified","status",
      "target_list_status","sequencing_status","isMarine","isBrackish","isFreshwater",
      "isTerrestrial","isExtinct","spec_code","FBname","DemersPelag","Subfamily",
      "depth_min_in_m","depth_max_in_m","common_depth_min","common_depth_max",
      "length_max_in_cm","common_length_in_cm","weight_max_in_g","TempMin","TempMax"
    )
    BINARY_COLS <- c("ismarine","isbrackish","isfreshwater","isterrestrial")
    # Do not display these fields in the UI
    DISPLAY_EXCLUDE <- c("ismarine", "isbrackish", "isfreshwater", "isterrestrial")
    
    normalize_binaries <- function(df) {
      if (is.null(df) || !nrow(df)) return(df)
      nm <- intersect(BINARY_COLS, names(df))
      if (!length(nm)) return(df)
      map_to_int01 <- function(x) {
        if (is.logical(x)) return(ifelse(x, 1L, 0L))
        sx <- trimws(as.character(x))
        sx_low <- tolower(sx)
        out <- rep(NA_integer_, length(sx))
        out[sx %in% c("1","1.0")] <- 1L
        out[sx %in% c("0","0.0","")] <- 0L
        out[sx_low %in% c("y","yes","t","true")] <- 1L
        out[sx_low %in% c("n","no","f","false")] <- 0L
        suppressWarnings({
          num <- as.numeric(sx)
          out[is.na(out) & !is.na(num) & num >= 0.5] <- 1L
          out[is.na(out) & !is.na(num) & num <  0.5] <- 0L
        })
        out
      }
      df |>
        dplyr::mutate(dplyr::across(all_of(nm), map_to_int01))
    }
    
    # convert any value to a safe character key; strip trailing .0; "" and "NA" -> NA
    str_key <- function(x) {
      sx <- trimws(as.character(x))
      sx <- sub("\\.0+$", "", sx, perl = TRUE)
      sx[nchar(sx) == 0 | toupper(sx) == "NA"] <- NA_character_
      sx
    }
    
    clean_extra <- function(df) {
      if (is.null(df) || !nrow(df)) return(df)
      # 1) Standardize column names
      names(df) <- names(df) |>
        trimws() |>
        gsub("\\s+", "_", ., perl = TRUE) |>
        tolower()
      # 2) Trim all character cells and convert "-" / "NA" to NA
      chr_cols <- names(df)[vapply(df, is.character, logical(1))]
      if (length(chr_cols)) {
        df[chr_cols] <- lapply(df[chr_cols], function(x) {
          z <- trimws(x)
          z[z == "-" | toupper(z) == "NA"] <- NA_character_
          z
        })
      }
      # 3) Keep only the columns we want
      df <- df |> dplyr::select(any_of(tolower(EXTRA_KEEP)))
      # 4) Normalize binaries robustly
      df <- normalize_binaries(df)
      df
    }
    
    extra_df <- shiny::reactiveVal(NULL)
    
    auto_load_extra <- function() {
      paths <- c("CSV/new_final_species.csv", "www/new_final_species.csv", "new_final_species.csv")
      p <- paths[file.exists(paths)]
      if (length(p)) {
        try({
          df <- readr::read_csv(p[1], show_col_types = FALSE, progress = FALSE)
          extra_df(clean_extra(df))
        }, silent = TRUE)
      }
    }
    auto_load_extra()
    
    # ---------------- UI helpers ----------------
    display <- function(x) {
      if (is.null(x) || length(x) == 0) return("—")
      if (all(is.na(x))) return("—")
      sx <- as.character(x)[1]
      if (!nzchar(sx)) return("—")
      sx
    }
    kv <- function(label, value) {
      htmltools::tags$div(
        htmltools::tags$div(class = "k", label),
        htmltools::tags$div(class = "v", display(value))
      )
    }
    badge <- function(text, cls) htmltools::tags$span(class = paste("badge", cls), text)
    yn_badge <- function(x) {
      if (is.null(x) || is.na(x)) return("—")
      if (as.integer(x) == 1L) htmltools::tags$span(class="badge ok",  "Yes")
      else                      htmltools::tags$span(class="badge none","No")
    }
    kv_bool <- function(label, value) {
      htmltools::tags$div(
        htmltools::tags$div(class="k", label),
        htmltools::tags$div(class="v", yn_badge(value))
      )
    }
    # CSV or WoRMS fallback for a boolean-like field (kept for future use)
    val_of <- function(row, nm_lower, nm_camel = NULL) {
      v1 <- if (nm_lower  %in% names(row)) suppressWarnings(as.integer(row[[nm_lower]][[1]])) else NA_integer_
      v2 <- if (!is.null(nm_camel) && nm_camel %in% names(row)) suppressWarnings(as.integer(row[[nm_camel]][[1]])) else NA_integer_
      if (!is.na(v1)) v1 else v2
    }
    
    parse_ids_chr <- function(x) {
      if (is.null(x) || !nzchar(x)) return(character(0))
      ids <- unlist(strsplit(x, "[,\\s]+", perl = TRUE))
      ids <- stringr::str_squish(ids)
      ids <- ids[nzchar(ids)]
      unique(ids)
    }
    
    polite_headers <- function() {
      ua <- sprintf("OG-Explorer/Shiny (contact: your.email@uwa.edu.au)")
      c("Accept" = "application/json", "User-Agent" = ua)
    }
    
    # ---------------- WoRMS lookups ----------------
    .worms_by_external_id <- function(ext_id, id_type) {
      base <- sprintf("https://www.marinespecies.org/rest/AphiaRecordByExternalID/%s",
                      utils::URLencode(ext_id, reserved = TRUE))
      url <- paste0(base, "?type=", utils::URLencode(id_type, reserved = TRUE))
      resp <- tryCatch(
        httr::RETRY("GET", url,
                    httr::add_headers(.headers = polite_headers()),
                    httr::timeout(15),
                    pause_min = 0.5, pause_cap = 5, times = 3
        ),
        error = function(e) e
      )
      if (inherits(resp, "error")) {
        return(tibble::tibble(
          query_id = ext_id, type = id_type, status_code = NA_integer_,
          AphiaID = NA_integer_, scientificname = NA_character_, authority = NA_character_,
          status = NA_character_, rank = NA_character_, valid_AphiaID = NA_integer_,
          valid_name = NA_character_, kingdom = NA_character_, phylum = NA_character_,
          class = NA_character_, order = NA_character_, family = NA_character_,
          genus = NA_character_, isMarine = NA_integer_, url = NA_character_,
          match_type = NA_character_, note = paste0("Request error: ", conditionMessage(resp))
        ))
      }
      code <- httr::status_code(resp)
      if (code == 204) {
        return(tibble::tibble(
          query_id = ext_id, type = id_type, status_code = code,
          AphiaID = NA_integer_, scientificname = NA_character_, authority = NA_character_,
          status = NA_character_, rank = NA_character_, valid_AphiaID = NA_integer_,
          valid_name = NA_character_, kingdom = NA_character_, phylum = NA_character_,
          class = NA_character_, order = NA_character_, family = NA_character_,
          genus = NA_character_, isMarine = NA_integer_, url = NA_character_,
          match_type = NA_character_, note = "No match (204)"
        ))
      }
      if (code >= 400) {
        return(tibble::tibble(
          query_id = ext_id, type = id_type, status_code = code,
          AphiaID = NA_integer_, scientificname = NA_character_, authority = NA_character_,
          status = NA_character_, rank = NA_character_, valid_AphiaID = NA_integer_,
          valid_name = NA_character_, kingdom = NA_character_, phylum = NA_character_,
          class = NA_character_, order = NA_character_, family = NA_character_,
          genus = NA_character_, isMarine = NA_integer_, url = NA_character_,
          match_type = NA_character_, note = paste0("HTTP ", code)
        ))
      }
      txt <- httr::content(resp, as = "text", encoding = "UTF-8")
      js  <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
      if (is.null(js) || length(js) == 0) {
        return(tibble::tibble(
          query_id = ext_id, type = id_type, status_code = code,
          AphiaID = NA_integer_, scientificname = NA_character_, authority = NA_character_,
          status = NA_character_, rank = NA_character_, valid_AphiaID = NA_integer_,
          valid_name = NA_character_, kingdom = NA_character_, phylum = NA_character_,
          class = NA_character_, order = NA_character_, family = NA_character_,
          genus = NA_character_, isMarine = NA_integer_, url = NA_character_,
          match_type = NA_character_, note = "Empty response"
        ))
      }
      pick <- function(nm) if (!is.null(js[[nm]])) js[[nm]] else NA
      tibble::tibble(
        query_id       = ext_id,
        type           = id_type,
        status_code    = code,
        AphiaID        = suppressWarnings(as.integer(pick("AphiaID"))),
        scientificname = as.character(pick("scientificname")),
        authority      = as.character(pick("authority")),
        status         = as.character(pick("status")),
        rank           = as.character(pick("rank")),
        valid_AphiaID  = suppressWarnings(as.integer(pick("valid_AphiaID"))),
        valid_name     = as.character(pick("valid_name")),
        kingdom        = as.character(pick("kingdom")),
        phylum         = as.character(pick("phylum")),
        class          = as.character(pick("class")),
        order          = as.character(pick("order")),
        family         = as.character(pick("family")),
        genus          = as.character(pick("genus")),
        isMarine       = suppressWarnings(as.integer(pick("isMarine"))),
        url            = as.character(pick("url")),
        match_type     = as.character(pick("match_type")),
        note           = NA_character_
      )
    }
    worms_by_external_id <- memoise::memoise(.worms_by_external_id)
    
    # ---- aphiaids[] batch -> records ----
    fetch_aphia_batch <- function(aphia_ids_int) {
      base <- "https://www.marinespecies.org/rest/AphiaRecordsByAphiaIDs"
      q <- paste0("aphiaids%5B%5D=", utils::URLencode(as.character(aphia_ids_int), reserved = TRUE))
      url <- paste0(base, "?", paste(q, collapse = "&"))
      resp <- tryCatch(
        httr::RETRY("GET", url,
                    httr::add_headers(.headers = polite_headers()),
                    httr::timeout(20),
                    pause_min = 0.5, pause_cap = 5, times = 3
        ),
        error = function(e) e
      )
      if (inherits(resp, "error")) {
        return(tibble::tibble(
          query_id = aphia_ids_int, type = "aphia", status_code = NA_integer_,
          AphiaID = NA_integer_, scientificname = NA_character_, authority = NA_character_,
          status = NA_character_, rank = NA_character_, valid_AphiaID = NA_integer_,
          valid_name = NA_character_, kingdom = NA_character_, phylum = NA_character_,
          class = NA_character_, order = NA_character_, family = NA_character_,
          genus = NA_character_, isMarine = NA_integer_, url = NA_character_,
          match_type = NA_character_, note = paste0("Request error: ", conditionMessage(resp))
        ))
      }
      code <- httr::status_code(resp)
      if (code >= 400) {
        return(tibble::tibble(
          query_id = aphia_ids_int, type = "aphia", status_code = code,
          AphiaID = NA_integer_, scientificname = NA_character_, authority = NA_character_,
          status = NA_character_, rank = NA_character_, valid_AphiaID = NA_integer_,
          valid_name = NA_character_, kingdom = NA_character_, phylum = NA_character_,
          class = NA_character_, order = NA_character_, family = NA_character_,
          genus = NA_character_, isMarine = NA_integer_, url = NA_character_,
          match_type = NA_character_, note = paste0("HTTP ", code)
        ))
      }
      txt <- httr::content(resp, as = "text", encoding = "UTF-8")
      arr <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
      if (is.null(arr) || length(arr) == 0) {
        return(tibble::tibble(
          query_id = aphia_ids_int, type = "aphia", status_code = code,
          AphiaID = NA_integer_, scientificname = NA_character_, authority = NA_character_,
          status = NA_character_, rank = NA_character_, valid_AphiaID = NA_integer_,
          valid_name = NA_character_, kingdom = NA_character_, phylum = NA_character_,
          class = NA_character_, order = NA_character_, family = NA_character_,
          genus = NA_character_, isMarine = NA_integer_, url = NA_character_,
          match_type = NA_character_, note = "No match"
        ))
      }
      df <- tibble::as_tibble(arr)
      out <- df |>
        dplyr::transmute(
          query_id       = suppressWarnings(as.integer(.data$AphiaID)),
          type           = "aphia",
          status_code    = code,
          AphiaID        = suppressWarnings(as.integer(.data$AphiaID)),
          scientificname = as.character(.data$scientificname),
          authority      = as.character(.data$authority),
          status         = as.character(.data$status),
          rank           = as.character(.data$rank),
          valid_AphiaID  = suppressWarnings(as.integer(.data$valid_AphiaID)),
          valid_name     = as.character(.data$valid_name),
          kingdom        = as.character(.data$kingdom),
          phylum         = as.character(.data$phylum),
          class          = as.character(.data$class),
          order          = as.character(.data$order),
          family         = as.character(.data$family),
          genus          = as.character(.data$genus),
          isMarine       = suppressWarnings(as.integer(.data$isMarine)),
          url            = as.character(.data$url),
          match_type     = as.character(.data$match_type),
          note           = NA_character_
        )
      missing_ids <- setdiff(aphia_ids_int, out$query_id)
      if (length(missing_ids)) {
        out <- dplyr::bind_rows(
          out,
          tibble::tibble(
            query_id = missing_ids, type = "aphia", status_code = code,
            AphiaID = NA_integer_, scientificname = NA_character_, authority = NA_character_,
            status = NA_character_, rank = NA_character_, valid_AphiaID = NA_integer_,
            valid_name = NA_character_, kingdom = NA_character_, phylum = NA_character_,
            class = NA_character_, order = NA_character_, family = NA_character_,
            genus = NA_character_, isMarine = NA_integer_, url = NA_character_,
            match_type = NA_character_, note = "No match"
          )
        )
      }
      out
    }
    
    fetch_aphia_records <- function(ids_chr) {
      ids_int <- suppressWarnings(as.integer(ids_chr))
      valid   <- ids_int[is.finite(ids_int)]
      invalid <- ids_chr[!is.finite(ids_int)]
      out <- tibble::tibble()
      if (length(valid)) {
        chunks <- split(valid, ceiling(seq_along(valid)/50))
        parts <- lapply(chunks, fetch_aphia_batch)
        out <- dplyr::bind_rows(parts)
      }
      if (length(invalid)) {
        out <- dplyr::bind_rows(
          out,
          tibble::tibble(
            query_id = invalid, type = "aphia", status_code = NA_integer_,
            AphiaID = NA_integer_, scientificname = NA_character_, authority = NA_character_,
            status = NA_character_, rank = NA_character_, valid_AphiaID = NA_integer_,
            valid_name = NA_character_, kingdom = NA_character_, phylum = NA_character_,
            class = NA_character_, order = NA_character_, family = NA_character_,
            genus = NA_character_, isMarine = NA_integer_, url = NA_character_,
            match_type = NA_character_, note = "Invalid AphiaID"
          )
        )
      }
      out
    }
    
    # ---------------- Fetch on click ----------------
    results_df_raw <- eventReactive(input$go, {
      ids <- parse_ids_chr(input$ext_id)
      if (!length(ids)) return(tibble::tibble())
      
      if (identical(input$id_type, "aphia")) {
        fetch_aphia_records(ids)
      } else {
        purrr::map_dfr(ids, function(one) {
          Sys.sleep(0.2)
          .worms_by_external_id(one, input$id_type)
        })
      }
    }, ignoreInit = TRUE)
    
    # ---------------- Enrichment with Extra CSV (auto-loaded only) ----------------
    enrich_with_extra <- function(df, extra) {
      if (is.null(extra) || !nrow(extra) || !nrow(df)) return(df)
      
      extra <- extra |> dplyr::rename_with(tolower)
      if (!"aphia_id" %in% names(extra)) extra$aphia_id <- NA
      if (!"ncbi_taxon_id" %in% names(extra)) extra$ncbi_taxon_id <- NA
      
      extra <- extra |>
        dplyr::mutate(
          aphia_id      = str_key(.data$aphia_id),
          ncbi_taxon_id = str_key(.data$ncbi_taxon_id)
        )
      
      df2 <- df |>
        dplyr::mutate(
          aphia_join = dplyr::coalesce(.data$AphiaID, .data$valid_AphiaID),
          ncbi_join  = dplyr::if_else(.data$type == "ncbi",
                                      suppressWarnings(as.integer(.data$query_id)),
                                      as.integer(NA))
        ) |>
        dplyr::mutate(
          aphia_key = str_key(.data$aphia_join),
          ncbi_key  = str_key(.data$ncbi_join)
        )
      
      # 1) Join by Aphia (character key)
      out <- df2 |>
        dplyr::left_join(extra, by = c("aphia_key" = "aphia_id"), multiple = "first")
      
      # 2) Join by NCBI and coalesce any still-missing extra columns
      if ("ncbi_taxon_id" %in% names(extra)) {
        out2 <- df2 |>
          dplyr::left_join(extra, by = c("ncbi_key" = "ncbi_taxon_id"),
                           suffix = c("", ".ncbi"), multiple = "first")
        
        fill_cols <- setdiff(intersect(names(extra), names(out)), c("aphia_id","ncbi_taxon_id"))
        for (nm in fill_cols) {
          nm_ncbi <- paste0(nm, ".ncbi")
          if (nm_ncbi %in% names(out2)) {
            out[[nm]] <- dplyr::coalesce(out[[nm]], out2[[nm_ncbi]])
          }
        }
      }
      
      # Re-normalize binaries after coalesce
      out <- normalize_binaries(out)
      out
    }
    
    results_df <- shiny::reactive({
      enrich_with_extra(results_df_raw(), extra_df())
    })
    
    # --- Disable/enable Search button while loading ---
    observeEvent(input$go, {
      shinyjs::disable("go")
      shiny::updateActionButton(session, "go", label = "Downloading…")
    })
    observeEvent(results_df(), {
      shinyjs::enable("go")
      shiny::updateActionButton(session, "go", label = "Search")
    })
    
    # ---------------- Status text ----------------
    output$status <- renderText({
      df <- results_df_raw()
      if (is.null(df) || !nrow(df)) return("waiting for query…")
      ok   <- sum(is.finite(df$status_code) & df$status_code == 200, na.rm = TRUE)
      none <- sum(df$status_code == 204 | df$note %in% c("No match"), na.rm = TRUE)
      err  <- sum(!is.finite(df$status_code) | df$status_code >= 400, na.rm = TRUE)
      paste0(ok, " success, ", none, " no match, ", err, " error")
    })
    
    # ---------------- Label prettifier (lowercase keys) ----------------
    pretty_label <- function(nm) {
      switch(nm,
             "species_canonical"   = "Species (canonical)",
             "all_synonyms"        = "All synonyms",
             "publication_id"      = "Publication ID",
             "family_x"            = "Family (source)",
             "subspecies"          = "Subspecies",
             "authority"           = "Authority",
             "modified"            = "Modified",
             "status"              = "Status",
             "target_list_status"  = "Target list status",
             "sequencing_status"   = "Sequencing status",
             "ismarine"            = "isMarine",
             "isbrackish"          = "isBrackish",
             "isfreshwater"        = "isFreshwater",
             "isterrestrial"       = "isTerrestrial",
             "isextinct"           = "isExtinct",
             "spec_code"           = "Spec code",
             "fbname"              = "FishBase name",
             "demerspelag"         = "Demersal/Pelagic",
             "subfamily"           = "Subfamily",
             "depth_min_in_m"      = "Depth min (m)",
             "depth_max_in_m"      = "Depth max (m)",
             "common_depth_min"    = "Common depth min",
             "common_depth_max"    = "Common depth max",
             "length_max_in_cm"    = "Length max (cm)",
             "common_length_in_cm" = "Common length (cm)",
             "weight_max_in_g"     = "Weight max (g)",
             "tempmin"             = "Temp min (°C)",
             "tempmax"             = "Temp max (°C)",
             nm
      )
    }
    
    # ---------------- Cards ----------------
    output$cards <- renderUI({
      df <- results_df()
      if (is.null(df) || !nrow(df)) {
        return(htmltools::tags$em("Enter ID(s) and click Search."))
      }
      
      # Ensure lower-case extra names exist even if not loaded
      have_extra <- tolower(EXTRA_KEEP)
      for (nm in setdiff(have_extra, names(df))) df[[nm]] <- NA
      
      cards <- lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, , drop = FALSE]
        
        b_status <- if (isTRUE(row$status_code == 200)) badge("200 OK", "ok") else
          if (isTRUE(row$status_code == 204) || display(row$note) == "No match") badge("No match", "none") else
            if (isTRUE(is.na(row$status_code) || row$status_code >= 400)) badge(display(row$status_code), "err") else NULL
        b_match  <- if (!is.na(row$match_type) && nzchar(row$match_type)) badge(toupper(row$match_type), "blue") else NULL
        
        link_tag <- NULL
        if (!is.na(row$url) && nzchar(row$url)) {
          link_tag <- htmltools::tags$a("Open in WoRMS", href = row$url, target = "_blank", rel = "noopener")
        } else if (!is.na(row$AphiaID)) {
          link_tag <- htmltools::tags$a(
            "Open in WoRMS",
            href = paste0("https://www.marinespecies.org/aphia.php?p=taxdetails&id=", row$AphiaID),
            target = "_blank", rel = "noopener"
          )
        }
        
        # MAIN GRID: merged 'Query' line + WoRMS attrs (HIDE the four flags)
        main_grid <- htmltools::tags$div(
          class = "kv-grid",
          kv("Query", paste0(display(row$type), " - ", display(row$query_id))),
          kv("AphiaID", row$AphiaID),
          kv("Valid AphiaID", row$valid_AphiaID),
          kv("Valid name", row$valid_name),
          kv("Kingdom", row$kingdom),
          kv("Phylum", row$phylum),
          kv("Class", row$class),
          kv("Order", row$order),
          kv("Family", row$family),
          kv("Genus", row$genus),
          kv("Note", row$note)
        )
        
        # EXTRA ATTRIBUTES (skip aphia_id/ncbi_taxon_id and the four flags)
        extra_pairs <- list()
        for (nm in setdiff(tolower(EXTRA_KEEP), c("aphia_id","ncbi_taxon_id"))) {
          if (nm %in% DISPLAY_EXCLUDE) next  # hide ismarine/isbrackish/isfreshwater/isterrestrial
          if (nm %in% names(row)) {
            val <- row[[nm]][[1]]
            if (!is.null(val) && !is.na(val) && nzchar(as.character(val))) {
              if (nm %in% BINARY_COLS) {
                extra_pairs[[length(extra_pairs)+1]] <- kv_bool(pretty_label(nm), as.integer(val))
              } else {
                extra_pairs[[length(extra_pairs)+1]] <- kv(pretty_label(nm), val)
              }
            }
          }
        }
        
        htmltools::tags$div(
          class = "worms-card",
          htmltools::tags$h4(
            htmltools::tags$span(class = "sci-name", display(row$scientificname)),
            if (!is.na(row$rank) && nzchar(row$rank)) htmltools::tags$span(class = "rank-badge", display(row$rank))
          ),
          htmltools::tags$div(class = "worms-badges", b_status, b_match),
          main_grid,
          if (length(extra_pairs)) {
            htmltools::tagList(
              htmltools::div(class="extra-h", "Extra attributes"),
              htmltools::div(class="kv-grid", extra_pairs)
            )
          },
          if (!is.null(link_tag)) htmltools::tags$div(class = "worms-link", link_tag)
        )
      })
      
      htmltools::div(
        style = "display:grid; grid-template-columns: repeat(auto-fill, minmax(360px, 1fr)); gap:12px;",
        cards
      )
    })
    
    # ---------------- CSV download ----------------
    output$dl_csv <- downloadHandler(
      filename = function() {
        paste0("worms_lookup_", gsub("[^A-Za-z0-9]+","_", input$id_type), ".csv")
      },
      content = function(file) {
        df <- results_df()
        if (is.null(df) || !nrow(df)) df <- tibble::tibble()
        readr::write_csv(df, file, na = "")
      }
    )
  })
}
