# ===== File: R/pages_id_search.R =====
# ID Search (module) — query WoRMS: AphiaRecordByExternalID/{ID}?type={type}
# Now also supports direct AphiaID lookups via the batch endpoint:
#   GET /AphiaRecordsByAphiaIDs?aphiaids[]=ID&aphiaids[]=...
# Multiple IDs (comma/whitespace-separated), memoised, with error handling.

requireNamespace("memoise")

IDSearchUI <- function(id) {
  ns <- NS(id)
  shiny::fluidPage(
    shiny::titlePanel("ID Search (WoRMS)"),
    htmltools::tags$style("
      .worms-card { border:1px solid #e5e7eb; border-radius:12px; padding:12px; }
      .worms-card h4 { margin:0 0 8px 0; font-size:16px; }
      .sci-name { color:#2563eb; font-size:20px; font-weight:600; }  /* BLUE + BIGGER */
      .worms-meta { color:#6b7280; font-weight:400; margin-left:6px; }
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
            "NCBI Taxonomy ID"     = "ncbi",
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
        shiny::actionButton(ns("go"), "Search", class = "btn-primary"),
      ),
      shiny::mainPanel(
        htmltools::div(
          style = "display:flex; align-items:center; gap:10px; margin-bottom:8px;",
          htmltools::span("Status:"),
          htmltools::strong(textOutput(ns("status"), inline = TRUE))
        ),
        uiOutput(ns("cards")),
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
    
    # ---------- helpers ----------
    display <- function(x) {
      if (is.null(x)) return("—")
      if (length(x) == 0) return("—")
      if (is.na(x)) return("—")
      sx <- as.character(x)
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
    
    # Split "1, 2  \n3" -> c("1","2","3")
    parse_ids_chr <- function(x) {
      if (is.null(x) || !nzchar(x)) return(character(0))
      ids <- unlist(strsplit(x, "[,\\s]+", perl = TRUE))
      ids <- stringr::str_squish(ids)
      ids[nzchar(ids)]
    }
    
    polite_headers <- function() {
      ua <- sprintf("OG-Explorer/Shiny (contact: your.email@uwa.edu.au)")
      c("Accept" = "application/json", "User-Agent" = ua)
    }
    
    # ---------- external-id -> AphiaRecord (single-per-call) ----------
    .worms_by_external_id <- function(ext_id, id_type) {
      base <- sprintf(
        "https://www.marinespecies.org/rest/AphiaRecordByExternalID/%s",
        utils::URLencode(ext_id, reserved = TRUE)
      )
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
    
    # ---------- aphiaids[] batch -> records (many-per-call) ----------
    fetch_aphia_batch <- function(aphia_ids_int) {
      # aphia_ids_int: integer vector length 1..50
      base <- "https://www.marinespecies.org/rest/AphiaRecordsByAphiaIDs"
      # Build query: aphiaids[]=ID&aphiaids[]=ID...
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
      if (code == 204) {
        # 204 for the whole batch is rare; treat as empty
        return(tibble::tibble(
          query_id = aphia_ids_int, type = "aphia", status_code = code,
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
      
      # If empty array, return "no match" rows for each query id
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
      
      # Ensure data.frame; then map rows
      if (is.data.frame(arr)) {
        df <- tibble::as_tibble(arr)
      } else if (is.list(arr)) {
        df <- tibble::as_tibble(arr)
      } else {
        df <- tibble::tibble()
      }
      
      # Normalize & select fields
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
      
      # For any requested IDs missing in response, append "no match" rows
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
      # Convert to integers & keep valid ones; track invalids as "error" rows
      ids_int <- suppressWarnings(as.integer(ids_chr))
      valid   <- ids_int[is.finite(ids_int)]
      invalid <- ids_chr[!is.finite(ids_int)]
      
      out <- tibble::tibble()
      if (length(valid)) {
        # Chunk by 50 (API limit)
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
    
    # ---------- Fetch on click ----------
    results_df <- eventReactive(input$go, {
      ids <- parse_ids_chr(input$ext_id)
      if (!length(ids)) return(tibble::tibble())
      
      if (identical(input$id_type, "aphia")) {
        fetch_aphia_records(ids)
      } else {
        # polite: small delay between calls (external-id endpoint is single-lookup)
        purrr::map_dfr(ids, function(one) {
          Sys.sleep(0.2)
          .worms_by_external_id(one, input$id_type)
        })
      }
    }, ignoreInit = TRUE)
    
    # ---------- Status text ----------
    output$status <- renderText({
      df <- results_df()
      if (is.null(df) || !nrow(df)) return("waiting for query…")
      ok   <- sum(is.finite(df$status_code) & df$status_code == 200, na.rm = TRUE)
      none <- sum(df$status_code == 204 | df$note %in% c("No match"), na.rm = TRUE)
      err  <- sum(!is.finite(df$status_code) | df$status_code >= 400, na.rm = TRUE)
      paste0(ok, " success, ", none, " no match, ", err, " error")
    })
    
    # ---------- Cards (two-column key/value) ----------
    output$cards <- renderUI({
      df <- results_df()
      if (is.null(df) || !nrow(df)) {
        return(htmltools::tags$em("Enter ID(s) and click Search."))
      }
      
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
        
        htmltools::tags$div(
          class = "worms-card",
          htmltools::tags$h4(
            htmltools::tags$span(class = "sci-name", display(row$scientificname)),
            if (!is.na(row$rank) && nzchar(row$rank)) htmltools::tags$span(class = "rank-badge", display(row$rank))
          ),
          htmltools::tags$div(class = "worms-badges", b_status, b_match),
          htmltools::tags$div(
            class = "kv-grid",
            kv("Query ID", row$query_id),
            kv("Type", row$type),
            kv("AphiaID", row$AphiaID),
            kv("Valid AphiaID", row$valid_AphiaID),
            kv("Valid name", row$valid_name),
            kv("Kingdom", row$kingdom),
            kv("Phylum", row$phylum),
            kv("Class", row$class),
            kv("Order", row$order),
            kv("Family", row$family),
            kv("Genus", row$genus),
            kv("isMarine", row$isMarine),
            kv("Note", row$note)
          ),
          if (!is.null(link_tag)) htmltools::tags$div(class = "worms-link", link_tag)
        )
      })
      
      htmltools::div(
        style = "display:grid; grid-template-columns: repeat(auto-fill, minmax(360px, 1fr)); gap:12px;",
        cards
      )
    })
    
    # ---------- CSV download ----------
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
