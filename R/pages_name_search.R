# ===== File: R/pages_name_search.R =====
ID <- NULL # appease R CMD check for NSE

requireNamespace("memoise")

NameSearchUI <- function(id) {
  ns <- NS(id)
  shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::titlePanel("Search by Scientific Name"),
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
      .badge.none{ background:#6b72801a; color:#6b7280; border-color:#6b728033; }
      .badge.err{ background:#dc26261a; color:#dc2626; border-color:#dc262633; }
      .badge.blue{ background:#2563eb1a; color:#2563eb; border-color:#2563eb33; }
      .worms-link { margin-top:8px; }
    "),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textInput(
          ns("name"),
          "Scientific name (exact or prefix)",
          placeholder = "e.g. Antigonia hulleyi  or  Antigonia"
        ),
        shiny::checkboxInput(ns("like"), "Use prefix match (LIKE: adds '%')", value = TRUE),
        shiny::checkboxInput(ns("marine_only"), "Marine taxa only", value = TRUE),
        shiny::checkboxInput(ns("extant_only"), "Extant taxa only", value = TRUE),
        shiny::numericInput(ns("offset"), "Offset (for next 50)", value = 1, min = 1, step = 50),
        shiny::div(
          style = "display:flex; gap:8px; align-items:center;",
          shiny::actionButton(ns("go"), "Search", class = "btn-primary"),
          shiny::actionButton(ns("prev"), "Prev 50"),
          shiny::actionButton(ns("next"), "Next 50")
        ),
        shiny::helpText("The API returns up to 50 records per request.")
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

NameSearchServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    requireNamespace("httr")
    requireNamespace("jsonlite")
    requireNamespace("dplyr")
    requireNamespace("tibble")
    requireNamespace("purrr")
    requireNamespace("stringr")
    requireNamespace("readr")
    requireNamespace("shinyjs")
    
    display <- function(x) {
      if (is.null(x) || length(x) == 0 || is.na(x)) return("—")
      sx <- as.character(x); if (!nzchar(sx)) return("—"); sx
    }
    kv <- function(label, value) {
      htmltools::tags$div(
        htmltools::tags$div(class = "k", label),
        htmltools::tags$div(class = "v", display(value))
      )
    }
    badge <- function(text, cls) htmltools::tags$span(class = paste("badge", cls), text)
    
    polite_headers <- function() {
      ua <- sprintf("OG-Explorer/Shiny (contact: your.email@uwa.edu.au)")
      c("Accept" = "application/json", "User-Agent" = ua)
    }
    
    .fetch_by_name <- function(name, like = TRUE, marine_only = TRUE, extant_only = TRUE, offset = 1) {
      base <- sprintf(
        "https://www.marinespecies.org/rest/AphiaRecordsByName/%s",
        utils::URLencode(name, reserved = TRUE)
      )
      q <- list(
        like = tolower(as.character(isTRUE(like))),
        marine_only = tolower(as.character(isTRUE(marine_only))),
        extant_only = tolower(as.character(isTRUE(extant_only))),
        offset = as.integer(offset %||% 1L)
      )
      url <- paste0(
        base, "?", paste0(names(q), "=", utils::URLencode(as.character(q), reserved = TRUE), collapse = "&")
      )
      
      resp <- tryCatch(
        httr::RETRY("GET", url,
                    httr::add_headers(.headers = polite_headers()),
                    httr::timeout(20),
                    pause_min = 0.5, pause_cap = 5, times = 3),
        error = function(e) e
      )
      if (inherits(resp, "error")) {
        return(tibble::tibble(
          status_code = NA_integer_, query = name,
          AphiaID = NA_integer_, scientificname = NA_character_, authority = NA_character_,
          status = NA_character_, rank = NA_character_, valid_AphiaID = NA_integer_,
          valid_name = NA_character_, kingdom = NA_character_, phylum = NA_character_,
          class = NA_character_, order = NA_character_, family = NA_character_, genus = NA_character_,
          isMarine = NA_integer_, url = NA_character_, match_type = NA_character_, modified = NA_character_,
          note = paste0("Request error: ", conditionMessage(resp))
        ))
      }
      code <- httr::status_code(resp)
      if (code == 204) {
        return(tibble::tibble(
          status_code = code, query = name,
          AphiaID = NA_integer_, scientificname = NA_character_, authority = NA_character_,
          status = NA_character_, rank = NA_character_, valid_AphiaID = NA_integer_,
          valid_name = NA_character_, kingdom = NA_character_, phylum = NA_character_,
          class = NA_character_, order = NA_character_, family = NA_character_, genus = NA_character_,
          isMarine = NA_integer_, url = NA_character_, match_type = NA_character_, modified = NA_character_,
          note = "No match (204)"
        ))
      }
      if (code >= 400) {
        return(tibble::tibble(
          status_code = code, query = name,
          AphiaID = NA_integer_, scientificname = NA_character_, authority = NA_character_,
          status = NA_character_, rank = NA_character_, valid_AphiaID = NA_integer_,
          valid_name = NA_character_, kingdom = NA_character_, phylum = NA_character_,
          class = NA_character_, order = NA_character_, family = NA_character_, genus = NA_character_,
          isMarine = NA_integer_, url = NA_character_, match_type = NA_character_, modified = NA_character_,
          note = paste0("HTTP ", code)
        ))
      }
      txt <- httr::content(resp, as = "text", encoding = "UTF-8")
      arr <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
      
      if (is.null(arr) || length(arr) == 0) {
        return(tibble::tibble(
          status_code = code, query = name,
          AphiaID = NA_integer_, scientificname = NA_character_, authority = NA_character_,
          status = NA_character_, rank = NA_character_, valid_AphiaID = NA_integer_,
          valid_name = NA_character_, kingdom = NA_character_, phylum = NA_character_,
          class = NA_character_, order = NA_character_, family = NA_character_, genus = NA_character_,
          isMarine = NA_integer_, url = NA_character_, match_type = NA_character_, modified = NA_character_,
          note = "No match"
        ))
      }
      
      df <- tibble::as_tibble(arr) |>
        dplyr::transmute(
          status_code    = code,
          query          = name,
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
          modified       = as.character(.data$modified),
          note           = NA_character_
        ) |>
        dplyr::distinct(.data$AphiaID, .keep_all = TRUE)  # avoid duplicates
      df
    }
    fetch_by_name <- memoise::memoise(.fetch_by_name)
    
    # Pager buttons update offset
    observeEvent(input$`next`, {
  val <- max(1L, as.integer(input$offset) + 50L)
  updateNumericInput(session, "offset", value = val)
})

    
    
    # Fetch (debounced on name to avoid accidental double calls)
    results_df <- eventReactive(input$go, {
      nm <- stringr::str_squish(input$name %||% "")
      if (!nzchar(nm)) return(tibble::tibble())
      fetch_by_name(
        nm,
        like        = isTRUE(input$like),
        marine_only = isTRUE(input$marine_only),
        extant_only = isTRUE(input$extant_only),
        offset      = as.integer(input$offset %||% 1L)
      )
    }, ignoreInit = TRUE)
    
    # Disable button while loading
    observeEvent(input$go, {
      shinyjs::disable("go")
      updateActionButton(session, "go", label = "Searching…")
    })
    observeEvent(results_df(), {
      shinyjs::enable("go")
      updateActionButton(session, "go", label = "Search")
    })
    
    # Status
    output$status <- renderText({
      df <- results_df()
      if (!nrow(df)) return("waiting for query…")
      ok   <- sum(df$status_code == 200, na.rm = TRUE)
      none <- sum(df$note %in% c("No match", "No match (204)"), na.rm = TRUE)
      err  <- sum(!is.finite(df$status_code) | df$status_code >= 400, na.rm = TRUE)
      paste0(ok, " success, ", none, " no match, ", err, " error")
    })
    
    # Cards
    output$cards <- renderUI({
      df <- results_df()
      if (!nrow(df)) return(htmltools::tags$em("Enter a name and click Search."))
      
      cards <- lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, , drop = FALSE]
        b_status <- if (isTRUE(row$status_code == 200)) badge("200 OK", "ok") else
          if (display(row$note) %in% c("No match", "No match (204)")) badge("No match", "none") else
            if (isTRUE(is.na(row$status_code) || row$status_code >= 400)) badge(display(row$status_code), "err") else NULL
        b_match  <- if (!is.na(row$match_type) && nzchar(row$match_type)) badge(toupper(row$match_type), "blue") else NULL
        
        link_tag <- if (!is.na(row$url) && nzchar(row$url)) {
          htmltools::tags$a("Open in WoRMS", href = row$url, target = "_blank", rel = "noopener")
        } else if (!is.na(row$AphiaID)) {
          htmltools::tags$a(
            "Open in WoRMS",
            href = paste0("https://www.marinespecies.org/aphia.php?p=taxdetails&id=", row$AphiaID),
            target = "_blank", rel = "noopener"
          )
        } else NULL
        
        htmltools::tags$div(
          class = "worms-card",
          htmltools::tags$h4(
            htmltools::tags$span(class = "sci-name", display(row$scientificname)),
            if (!is.na(row$rank) && nzchar(row$rank)) htmltools::tags$span(class = "rank-badge", display(row$rank))
          ),
          htmltools::tags$div(class = "worms-badges", b_status, b_match),
          htmltools::tags$div(
            class = "kv-grid",
            kv("AphiaID", row$AphiaID),
            kv("Valid AphiaID", row$valid_AphiaID),
            kv("Valid name", row$valid_name),
            kv("Authority", row$authority),
            kv("Status", row$status),
            kv("Kingdom", row$kingdom),
            kv("Phylum", row$phylum),
            kv("Class", row$class),
            kv("Order", row$order),
            kv("Family", row$family),
            kv("Genus", row$genus),
            kv("isMarine", row$isMarine),
            kv("Modified", row$modified)
          ),
          if (!is.null(link_tag)) htmltools::tags$div(class = "worms-link", link_tag)
        )
      })
      
      htmltools::div(
        style = "display:grid; grid-template-columns: repeat(auto-fill, minmax(360px, 1fr)); gap:12px;",
        cards
      )
    })
    
    # CSV
    output$dl_csv <- downloadHandler(
      filename = function() {
        paste0("worms_by_name_", gsub("[^A-Za-z0-9]+","_", input$name %||% "query"), ".csv")
      },
      content = function(file) {
        df <- results_df()
        if (!nrow(df)) df <- tibble::tibble()
        readr::write_csv(df, file, na = "")
      }
    )
  })
}
