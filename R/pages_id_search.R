# ===== File: R/pages_id_search.R =====
# ID Search page — crosswalk WoRMS (AphiaID) <-> NCBI (uid) from a query

IDSearchUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("ID Search"),
    sidebarLayout(
      sidebarPanel(
        helpText("Enter an AphiaID (e.g., 204335), an NCBI Taxon ID (e.g., 8081), or a scientific name (e.g., Lutjanus sebae)."),
        textInput(ns("query"), "Query", "Lutjanus sebae"),
        actionButton(ns("go"), "Search", class = "btn-primary"),
        br(), br(),
        uiOutput(ns("note_pkgs"))
      ),
      mainPanel(
        DT::DTOutput(ns("results")),
        br(),
        verbatimTextOutput(ns("debug"))
      )
    )
  )
}

IDSearchServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    has_taxize <- requireNamespace("taxize", quietly = TRUE)
    has_worrms <- requireNamespace("worrms", quietly = TRUE)
    
    output$note_pkgs <- renderUI({
      tags$div(
        if (!has_worrms) tags$p("Note: {worrms} not installed — AphiaID lookup unavailable.", class = "text-danger"),
        if (!has_taxize) tags$p("Note: {taxize} not installed — NCBI Taxon ID lookup unavailable.", class = "text-warning")
      )
    })
    
    # --- tiny helpers ---------------------------------------------------------
    .is_integer_string <- function(x) isTRUE(grepl("^[0-9]+$", trimws(x)))
    .trim_null <- function(x) ifelse(length(x) == 0 || is.null(x) || is.na(x), NA, x)
    
    # Resolve via WoRMS (AphiaID) from name or ID
    resolve_worms <- function(q) {
      if (!has_worrms) return(NULL)
      
      # If query is numeric -> assume AphiaID
      if (.is_integer_string(q)) {
        rec <- try(worrms::wm_record(as.integer(q)), silent = TRUE)
        if (inherits(rec, "try-error") || is.null(rec)) return(NULL)
        return(rec)
      }
      
      # Else treat as name
      recs <- try(worrms::wm_records_name(name = q), silent = TRUE)
      if (inherits(recs, "try-error") || is.null(recs) || length(recs) == 0) return(NULL)
      
      # Prefer exact (case-insensitive) match, then first result
      exact <- Filter(function(r) isTRUE(tolower(r$scientificname) == tolower(q)), recs)
      (exact %||% recs)[[1]]
    }
    
    # Resolve NCBI uid from name (or from AphiaID->name)
    resolve_ncbi_uid <- function(name) {
      if (!has_taxize) return(NA)
      out <- try(taxize::get_uid(name, messages = FALSE), silent = TRUE)
      if (inherits(out, "try-error")) return(NA)
      as.character(.trim_null(out[[1]]))
    }
    
    resolve_ncbi_name <- function(uid) {
      if (!has_taxize) return(NA)
      out <- try(taxize::uid2name(uid, db = "ncbi"), silent = TRUE)
      if (inherits(out, "try-error") || is.null(out)) return(NA)
      as.character(.trim_null(out$name))
    }
    
    # Main action
    results <- eventReactive(input$go, {
      req(input$query)
      q <- trimws(input$query)
      
      # Start with WoRMS where possible
      worms_rec <- resolve_worms(q)
      
      # Build a base row
      sci_name <- if (!is.null(worms_rec)) worms_rec$scientificname else if (.is_integer_string(q) && has_taxize) resolve_ncbi_name(q) else q
      rank     <- if (!is.null(worms_rec)) worms_rec$rank else NA
      status   <- if (!is.null(worms_rec)) worms_rec$status else NA
      aphia    <- if (!is.null(worms_rec)) worms_rec$AphiaID else NA
      valid_id <- if (!is.null(worms_rec)) worms_rec$valid_AphiaID else NA
      valid_nm <- if (!is.null(worms_rec)) worms_rec$valid_name else NA
      
      # If we only got name, try NCBI from name
      ncbi_uid <- if (.is_integer_string(q) && isTRUE(nchar(q) > 0) && has_taxize) {
        # user supplied numeric — could be NCBI id
        q
      } else {
        resolve_ncbi_uid(sci_name)
      }
      
      tibble::tibble(
        query           = q,
        scientificName  = sci_name,
        rank            = rank,
        status          = status,
        AphiaID         = aphia,
        valid_AphiaID   = valid_id,
        valid_name      = valid_nm,
        NCBI_Taxon_ID   = ncbi_uid
      )
    }, ignoreInit = TRUE)
    
    output$results <- DT::renderDT({
      df <- results()
      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(tibble::tibble(msg = "No results"), rownames = FALSE))
      }
      DT::datatable(
        df,
        options = list(pageLength = 5, scrollX = TRUE),
        rownames = FALSE
      )
    })
    
    # Optional debug pane
    output$debug <- renderText({
      paste("packages: worrms =", has_worrms, ", taxize =", has_taxize)
    })
  })
}
