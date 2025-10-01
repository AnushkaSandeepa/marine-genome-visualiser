# ===== File: R/pages_visualizations.R =====
# Power BI tabs + Advanced visualizations (Python via reticulate + plotly)
# Adds an "Other" tab that embeds Streamlit AND always shows a hyperlink above the card,
# with a JS-based fallback if the iframe cannot load (blocked by X-Frame-Options / CSP).

VizUI <- function(id) {
  ns <- NS(id)
  shiny::fluidPage(
    shiny::titlePanel("Visualizations"),
    shiny::hr(),
    
    bslib::navs_tab(
      id = ns("viz_tabs"),
      
      # --- Live, hard-wired embeds ---
      bslib::nav_panel("Distribution", shiny::uiOutput(ns("dist_panel"))),
      bslib::nav_panel("Progress",     shiny::uiOutput(ns("progress_panel"))),
      
      # --- Advanced: Python/Plotly (reticulate) ---
      bslib::nav_panel(
        "Advanced visualizations",
        shiny::div(
          class = "mb-2",
          shiny::helpText("These plots are rendered from Python via reticulate (pandas + plotly).")
        ),
        shiny::uiOutput(ns("py_grid"))  # 2x2 grid of plot HTML
      ),
      
      # --- External (Streamlit) ---
      bslib::nav_panel(
        "Other",
        shiny::uiOutput(ns("other_panel"))
      ),
      
      # --- Testing (allows overriding links at runtime) ---
      bslib::nav_panel(
        "Testing",
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::radioButtons(
              ns("mode"), "Embed mode",
              c("Public (Publish to web)" = "public", "Secure (Embed for your org)" = "secure"),
              selected = "public"
            ),
            
            # --- Public publish-to-web preview ---
            shiny::conditionalPanel(
              sprintf("input['%s'] === 'public'", ns("mode")),
              shiny::textInput(ns("public_url"),
                               "Publish-to-web URL (Distribution)",
                               value = ""  # can paste another link here
              ),
              shiny::textInput(ns("progress_url"),
                               "Publish-to-web URL (Progress; optional)",
                               value = ""  # can paste another link here
              ),
              shiny::numericInput(ns("public_h"),   "Distribution height (px)", 800, 400, 3000, 20),
              shiny::numericInput(ns("progress_h"), "Progress height (px)",     820, 400, 3000, 20),
              shiny::actionButton(ns("show_public"), "Preview in Testing", class = "btn-primary")
            ),
            
            # --- Secure org embed preview (token-based) ---
            shiny::conditionalPanel(
              sprintf("input['%s'] === 'secure'", ns("mode")),
              shiny::textInput(ns("workspace_id"), "Workspace (Group) ID"),
              shiny::textInput(ns("report_id"),    "Report ID"),
              shiny::textInput(ns("embed_url"),    "Embed URL"),
              shiny::passwordInput(ns("embed_token"), "Embed Token"),
              shiny::numericInput(ns("secure_h"), "Height (px)", 780, 400, 3000, 20),
              shiny::actionButton(ns("show_secure"), "Show secure report", class = "btn-success")
            )
          ),
          shiny::column(8, shiny::uiOutput(ns("pbi_view")))
        )
      )
    ),
    
    # styles for containers
    htmltools::tags$head(
      htmltools::tags$style(htmltools::HTML("
        .pbi-container{width:100%;border:1px solid #e5e7eb;border-radius:12px;overflow:hidden;position:relative;background:#fff;}
        .pbi-iframe{width:100%;height:100%;border:0;}
        .py-card{border:1px solid #e5e7eb;border-radius:12px;padding:8px;margin-bottom:16px;background:#fff;}
      "))
    )
  )
}

VizServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---------- Helpers ----------
    is_blank <- function(x) is.null(x) || !nzchar(trimws(x))
    pbi_iframe <- function(url, height_px = 800, title = NULL) {
      if (is_blank(url)) {
        return(shiny::div(
          class = "p-3 py-card",
          shiny::strong("No Power BI URL provided."),
          shiny::div("Paste a Publish-to-web URL or configure the secure embed.")
        ))
      }
      htmltools::tags$div(
        class = "pbi-container", style = sprintf("height:%dpx;", as.integer(height_px)),
        if (!is_blank(title)) htmltools::tags$div(
          style = "padding:8px 10px;font-weight:600;border-bottom:1px solid #eee;background:#fafafa;",
          title
        ),
        htmltools::tags$iframe(
          class = "pbi-iframe",
          src = url,
          allowfullscreen = NA,
          frameborder = "0"
        )
      )
    }
    
    # ---------- Hard-wire your two live public links into the main tabs ----------
    # Distribution:
    DIST_URL <- "https://app.powerbi.com/view?r=eyJrIjoiOTYyOGJiMzEtYzU2Mi00Nzc0LTkyZTUtNTBlN2IxMTAzZjRlIiwidCI6IjYwMDg2NDZiLTFmODctNDI0NC05YzMxLTI0Yjg1ZGQwNGRhMiIsImMiOjEwfQ%3D%3D"
    # Progress:
    PROG_URL <- "https://app.powerbi.com/view?r=eyJrIjoiZDM0MTYyMjEtMjM3MS00OTEyLTljODUtNjgzODM3MmQ3OGIwIiwidCI6IjYwMDg2NDZiLTFmODctNDI0NC05YzMxLTI0Yjg1ZGQwNGRhMiIsImMiOjEwfQ%3D%3D"
    
    output$dist_panel <- shiny::renderUI({
      pbi_iframe(DIST_URL, height_px = 800, title = "Distribution dashboard")
    })
    
    output$progress_panel <- shiny::renderUI({
      pbi_iframe(PROG_URL, height_px = 820, title = "Progress dashboard")
    })
    
    # ---------- Advanced tab: Python plots via reticulate ----------
    # Find project root (folder that contains both R/ and python/)
    find_app_root <- function() {
      d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
      for (i in 1:8) {
        if (dir.exists(file.path(d, "R")) && dir.exists(file.path(d, "python"))) return(d)
        parent <- dirname(d); if (identical(parent, d)) break; d <- parent
      }
      stop("Couldn't locate project root containing 'R/' and 'python/'. Current getwd(): ", getwd())
    }
    
    app_root <- find_app_root()
    py_dir   <- file.path(app_root, "python")
    py_file  <- file.path(py_dir, "ocean_genomes_pyvis.py")
    csv_path <- file.path(app_root, "CSV", "new_final_species.csv")
    
    # Make sure Python can see the python/ folder, then import the module
    reticulate::py_run_string(sprintf(
      "import sys; p=r'%s';\nif p not in sys.path: sys.path.insert(0, p)",
      normalizePath(py_dir, winslash = "/", mustWork = TRUE)
    ))
    
    py_mod <- try(reticulate::import("ocean_genomes_pyvis", convert = TRUE), silent = TRUE)
    if (inherits(py_mod, "try-error") || is.null(py_mod)) {
      ok <- try(reticulate::source_python(py_file), silent = TRUE)
      if (inherits(ok, "try-error"))
        stop("Import failed. Checked absolute file: ", py_file,
             "\nTip: ensure pandas+plotly are installed in reticulate's Python (e.g., install.packages('reticulate'); reticulate::py_install(c('pandas','plotly')) ).")
    }
    
    output$py_grid <- shiny::renderUI({
      out <- try({
        if (exists("render_all_html", mode = "function", inherits = TRUE)) {
          render_all_html(csv_path)                 # sourced: global R wrapper
        } else {
          py_mod$render_all_html(csv_path)          # imported module
        }
      }, silent = TRUE)
      
      if (inherits(out, "try-error") || is.null(out)) {
        err <- tryCatch(reticulate::py_last_error(), error = function(e) NULL)
        return(shiny::div(
          class = "py-card",
          shiny::strong("Python error while rendering plots."),
          shiny::br(), "CSV path used: ", csv_path,
          if (!is.null(err)) shiny::tags$pre(paste(err$type, err$value, sep = ": "))
        ))
      }
      
      html <- function(k)
        if (!is.null(out[[k]]) && nzchar(out[[k]])) htmltools::HTML(out[[k]]) else shiny::div()
      
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(6, shiny::div(class = "py-card", html("plot1"))),
          shiny::column(6, shiny::div(class = "py-card", html("plot2")))
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::div(class = "py-card", html("plot3"))),
          shiny::column(6, shiny::div(class = "py-card", html("plot4")))
        )
      )
    })
    
    # ---------- "Other" tab: hyperlink + embed Streamlit with fallback ----------
    OTHER_URL <- "https://ocean-genomes-dashboard.streamlit.app/?embed=true"
    
    output$other_panel <- shiny::renderUI({
      container_id <- ns("other_iframe_container")
      iframe_id    <- ns("other_iframe")
      fb_id        <- ns("other_fallback")
      
      htmltools::tagList(
        # Always-visible hyperlink (opens in new tab)
        shiny::div(
          class = "py-card",
          shiny::a(
            href = "https://ocean-genomes-dashboard.streamlit.app/",
            target = "_blank", rel = "noopener",
            class = "btn btn-primary",
            "Open Ocean Genomes (Streamlit) in a new tab"
          )
        ),
        
        # The embedded card
        htmltools::tags$div(
          id = container_id,
          class = "pbi-container",
          style = "height:900px;",
          htmltools::tags$div(
            style = "padding:8px 10px;font-weight:600;border-bottom:1px solid #eee;background:#fafafa;",
            "Ocean Genomes (Streamlit)"
          ),
          # iframe attempt
          htmltools::tags$iframe(
            id = iframe_id,
            class = "pbi-iframe",
            src = OTHER_URL,
            allow = "clipboard-read; clipboard-write; fullscreen",
            frameborder = "0"
          ),
          # Fallback message if iframe won't load
          htmltools::tags$div(
            id = fb_id,
            style = "display:none; padding:16px;",
            htmltools::tags$strong("The site didn’t display inside the app."),
            htmltools::tags$p("Some websites block embedding (X-Frame-Options / CSP). Use the button above to open it."),
            # extra link here too, just in case
            htmltools::tags$a(
              href = "https://ocean-genomes-dashboard.streamlit.app/",
              target = "_blank", rel = "noopener",
              class = "btn btn-outline-secondary",
              "Open in a new tab"
            )
          )
        ),
        
        # Tiny JS: if iframe doesn't load within 2.5s, show fallback text
        htmltools::tags$script(htmltools::HTML(sprintf("
          (function(){
            var iframe = document.getElementById('%s');
            var fallback = document.getElementById('%s');
            var loaded = false;
            if (!iframe) return;
            iframe.addEventListener('load', function(){ loaded = true; }, { once: true });
            setTimeout(function(){
              if (!loaded && fallback) { fallback.style.display = 'block'; }
            }, 2500);
          })();
        ", iframe_id, fb_id)))
      )
    })
    
    # ---------- Testing tab logic ----------
    observeEvent(input$show_public, {
      # If nothing entered, fall back to the hard-wired links so Testing still shows something useful.
      url_a <- if (!is_blank(input$public_url)) input$public_url else DIST_URL
      url_b <- if (!is_blank(input$progress_url)) input$progress_url else PROG_URL
      
      output$pbi_view <- shiny::renderUI({
        shiny::tagList(
          pbi_iframe(url_a, as.integer(input$public_h),   title = "Distribution (Testing)"),
          shiny::div(style = "height:12px;"),
          pbi_iframe(url_b, as.integer(input$progress_h), title = "Progress (Testing)")
        )
      })
    }, ignoreInit = TRUE)
    
    observeEvent(input$show_secure, {
      # Minimal secure embed placeholder (your JS SDK/token workflow would live here)
      if (is_blank(input$embed_url)) {
        output$pbi_view <- shiny::renderUI(shiny::div(
          class = "py-card",
          shiny::strong("Secure embed requires a valid Embed URL."),
          shiny::div("Tip: supply Workspace ID, Report ID, Embed URL, and an Embed Token.")
        ))
        return()
      }
      output$pbi_view <- shiny::renderUI({
        pbi_iframe(input$embed_url, as.integer(input$secure_h), title = "Secure embed (Testing)")
      })
    }, ignoreInit = TRUE)
  })
}
