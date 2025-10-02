# ===== File: R/pages_visualizations.R =====
# Power BI tabs + External Streamlit embeds (with hyperlink, spinner, and fallback)

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
      bslib::nav_panel("Taxonomy flow",    shiny::uiOutput(ns("taxonomy_panel"))),
      bslib::nav_panel("Advanced Insights", shiny::uiOutput(ns("other_panel"))),
      
      # --- Testing (allows overriding links at runtime) ---
      bslib::nav_panel(
        "Testing",
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::radioButtons(
              ns("mode"), "Embed mode",
              c("Public (Publish to web)" = "public",
                "Secure (Embed for your org)" = "secure"),
              selected = "public"
            ),
            shiny::conditionalPanel(
              sprintf("input['%s'] === 'public'", ns("mode")),
              shiny::textInput(ns("public_url"),  "Publish-to-web URL (Distribution)", value = ""),
              shiny::textInput(ns("progress_url"), "Publish-to-web URL (Progress; optional)", value = ""),
              shiny::numericInput(ns("public_h"),   "Distribution height (px)", 800, 400, 3000, 20),
              shiny::numericInput(ns("progress_h"), "Progress height (px)",     820, 400, 3000, 20),
              shiny::actionButton(ns("show_public"), "Preview in Testing", class = "btn-primary")
            ),
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
    
    # ---- Styles ----
    htmltools::tags$head(
      htmltools::tags$style(htmltools::HTML("
        .pbi-container{width:100%;border:1px solid #e5e7eb;border-radius:12px;overflow:hidden;position:relative;background:#fff;}
        .pbi-iframe{width:100%;height:100%;border:0;}
        .panel-header{padding:8px 10px;font-weight:600;border-bottom:1px solid #eee;background:#fafafa;}

        /* Spinner overlay inside the container */
        .spinner-wrap{
          position:absolute; inset:40px 0 0 0;
          display:flex; align-items:center; justify-content:center;
          background:linear-gradient(180deg,#fff,rgba(255,255,255,0.92));
          z-index:2;
        }
        .spinner{
          width:42px; height:42px; border-radius:50%;
          border:4px solid #e5e7eb; border-top-color:#25a5c3;
          animation:spin 0.9s linear infinite;
        }
        @keyframes spin { to { transform: rotate(360deg); } }
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
          class = "p-3",
          shiny::strong("No Power BI URL provided."),
          shiny::div("Paste a Publish-to-web URL or configure the secure embed.")
        ))
      }
      htmltools::tags$div(
        class = "pbi-container", style = sprintf("height:%dpx;", as.integer(height_px)),
        if (!is_blank(title)) htmltools::tags$div(class = "panel-header", title),
        htmltools::tags$iframe(class = "pbi-iframe", src = url, allowfullscreen = NA, frameborder = "0")
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
  
  # ---------- "Other" tab: hyperlink + embed Streamlit with spinner + fallback ----------
  OTHER_URL <- "https://ocean-genomes-dashboard.streamlit.app/?embed=true"
  
  output$other_panel <- shiny::renderUI({
    container_id <- ns("other_iframe_container")
    iframe_id    <- ns("other_iframe")
    fb_id        <- ns("other_fallback")
    spin_id      <- ns("other_spinner")
    
    htmltools::tagList(
      # Always-visible hyperlink (opens in new tab) with top/bottom padding
      shiny::div(
        style = "padding-top:20px; padding-bottom:20px;",
        shiny::a(
          href   = "https://ocean-genomes-dashboard.streamlit.app/",
          target = "_blank", rel = "noopener",
          class  = "btn btn-primary",
          "Open Ocean Genomes (Streamlit) in a new tab"
        )
      ),
      
      # The embedded card
      htmltools::tags$div(
        id = container_id,
        class = "pbi-container",
        style = "height:900px;",
        
        # Card header
        htmltools::tags$div(class = "panel-header", "Ocean Genomes (Streamlit)"),
        
        # Spinner overlay (visible by default, hidden on load)
        htmltools::tags$div(
          id = spin_id, class = "spinner-wrap",
          htmltools::tags$div(class = "spinner"),
          htmltools::tags$div(style = "margin-left:10px; font-weight:500; color:#374151;", "Loading dashboard…")
        ),
        
        # iframe attempt
        htmltools::tags$iframe(
          id = iframe_id, class = "pbi-iframe",
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
          htmltools::tags$a(
            href = "https://ocean-genomes-dashboard.streamlit.app/",
            target = "_blank", rel = "noopener",
            class = "btn btn-outline-secondary",
            "Open in a new tab"
          )
        )
      ),
      
      # JS: spinner hide + 3s fallback
      htmltools::tags$script(htmltools::HTML(sprintf("
          (function(){
            var iframe   = document.getElementById('%s');
            var fallback = document.getElementById('%s');
            var spinner  = document.getElementById('%s');
            if (!iframe) return;

            var loaded = false;
            iframe.addEventListener('load', function(){
              loaded = true;
              if (spinner) spinner.style.display = 'none';
              if (fallback) fallback.style.display = 'none';
            }, { once: true });

            setTimeout(function(){
              if (!loaded) {
                if (spinner)  spinner.style.display  = 'none';
                if (fallback) fallback.style.display = 'block';
              }
            }, 3000);
          })();
        ", iframe_id, fb_id, spin_id)))
    )
  })
  
  # ---------- NEW: "Taxonomy flow" tab (same UX pattern) ----------
  TAX_URL      <- "https://speciesdashboard-z7fkbgdptyxl3deet89pqb.streamlit.app/?embed=true"
  TAX_PUBLIC   <- "https://speciesdashboard-z7fkbgdptyxl3deet89pqb.streamlit.app/"
  
  output$taxonomy_panel <- shiny::renderUI({
    container_id <- ns("tax_iframe_container")
    iframe_id    <- ns("tax_iframe")
    fb_id        <- ns("tax_fallback")
    spin_id      <- ns("tax_spinner")
    
    htmltools::tagList(
      # Always-visible hyperlink (opens in new tab) with top/bottom padding
      shiny::div(
        style = "padding-top:20px; padding-bottom:20px;",
        shiny::a(
          href   = TAX_PUBLIC,
          target = "_blank", rel = "noopener",
          class  = "btn btn-primary",
          "Open Taxonomy flow (Streamlit) in a new tab"
        )
      ),
      
      # Embedded card
      htmltools::tags$div(
        id = container_id,
        class = "pbi-container",
        style = "height:900px;",
        
        htmltools::tags$div(class = "panel-header", "Taxonomy flow (Streamlit)"),
        
        htmltools::tags$div(
          id = spin_id, class = "spinner-wrap",
          htmltools::tags$div(class = "spinner"),
          htmltools::tags$div(style = "margin-left:10px; font-weight:500; color:#374151;", "Loading dashboard…")
        ),
        
        htmltools::tags$iframe(
          id = iframe_id, class = "pbi-iframe",
          src = TAX_URL,
          allow = "clipboard-read; clipboard-write; fullscreen",
          frameborder = "0"
        ),
        
        htmltools::tags$div(
          id = fb_id,
          style = "display:none; padding:16px;",
          htmltools::tags$strong("The site didn’t display inside the app."),
          htmltools::tags$p("Some websites block embedding (X-Frame-Options / CSP). Use the button above to open it."),
          htmltools::tags$a(
            href = TAX_PUBLIC,
            target = "_blank", rel = "noopener",
            class = "btn btn-outline-secondary",
            "Open in a new tab"
          )
        )
      ),
      
      # JS: spinner hide + 3s fallback
      htmltools::tags$script(htmltools::HTML(sprintf("
          (function(){
            var iframe   = document.getElementById('%s');
            var fallback = document.getElementById('%s');
            var spinner  = document.getElementById('%s');
            if (!iframe) return;

            var loaded = false;
            iframe.addEventListener('load', function(){
              loaded = true;
              if (spinner) spinner.style.display = 'none';
              if (fallback) fallback.style.display = 'none';
            }, { once: true });

            setTimeout(function(){
              if (!loaded) {
                if (spinner)  spinner.style.display  = 'none';
                if (fallback) fallback.style.display = 'block';
              }
            }, 3000);
          })();
        ", iframe_id, fb_id, spin_id)))
    )
  })
  
  # ---------- Testing tab logic ----------
  observeEvent(input$show_public, {
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
    if (is_blank(input$embed_url)) {
      output$pbi_view <- shiny::renderUI(shiny::div(
        class = "p-3",
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
