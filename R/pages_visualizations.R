# ===== File: R/pages_visualizations.R =====
# Power BI tabs + External Streamlit embeds (with hyperlink, spinner, fallback, and PNG capture)

VizUI <- function(id) {
  ns <- NS(id)
  shiny::fluidPage(
    shiny::titlePanel("Visualizations"),
    shiny::hr(),
    
    bslib::navs_tab(
      id = ns("viz_tabs"),
      
      # --- Live, hard-wired embeds ---
      bslib::nav_panel("Distribution",      shiny::uiOutput(ns("dist_panel"))),
      bslib::nav_panel("Progress",          shiny::uiOutput(ns("progress_panel"))),
      bslib::nav_panel("Taxonomy flow",     shiny::uiOutput(ns("taxonomy_panel"))),
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
              shiny::textInput(ns("public_url"),   "Publish-to-web URL (Distribution)", value = ""),
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
    
    # ---- Styles + JS helpers ----
    htmltools::tags$head(
      htmltools::tags$style(htmltools::HTML("
        .pbi-container{width:100%;border:1px solid #e5e7eb;border-radius:12px;overflow:hidden;position:relative;background:#fff;}
        .pbi-iframe{width:100%;height:100%;border:0;}
        .panel-header{padding:8px 10px;font-weight:600;border-bottom:1px solid #eee;background:#fafafa;}
        .toolbar{display:flex;gap:8px;align-items:center;margin:6px 0 10px 0;}
        .toolbar .btn{padding:4px 10px; border-radius:8px;}
        .spinner-wrap{position:absolute; inset:40px 0 0 0; display:flex; align-items:center; justify-content:center;
          background:linear-gradient(180deg,#fff,rgba(255,255,255,0.92)); z-index:2;}
        .spinner{width:42px; height:42px; border-radius:50%; border:4px solid #e5e7eb; border-top-color:#25a5c3; animation:spin 0.9s linear infinite;}
        @keyframes spin { to { transform: rotate(360deg); } }
      ")),
      # html2canvas for fallback
      htmltools::tags$script(src = "https://cdn.jsdelivr.net/npm/html2canvas@1.4.1/dist/html2canvas.min.js"),
      # Screenshot helper: screen capture + crop; html2canvas fallback
      htmltools::tags$script(htmltools::HTML("
        async function captureElementPNG(elId, fileName) {
          const el = document.getElementById(elId);
          if (!el) { alert('Panel not found.'); return; }
          const rect = el.getBoundingClientRect();
          const dpr = window.devicePixelRatio || 1;

          const canScreenCap = !!(navigator.mediaDevices && navigator.mediaDevices.getDisplayMedia);
          const isSecure = (location.protocol === 'https:' || location.hostname === 'localhost');

          if (canScreenCap && isSecure && window === window.top) {
            try {
              const stream = await navigator.mediaDevices.getDisplayMedia({
                video: { displaySurface: 'browser', preferCurrentTab: true, logicalSurface: true },
                audio: false
              });
              const track = stream.getVideoTracks()[0];
              const imageCapture = new ImageCapture(track);
              const bmp = await imageCapture.grabFrame();
              const fullW = bmp.width, fullH = bmp.height;

              const cropX = Math.max(0, Math.floor(rect.left * dpr));
              const cropY = Math.max(0, Math.floor(rect.top  * dpr));
              const cropW = Math.min(fullW - cropX, Math.floor(rect.width  * dpr));
              const cropH = Math.min(fullH - cropY, Math.floor(rect.height * dpr));

              const canvas = document.createElement('canvas');
              canvas.width = cropW; canvas.height = cropH;
              const ctx = canvas.getContext('2d');
              ctx.drawImage(bmp, cropX, cropY, cropW, cropH, 0, 0, cropW, cropH);

              track.stop();
              const a = document.createElement('a');
              a.download = fileName || 'visualization.png';
              a.href = canvas.toDataURL('image/png');
              a.click();
              return;
            } catch (err) {
              console.warn('Screen capture failed; falling back to html2canvas.', err);
            }
          }

          if (window.html2canvas) {
            try {
              const canvas = await window.html2canvas(el, {useCORS:true, allowTaint:true, backgroundColor:'#ffffff'});
              const a = document.createElement('a');
              a.download = fileName || 'visualization.png';
              a.href = canvas.toDataURL('image/png');
              a.click();
            } catch (e) {
              alert('Fallback capture failed. Open in a new tab and use your browser\\'s Save or Print to PDF.');
            }
          } else {
            alert('Unable to capture. Try opening in a new tab and use your browser\\'s Save or Print to PDF.');
          }
        }
      "))
    )
  )
}

VizServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---------- Helpers ----------
    is_blank <- function(x) is.null(x) || !nzchar(trimws(x))
    
    # Plain iframe card (used in Testing tab)
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
    
    # Toolbar + iframe + download button
    pbi_card <- function(container_id, url, height_px = 800, title = NULL, filename = "visualization.png") {
      if (is_blank(url)) {
        return(shiny::div(
          class = "p-3",
          shiny::strong("No Power BI URL provided."),
          shiny::div("Paste a Publish-to-web URL or configure the secure embed.")
        ))
      }
      htmltools::tagList(
        htmltools::div(
          class = "toolbar",
          shiny::actionButton(
            ns(paste0(container_id, "_dl")),
            label = "📷 Download PNG",
            class = "btn btn-outline-secondary",
            onclick = sprintf("captureElementPNG('%s','%s');", ns(container_id), filename)
          )
        ),
        htmltools::tags$div(
          id = ns(container_id),
          class = "pbi-container", style = sprintf("height:%dpx;", as.integer(height_px)),
          if (!is_blank(title)) htmltools::tags$div(class = "panel-header", title),
          htmltools::tags$iframe(class = "pbi-iframe", src = url, allowfullscreen = NA, frameborder = "0")
        )
      )
    }
    
    # Streamlit card with spinner + fallback + toolbar
    # Streamlit card with spinner + timeout fallback + toolbar
    streamlit_card <- function(container_id, url, public_url, height_px = 900, title = "", filename = "visualization.png", timeout_ms = 12000) {
      iframe_id <- ns(paste0(container_id, "_iframe"))
      fb_id     <- ns(paste0(container_id, "_fb"))
      spin_id   <- ns(paste0(container_id, "_spin"))
      wrapper_id<- ns(container_id)
      
      htmltools::tagList(
        htmltools::div(
          class = "toolbar",
          shiny::a(href = public_url, target = "_blank", rel = "noopener",
                   class = "btn btn-primary", "Open in new tab"),
          shiny::actionButton(
            ns(paste0(container_id, "_dl")),
            label = "📷 Download PNG",
            class = "btn btn-outline-secondary",
            onclick = sprintf("captureElementPNG('%s','%s');", wrapper_id, filename)
          )
        ),
        htmltools::tags$div(
          id = wrapper_id,
          class = "pbi-container",
          style = sprintf("height:%dpx;", as.integer(height_px)),
          htmltools::tags$div(class = "panel-header", title),
          
          # Spinner overlay (initially visible)
          htmltools::tags$div(
            id = spin_id, class = "spinner-wrap",
            htmltools::tags$div(class = "spinner"),
            htmltools::tags$div(style = "margin-left:10px; font-weight:500; color:#374151;", "Loading dashboard…")
          ),
          
          # The iframe; when it loads we hide the spinner & ensure fallback is hidden
          htmltools::tags$iframe(
            id   = iframe_id,
            class= "pbi-iframe",
            src  = url,
            allow= "clipboard-read; clipboard-write; fullscreen",
            frameborder = "0",
            onload = sprintf(
              "try{ 
             var s=document.getElementById('%s'); if(s) s.style.display='none';
             var f=document.getElementById('%s'); if(f) f.style.display='none';
           }catch(e){}",
              spin_id, fb_id
            )
          ),
          
          # Fallback panel (initially hidden; shown if we time out)
          htmltools::tags$div(
            id = fb_id,
            style = "display:none; padding:16px;",
            htmltools::tags$strong("The site didn’t display inside the app."),
            htmltools::tags$p("Some websites block embedding (X-Frame-Options / CSP). Use the button above to open it."),
            htmltools::tags$a(
              href = public_url, target = "_blank", rel = "noopener",
              class = "btn btn-outline-secondary", "Open in a new tab"
            )
          )
        ),
        
        # Per-card watchdog: if onload never fires (blocked/CSP), show fallback & hide spinner
        htmltools::tags$script(htmltools::HTML(sprintf("
      (function(){
        var fired = false;
        var ifr = document.getElementById('%s');
        var spin= document.getElementById('%s');
        var fb  = document.getElementById('%s');

        if (!ifr) return;

        var markLoaded = function(){ fired = true; if (spin) spin.style.display='none'; if (fb) fb.style.display='none'; };

        // If the onload has already fired by the time this runs, hide spinner now
        if (ifr.complete) { markLoaded(); }

        // Belt-and-braces: also listen to load (some browsers dispatch late)
        ifr.addEventListener('load', markLoaded, { once:true });

        // Watchdog timeout: if nothing loads in time, reveal fallback
        setTimeout(function(){
          if (fired) return;
          if (spin) spin.style.display='none';
          if (fb)   fb.style.display='';
        }, %d);
      })();
    ", iframe_id, spin_id, fb_id, as.integer(timeout_ms))))
      )
    }
    
    
    # ---------- Hard-wire your two live public links into the main tabs ----------
    DIST_URL <- "https://app.powerbi.com/view?r=eyJrIjoiOTYyOGJiMzEtYzU2Mi00Nzc0LTkyZTUtNTBlN2IxMTAzZjRlIiwidCI6IjYwMDg2NDZiLTFmODctNDI0NC05YzMxLTI0Yjg1ZGQwNGRhMiIsImMiOjEwfQ%3D%3D"
    PROG_URL <- "https://app.powerbi.com/view?r=eyJrIjoiZDM0MTYyMjEtMjM3MS00OTEyLTljODUtNjgzODM3MmQ3OGIwIiwidCI6IjYwMDg2NDZiLTFmODctNDI0NC05YzMxLTI0Yjg1ZGQwNGRhMiIsImMiOjEwfQ%3D%3D"
    
    output$dist_panel <- shiny::renderUI({
      fname <- sprintf('Distribution_%s.png', format(Sys.time(), '%%Y%%m%%d_%%H%%M%%S'))
      pbi_card(container_id = "dist_card", url = DIST_URL, height_px = 800,
               title = "Distribution dashboard", filename = fname)
    })
    
    output$progress_panel <- shiny::renderUI({
      fname <- sprintf('Progress_%s.png', format(Sys.time(), '%%Y%%m%%d_%%H%%M%%S'))
      pbi_card(container_id = "prog_card", url = PROG_URL, height_px = 820,
               title = "Progress dashboard", filename = fname)
    })
    
    # ---------- "Other" tab: hyperlink + embed Streamlit with spinner + fallback ----------
    OTHER_URL   <- "https://ocean-genomes-dashboard.streamlit.app/?embed=true"
    OTHER_LINK  <- "https://ocean-genomes-dashboard.streamlit.app/"
    output$other_panel <- shiny::renderUI({
      fname <- sprintf('AdvancedInsights_%s.png', format(Sys.time(), '%%Y%%m%%d_%%H%%M%%S'))
      streamlit_card(container_id = "other_iframe_container",
                     url = OTHER_URL, public_url = OTHER_LINK,
                     height_px = 900, title = "Ocean Genomes (Streamlit)",
                     filename = fname)
    })
    
    # ---------- "Taxonomy flow" tab ----------
    TAX_URL    <- "https://speciesdashboard-z7fkbgdptyxl3deet89pqb.streamlit.app/?embed=true"
    TAX_PUBLIC <- "https://speciesdashboard-z7fkbgdptyxl3deet89pqb.streamlit.app/"
    output$taxonomy_panel <- shiny::renderUI({
      fname <- sprintf('TaxonomyFlow_%s.png', format(Sys.time(), '%%Y%%m%%d_%%H%%M%%S'))
      streamlit_card(container_id = "tax_iframe_container",
                     url = TAX_URL, public_url = TAX_PUBLIC,
                     height_px = 900, title = "Taxonomy flow (Streamlit)",
                     filename = fname)
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
