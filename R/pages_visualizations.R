# ===== File: R/pages_visualizations.R =====
# Power BI + Streamlit embeds with auto-fallback to new window
# Testing tab shows NO visualization until a new link is pasted and previewed.

VizUI <- function(id) {
  ns <- NS(id)
  shiny::fluidPage(
    shiny::titlePanel("Visualizations"),
    shiny::p("If an embed is blocked by cross-site restrictions, it will open in a new window automatically."),
    shiny::hr(),

    bslib::navs_tab(
      id = ns("viz_tabs"),

      bslib::nav_panel("Distribution", shiny::uiOutput(ns("dist_panel"))),
      bslib::nav_panel("Progress",     shiny::uiOutput(ns("progress_panel"))),

      bslib::nav_panel(
        "Advanced visualizations",
        shiny::div(
          class = "mb-2",
          "If the embed doesn’t load due to cross-site restrictions, ",
          shiny::tags$a(href = "https://ocean-genomes-dashboard.streamlit.app/",
                        target = "_blank", rel = "noopener", "open it in a new window.")
        ),
        shiny::uiOutput(ns("adv_vis"))
      ),

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
            # ---- Public inputs (LEFT BLANK BY DEFAULT) ----
            shiny::conditionalPanel(
              sprintf("input['%s'] === 'public'", ns("mode")),
              shiny::textInput(
                ns("public_url"),
                "Publish-to-web URL (Distribution)",
                value = "", # leave empty
                placeholder = "https://app.powerbi.com/view?r=XXXXXXXX"
              ),
              shiny::textInput(
                ns("progress_url"),
                "Publish-to-web URL (Progress, optional — leave blank to reuse Distribution)",
                value = "", # leave empty
                placeholder = "https://app.powerbi.com/view?r=YYYYYYYY"
              ),
              shiny::numericInput(ns("public_h"),   "Distribution height (px)", 800, 400, 3000, 20),
              shiny::numericInput(ns("progress_h"), "Progress height (px)",     820, 400, 3000, 20),
              shiny::actionButton(ns("show_public"), "Preview in Testing", class = "btn-primary")
            ),
            # ---- Secure inputs ----
            shiny::conditionalPanel(
              sprintf("input['%s'] === 'secure'", ns("mode")),
              shiny::textInput(ns("workspace_id"), "Workspace (Group) ID"),
              shiny::textInput(ns("report_id"),    "Report ID"),
              shiny::textInput(ns("embed_url"),    "Embed URL"),
              shiny::passwordInput(ns("embed_token"), "Embed Token"),
              shiny::numericInput(ns("secure_h"), "Height (px)", 780, 400, 3000, 20),
              shiny::actionButton(ns("show_secure"), "Show secure report", class = "btn-success"),
              shiny::helpText("Generate a short-lived embed token server-side and paste it here.")
            )
          ),
          shiny::column(
            8,
            # Placeholder only (NOT a visualization)
            shiny::uiOutput(ns("pbi_view"))
          )
        )
      )
    ),

    htmltools::tags$head(
      htmltools::tags$script(src = "https://cdn.jsdelivr.net/npm/powerbi-client@2.23.1/dist/powerbi.js"),
      htmltools::tags$style(htmltools::HTML("
        .pbi-container{width:100%;border:1px solid #e5e7eb;border-radius:12px;overflow:hidden;position:relative;}
        .pbi-iframe{width:100%;height:100%;border:0;}
        .embed-fallback{padding:16px;}
        .embed-fallback a{font-weight:600; text-decoration:underline;}
        .embed-overlay-link{position:absolute; top:8px; right:12px; z-index:2; font-size:0.9rem;}
        .test-placeholder{padding:14px; color:#6b7280; background:#f9fafb; border:1px dashed #d1d5db; border-radius:10px;}
      "))
    )
  )
}

VizServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (is.null(a) || (is.character(a) && !nzchar(a))) b else a

    # ===== Internal defaults for live tabs (hidden from inputs) =====
    DIST_DEFAULT_LINK <- "https://app.powerbi.com/view?r=eyJrIjoiNWVhZGNlMzUtNzU5MS00ZTc3LWE2YjUtMjM5OGU4MjIyZjFkIiwidCI6IjYwMDg2NDZiLTFmODctNDI0NC05YzMxLTI0Yjg1ZGQwNGRhMiIsImMiOjEwfQ%3D%3D"
    PROG_DEFAULT_LINK <- DIST_DEFAULT_LINK  # temp: Progress uses same until you paste a new link

    # ---- Generic embed with auto-fallback to new window ----
    embed_with_fallback <- function(url, height_px, box_id) {
      ujson <- jsonlite::toJSON(url, auto_unbox = TRUE)
      bid   <- jsonlite::toJSON(box_id, auto_unbox = TRUE)
      htmltools::tagList(
        htmltools::div(
          id = box_id, class = "pbi-container",
          style = sprintf("height:%dpx;", as.integer(height_px)),
          htmltools::tags$a(class="embed-overlay-link", href = url, target = "_blank", rel = "noopener", "Open in new window")
        ),
        htmltools::tags$script(htmltools::HTML(sprintf("
          (function(){
            const url = %s, boxId = %s;
            const box = document.getElementById(boxId);
            if(!box) return;

            const overlay = box.querySelector('a.embed-overlay-link')?.outerHTML || '';
            box.innerHTML = overlay;
            const iframe = document.createElement('iframe');
            iframe.className = 'pbi-iframe';
            iframe.setAttribute('allowFullScreen','');
            iframe.src = url;
            box.appendChild(iframe);

            let loaded = false, opened = false;
            const openNew = () => {
              if (opened) return; opened = true;
              try { window.open(url, '_blank', 'noopener'); } catch(e) {}
              box.innerHTML = overlay + '<div class=\"embed-fallback\">This content is blocked by the site. ' +
                              '<a href=\"'+url+'\" target=\"_blank\" rel=\"noopener\">Open in a new window</a>.</div>';
            };

            iframe.addEventListener('load', function(){ loaded = true; });

            setTimeout(function(){ if (!loaded) openNew(); }, 1800);
            setTimeout(function(){
              try {
                const href = iframe.contentWindow && iframe.contentWindow.location && iframe.contentWindow.location.href;
                if (!href || href === 'about:blank') openNew();
              } catch(e) { /* cross-origin: ignore */ }
            }, 3000);
          })();
        ", ujson, bid)))
      )
    }

    # ---- Reactives for live tabs (allow pasted URLs to override defaults) ----
    dist_url <- shiny::reactive({
      url <- trimws(input$public_url %||% "")
      if (nzchar(url)) url else DIST_DEFAULT_LINK
    })
    prog_url <- shiny::reactive({
      url <- trimws(input$progress_url %||% "")
      if (nzchar(url)) url else PROG_DEFAULT_LINK
    })

    # === Testing tab: show NOTHING until a new link is pasted and Preview is clicked ===
    output$pbi_view <- shiny::renderUI({
      htmltools::div(class="test-placeholder",
                     "Paste a new Power BI link above and click ",
                     htmltools::tags$strong("Preview in Testing"),
                     " to see it here.")
    })
    observeEvent(input$show_public, {
      # Choose whichever new link the user pasted (Distribution first, then Progress)
      new_url <- trimws(input$public_url %||% "")
      if (!nzchar(new_url)) new_url <- trimws(input$progress_url %||% "")
      shiny::req(nzchar(new_url))  # if empty, do nothing (keeps placeholder)
      h <- as.integer(input$public_h %||% 780)
      output$pbi_view <- shiny::renderUI(embed_with_fallback(new_url, h, ns("test_iframe")))
    })

    # === Secure preview (only renders when user supplies all fields + clicks) ===
    observeEvent(input$show_secure, {
      embed_url <- trimws(input$embed_url %||% "")
      token     <- trimws(input$embed_token %||% "")
      report_id <- trimws(input$report_id %||% "")
      h         <- as.integer(input$secure_h %||% 780)
      shiny::validate(
        shiny::need(nzchar(embed_url), "Embed URL required."),
        shiny::need(nzchar(token),     "Embed token required."),
        shiny::need(nzchar(report_id), "Report ID required.")
      )
      output$pbi_view <- shiny::renderUI({
        htmltools::tagList(
          htmltools::div(id = ns("pbiReport"), class = "pbi-container", style = sprintf("height:%dpx;", h)),
          htmltools::tags$script(htmltools::HTML(sprintf("
            (() => {
              if (!window.powerbi) { console.error('powerbi.js not loaded'); return; }
              const models = window['powerbi-client'].models;
              const cfg = {
                type: 'report',
                id: '%s',
                embedUrl: '%s',
                accessToken: '%s',
                tokenType: models.TokenType.Embed,
                permissions: models.Permissions.Read,
                settings: { panes: { filters: { visible: false } }, navContentPaneEnabled: true }
              };
              const el = document.getElementById('%s');
              try { const existing = powerbi.get(el); if (existing) powerbi.reset(el); } catch(_) {}
              powerbi.embed(el, cfg);
            })();
          ", report_id, embed_url, token, ns("pbiReport"))))
        )
      })
    })

    # === Auto-render the live tabs only ===
    auto_render_tabs <- function() {
      output$dist_panel     <- shiny::renderUI(embed_with_fallback(dist_url(), as.integer(input$public_h %||% 800), ns("dist_iframe")))
      output$progress_panel <- shiny::renderUI(embed_with_fallback(prog_url(),  as.integer(input$progress_h %||% 820), ns("prog_iframe")))
      output$adv_vis        <- shiny::renderUI(embed_with_fallback("https://ocean-genomes-dashboard.streamlit.app/", 900, ns("adv_iframe")))
    }

    shiny::observe({ auto_render_tabs() })  # on session start
    shiny::observeEvent(list(input$public_url, input$progress_url), { auto_render_tabs() }, ignoreInit = TRUE)
  })
}
