# ===== File: R/pages_visualizations.R =====
# Embed Power BI (Public iframe + Secure embed) in Shiny

VizUI <- function(id) {
  ns <- NS(id)
  shiny::fluidPage(
    shiny::titlePanel("Visualizations"),
    shiny::p("Under Maintanance. Embed a Power BI report here. Choose public (quick test) or secure (recommended for internal data)."),
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::radioButtons(
          ns("mode"), "Embed mode",
          c("Public (Publish to web)" = "public", "Secure (Embed for your org)" = "secure"),
          selected = "public"
        ),
        # ----- PUBLIC (Publish to web) -----
        shiny::conditionalPanel(
          sprintf("input['%s'] === 'public'", ns("mode")),
          shiny::textInput(
            ns("public_url"),
            "Publish-to-web URL (Power BI → File → Embed → Publish to web → 'View' link)",
            placeholder = "https://app.powerbi.com/view?r=XXXXXXXX"
          ),
          shiny::numericInput(ns("public_h"), "Height (px)", 780, 400, 3000, 20),
          shiny::actionButton(ns("show_public"), "Show public report", class = "btn-primary")
        ),
        # ----- SECURE (Embed for your org / Embedded) -----
        shiny::conditionalPanel(
          sprintf("input['%s'] === 'secure'", ns("mode")),
          shiny::textInput(ns("workspace_id"), "Workspace (Group) ID"),
          shiny::textInput(ns("report_id"),    "Report ID"),
          shiny::textInput(ns("embed_url"),    "Embed URL"),
          shiny::passwordInput(ns("embed_token"), "Embed Token (paste from your backend)"),
          shiny::numericInput(ns("secure_h"), "Height (px)", 780, 400, 3000, 20),
          shiny::actionButton(ns("show_secure"), "Show secure report", class = "btn-success"),
          shiny::helpText("Generate a short-lived embed token server-side (Azure AD / service principal) and pass it here.")
        )
      ),
      shiny::column(
        8,
        shiny::uiOutput(ns("pbi_view"))
      )
    ),
    # Load Microsoft Power BI JS client (only needed for secure mode)
    htmltools::tags$head(
      htmltools::tags$script(src = "https://cdn.jsdelivr.net/npm/powerbi-client@2.23.1/dist/powerbi.js")
    ),
    htmltools::tags$style(HTML(".pbi-container{width:100%;border:1px solid #e5e7eb;border-radius:12px;overflow:hidden;}"))
  )
}

VizServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- PUBLIC: simple iframe from Publish to web ----
    observeEvent(input$show_public, {
      url <- trimws(input$public_url %||% "")
      h   <- as.integer(input$public_h %||% 780)
      validate(shiny::need(nzchar(url), "Paste a valid 'Publish to web' URL."))
      output$pbi_view <- shiny::renderUI({
        htmltools::div(class = "pbi-container", style = sprintf("height:%dpx;", h),
                       htmltools::tags$iframe(src = url, style = "width:100%;height:100%;border:0;", allowFullScreen = NA)
        )
      })
    })
    
    # ---- SECURE: powerbi-client with embed token ----
    observeEvent(input$show_secure, {
      embed_url <- trimws(input$embed_url %||% "")
      token     <- trimws(input$embed_token %||% "")
      report_id <- trimws(input$report_id %||% "")
      h         <- as.integer(input$secure_h %||% 780)
      validate(
        shiny::need(nzchar(embed_url), "Embed URL required."),
        shiny::need(nzchar(token),     "Embed token required."),
        shiny::need(nzchar(report_id), "Report ID required.")
      )
      output$pbi_view <- shiny::renderUI({
        htmltools::tagList(
          htmltools::div(id = ns("pbiReport"), class = "pbi-container", style = sprintf("height:%dpx;", h)),
          htmltools::tags$script(HTML(sprintf(
            "(() => {
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
})();", report_id, embed_url, token, ns("pbiReport")))
          )
        )
      })
    })
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a
