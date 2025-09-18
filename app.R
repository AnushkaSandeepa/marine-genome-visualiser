library(shiny)
library(bslib)

# --- Navbar content (your modules are assumed to be sourced already) ---
app_ui <- bslib::page_navbar(
  title = tagList(
    tags$img(src = "navbar-icon.png", height = 40,
             style = "margin-right:10px; vertical-align:middle; border-radius:6px;")
  ),
  bg = "#25a5c3",
  bslib::nav_panel("Map Search",  MapSearchUI("map")),
  bslib::nav_panel("ID Search",   IDSearchUI("idsearch")),
  bslib::nav_panel("Name Search", NameSearchUI("names")),   # <- NEW
  bslib::nav_panel("Visualizations", VizUI("viz"))
)


ui <- tagList(
  # ---------- HEAD: only links, styles, scripts ----------
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "app-icon.png"),
    tags$link(rel = "apple-touch-icon", href = "app-icon.png"),
    
    # Navbar link colors / states
    tags$style(HTML("
      .navbar .nav-link { color: #ffffff !important; }
      .navbar .navbar-brand { color: #111827 !important; }
      .navbar .nav-link:hover, .navbar .nav-link:focus {
        color: #111827 !important; text-decoration: none;
      }
      .navbar .nav-link.active, .navbar .nav-link.show, .navbar .nav-item.show > .nav-link {
        color: #0b1220 !important; font-weight: 600;
      }
      .navbar .dropdown-menu .dropdown-item:hover,
      .navbar .dropdown-menu .dropdown-item:focus {
        color: #111827 !important; background-color: #e5e7eb !important;
      }
    ")),
    
    # Splash styles
    tags$style(HTML("
      html, body { height: 100%; }
      body {
        min-height: 100vh;
        display: flex;
        flex-direction: column;
      }
      /* App content stretches to fill available space */
      #app-content {
        flex: 1 0 auto;         /* take remaining space */
        visibility: hidden;     /* you already toggle this via splash JS */
        display: block;
      }
      /* Keep footer at the very bottom, without overlapping content */
      .app-footer {
        flex-shrink: 0;         /* don't let it collapse */
        margin-top: auto;       /* push it to the bottom */
      }
      #splash {
        position: fixed; inset: 0;
        background: #001a2a url('ocean-genomics-dna.png') center/cover no-repeat; /* in www/ */
        display: flex; align-items: center; justify-content: center;
        z-index: 99999;
        opacity: 1; transition: opacity 600ms ease;
      }
      #splash .splash-brandmark {
        width: clamp(56px, 12vw, 96px);
        height: auto;
        filter: drop-shadow(0 6px 16px rgba(0,0,0,.35));
        opacity: .98; transition: transform 350ms ease, opacity 350ms ease;
      }
      #splash .splash-brandmark:hover { transform: scale(1.04); opacity: 1; }

      #splash .brand {
        text-align: center; color: #e6f7ff;
        font-family: system-ui, -apple-system, Segoe UI, Roboto, sans-serif;
        display: flex; flex-direction: column; align-items: center; gap: 10px;
        transform: translateY(-10vh);
      }
      #splash .brand h1 {
        font-size: clamp(28px, 5vw, 56px);
        letter-spacing: 0.06em; margin: 0 0 8px 0;
        text-shadow: 0 6px 30px rgba(0,0,0,.45);
      }
      #splash .brand p { margin: 0; opacity: .9; font-size: clamp(14px, 2.2vw, 18px); }
      #splash .glow {
        width: 120px; height: 2px; margin: 18px auto 0;
        background: linear-gradient(90deg, transparent, #9be7ff, transparent);
        animation: shimmer 2.2s infinite;
      }
      @keyframes shimmer {
        0% { opacity: .2; transform: translateX(-30px); }
        50% { opacity: 1; transform: translateX(30px); }
        100% { opacity: .2; transform: translateX(-30px); }
      }

      /* footer logo inside splash */
      #splash .splash-footer {
        position: absolute; left: 0; right: 0; bottom: 150px;
        display: flex; align-items: center; justify-content: center; padding: 0 16px;
      }
      #splash .splash-logo {
        width: clamp(120px, 28vw, 240px); height: auto;
        filter: drop-shadow(0 6px 16px rgba(0,0,0,.35));
        opacity: .95; transition: transform 400ms ease, opacity 400ms ease;
      }
      #splash .splash-logo:hover { transform: scale(1.03); opacity: 1; }

      /* reveal timing */
      #app-content { visibility: hidden; }
      #splash.fade-out { opacity: 0; pointer-events: none; }

      /* site footer */
      .app-footer {
        background:#0f4f60; color:#e6f7ff; padding:14px 18px;
        border-top:1px solid rgba(255,255,255,.15); font-size:14px;
      }
      .app-footer a { color:#c9f1ff; text-decoration:none; }
      .app-footer a:hover { color:#ffffff; text-decoration:underline; }
      .app-footer .wrap {
        max-width: 1200px; margin: 0 auto;
        display:flex; gap:16px; align-items:center; justify-content:space-between; flex-wrap:wrap;
      }
      .app-footer .left, .app-footer .center, .app-footer .right {
        display:flex; align-items:center; gap:10px;
      }
      .app-footer .sep { opacity:.5; margin:0 6px; }
      @media (max-width: 768px){
        .app-footer .wrap { justify-content:center; text-align:center; }
      }
      app-footer .icon-link {
        display: inline-flex; align-items: center; gap: 6px;
      }
      .app-footer .icon {
        width: 18px; height: 18px; display: inline-block;
        color: #c9f1ff; transition: color .2s ease, transform .2s ease;
        vertical-align: middle;
      }
      .app-footer .icon-link:hover .icon { color: #ffffff; transform: translateY(-1px); }
    ")),
    
    # Splash JS (only controls splash visibility)
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function () {
        const splash = document.getElementById('splash');
        const app = document.getElementById('app-content');
        let ready = false;
        function reveal() {
          if (ready) return;
          ready = true;
          if (splash) {
            splash.classList.add('fade-out');
            setTimeout(() => { if (splash.parentNode) splash.remove(); app.style.visibility = 'visible'; }, 650);
          } else {
            app.style.visibility = 'visible';
          }
        }
        setTimeout(reveal, 4000);
      });
    "))
  ),
  
  # ---------- SINGLE Splash overlay ----------
  div(
    id = "splash",
    div(class = "brand",
        tags$img(src = "app-icon.png", alt = "Ocean Genome Explorer logo", class = "splash-brandmark"),
        h1("Ocean Genome Explorer"),
        p("Integrating OBIS • WoRMS • NCBI • FishBase"),
        div(class = "glow")
    ),
    div(class = "splash-footer",
        tags$img(src = "UWA-Oceans.png", alt = "Client / Project Logo", class = "splash-logo")
    )
  ),
  
  # ---------- App content (hidden until splash fades) ----------
  div(id = "app-content", app_ui),
  
  # ---------- Site footer (outside head, once) ----------
  tags$footer(
    class = "app-footer",
    div(class="wrap",
        div(class="left",  HTML(sprintf("&copy; %s Design & Developed by UWA Data Science Students", format(Sys.Date(), "%Y")))),
        div(class="right",
           
            
            # Website (globe)
            a(href = "https://www.uwa.edu.au/oceans-institute",
              target = "_blank", rel = "noopener", class = "icon-link", `aria-label` = "Website",
              # inline SVG: globe
              tags$svg(class = "icon", viewBox = "0 0 24 24", fill = "currentColor",
                       tags$path(d = "M12 2a10 10 0 1 0 0 20a10 10 0 0 0 0-20zm0 2a8 8 0 0 1 5 1.78c-.7.3-1.5.63-2.37.78c-1.08.2-1.63-.12-2.38-.62c-.7-.47-1.58-1.06-3.25-1.06c-.83 0-1.55.14-2.17.34A7.98 7.98 0 0 1 12 4zm-6.32 3.2c.72.58 1.72 1.2 3.32 1.2c1.34 0 2.13-.5 2.86-.98c.64-.43 1.23-.82 2.14-.82c.57 0 1.32.17 2.52.45c.53.12 1.02.2 1.48.25c.38.88.6 1.85.6 2.86c0 .31-.02.61-.06.9c-1.47.49-2.35.61-2.94.36c-.72-.31-1.14-1.11-1.77-2.32c-.36-.69-.8-1.53-1.45-1.53c-.7 0-1.09.58-1.7 1.5c-.66.98-1.56 2.3-3.98 2.3c-1.6 0-2.74-.45-3.52-.96a8.1 8.1 0 0 1 .52-2.21zM4.1 12.5c.86.38 1.98.7 3.4.7c2.03 0 3.19-.76 3.95-1.56c.7-.74 1.16-1.54 1.84-1.54c.62 0 .98.5 1.64 1.73c.64 1.2 1.1 1.9 1.87 2.23c.47.2 1.1.27 2.12.07c-.87 2.83-3.52 4.87-6.82 4.87c-3.45 0-6.34-2.32-7.01-5.5z")
              ),
              span("UWA Oceans Institute")   # keep or remove if you want icon-only
            ),
            
            # Facebook
            a(href = "https://www.facebook.com/UWAOceansInstitute",
              target = "_blank", rel = "noopener", class = "icon-link", `aria-label` = "Facebook",
              # inline SVG: facebook 'f'
              tags$svg(class = "icon", viewBox = "0 0 24 24", fill = "currentColor",
                       tags$path(d = "M22 12.06C22 6.48 17.52 2 11.94 2S2 6.48 2 12.06c0 5.04 3.68 9.22 8.48 9.94v-7.03H7.9v-2.9h2.58V9.73c0-2.55 1.52-3.96 3.85-3.96c1.12 0 2.29.2 2.29.2v2.52h-1.29c-1.27 0-1.67.79-1.67 1.6v1.93h2.84l-.45 2.9h-2.39V22c4.8-.72 8.48-4.9 8.48-9.94z")
              ),
              span("Facebook")               # keep or remove for icon-only
            )
        )
        
    )
  )
)

server <- function(input, output, session) {
  MapSearchServer("map")
  IDSearchServer("idsearch")
  NameSearchServer("names")   # <- NEW
  VizServer("viz")
}


shinyApp(ui, server)


