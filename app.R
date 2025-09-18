library(shiny)
library(bslib)

# If you keep images in a non-www folder:
# shiny::addResourcePath("img", "Images")  # then use src="img/xxx.png"

app_ui <- bslib::page_navbar(
  title = tagList(
    tags$img(
      src = "navbar-icon.png",  # put this in www/
      height = 40,
      style = "margin-right:10px; vertical-align:middle; border-radius:6px;"
    )
    # , span("Ocean Genome Explorer")
  ),
  bg = "#25a5c3",
  bslib::nav_panel("Map Search", MapSearchUI("map")),
  bslib::nav_panel("ID Search",  IDSearchUI("idsearch")),
  bslib::nav_panel("Visualizations", VizUI("viz"))
)

ui <- tagList(
  # --- Head (favicons + styles + splash script) ---
  tags$head(
    # Favicons (files in www/)
    tags$link(rel = "icon", type = "image/png", href = "app-icon.png"),
    tags$link(rel = "apple-touch-icon", href = "app-icon.png"),
    
    # Navbar link colors / states
    tags$style(HTML("
      .navbar .nav-link { color: #374151 !important; }
      .navbar .navbar-brand { color: #111827 !important; }

      .navbar .nav-link:hover,
      .navbar .nav-link:focus {
        color: #111827 !important;
        text-decoration: none;
      }

      .navbar .nav-link.active,
      .navbar .nav-link.show,
      .navbar .nav-item.show > .nav-link {
        color: #0b1220 !important;
        font-weight: 600;
      }

      .navbar .dropdown-menu .dropdown-item:hover,
      .navbar .dropdown-menu .dropdown-item:focus {
        color: #111827 !important;
        background-color: #e5e7eb !important;
      }
    ")),
    
    # Splash styles
    tags$style(HTML("
      #splash {
        position: fixed; inset: 0;
        background: #001a2a url('ocean-genomics-dna.png') center/cover no-repeat; /* put in www/ */
        display: flex; align-items: center; justify-content: center;
        z-index: 99999;
        opacity: 1; transition: opacity 600ms ease;
      }
      #splash .splash-brandmark{
        width: clamp(56px, 12vw, 96px);   
        height: auto;
        filter: drop-shadow(0 6px 16px rgba(0,0,0,.35));
        opacity: .98;
        transition: transform 350ms ease, opacity 350ms ease;
      }
      #splash .splash-brandmark:hover{
        transform: scale(1.04);
        opacity: 1;
      }
      #splash .brand {
        text-align: center; color: #e6f7ff;
        font-family: system-ui, -apple-system, Segoe UI, Roboto, sans-serif;
        display:flex; flex-direction:column; align-items:center; gap:10px;
        transform: translateY(-10vh)
      }
      #splash .brand h1 {
        font-size: clamp(28px, 5vw, 56px);
        letter-spacing: 0.06em; margin: 0 0 8px 0;
        text-shadow: 0 6px 30px rgba(0,0,0,.45);
      }
      #splash .brand p {
        margin: 0; opacity: .9;
        font-size: clamp(14px, 2.2vw, 18px);
      }
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

      /* bottom logo area */
      #splash .splash-footer{
        position: absolute; left: 0; right: 0; bottom: 150px;
        display: flex; align-items: center; justify-content: center;
        padding: 0 16px;
      }
      #splash .splash-logo{
        width: clamp(120px, 28vw, 240px);
        height: auto;
        filter: drop-shadow(0 6px 16px rgba(0,0,0,.35));
        opacity: .95;
        transition: transform 400ms ease, opacity 400ms ease;
      }
      #splash .splash-logo:hover{ transform: scale(1.03); opacity: 1; }

      /* Reveal timing */
      #app-content { visibility: hidden; }
      #splash.fade-out { opacity: 0; pointer-events: none; }
    ")),
    
    # Splash JS
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function () {
        const splash = document.getElementById('splash');
        const app = document.getElementById('app-content');
        let ready = false;
        function reveal() {
          if (ready) return;
          ready = true;
          splash.classList.add('fade-out');
          setTimeout(() => { splash.remove(); app.style.visibility = 'visible'; }, 650);
        }
        setTimeout(reveal, 4000);
      });
    "))
  ),
  
  # --- Single Splash overlay (with footer logo) ---
  div(
    id = "splash",
    div(
      id = "splash",
      div(class = "brand",
          tags$img(
            src = "app-icon.png", 
            alt = "Ocean Genome Explorer logo",
            class = "splash-brandmark"
          ),
          h1("Ocean Genome Explorer"),
          p("Integrating OBIS • WoRMS • NCBI • FishBase"),
          div(class = "glow")
      ),
      div(
        class = "splash-footer",
        tags$img(src = "UWA-Oceans.png", alt = "Client / Project Logo", class = "splash-logo")
      )
    ),
    div(class = "brand",
        h1("Ocean Genome Explorer"),
        p("Integrating OBIS • WoRMS • NCBI • FishBase"),
        div(class = "glow")
    ),
    div(
      class = "splash-footer",
      tags$img(src = "UWA-Oceans.png", alt = "Client / Project Logo", class = "splash-logo")
    )
  ),
  
  # --- App content (hidden until splash fades) ---
  div(id = "app-content", app_ui)
)

server <- function(input, output, session) {
  MapSearchServer("map")
  IDSearchServer("idsearch")
  VizServer("viz")
}

shinyApp(ui, server)
