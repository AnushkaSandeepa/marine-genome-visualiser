# ===== File: R/commons.R =====

# polite pause between remote calls (helps avoid hammering APIs)
POLITE_DELAY <- 0.35

# ---- Client species list ----
client_species <- tibble::tribble(
  ~common_name, ~species,
  "Blackspot snapper", "Lutjanus fulviflamma",
  "Stripey snapper", "Lutjanus carponotatus",
  "Brownstripe snapper", "Lutjanus vitta",
  "Moses snapper", "Lutjanus russellii",
  "Grass emperor", "Lethrinus laticaudis",
  "Fivelined snapper", "Lutjanus quinquelineatus",
  "Chinamanfish", "Symphorus nematophorus",
  "Red emperor", "Lutjanus sebae",
  "Darktail snapper", "Lutjanus lemniscatus",
  "Spangled emperor", "Lethrinus nebulosus",
  "Sharptooth snapper", "Pristipomoides typus",
  "Crimson snapper", "Lutjanus erythropterus",
  "Goldflag jobfish", "Pristipomoides auricilla",
  "Ruby snapper", "Etelis carbunculus",
  "Flame snapper", "Etelis coruscans",
  "Oblique-banded snapper", "Pristipomoides zonatus",
  "Smalltooth jobfish", "Aphareus furca",
  "Blue fusilier", "Caesio teres",
  "Rusty jobfish", "Aphareus rutilans",
  "Blacktail snapper", "Lutjanus fulvus",
  "Red bass", "Lutjanus bohar",
  "Green jobfish", "Aprion virescens",
  "Midnight snapper", "Macolor macularis",
  "Mangrove jack", "Lutjanus argentimaculatus"
) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), stringr::str_squish))

# ---- Helpers ----
bbox_wkt <- function(w, s, e, n) {
  sprintf(
    "POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
    w, s, e, s, e, n, w, n, w, s
  )
}

# Robust date parser for OBIS eventDate
safe_event_date <- function(x) {
  d1 <- suppressWarnings(lubridate::ymd_hms(x, quiet = TRUE, tz = "UTC"))
  d2 <- suppressWarnings(lubridate::ymd(x, quiet = TRUE))
  d3 <- suppressWarnings(lubridate::ymd(substr(x, 1, 10), quiet = TRUE))
  out <- d1
  out[is.na(out)] <- d2[is.na(out)]
  out[is.na(out)] <- d3[is.na(out)]
  as.Date(out)
}

# Memoised OBIS fetcher to avoid repeated network calls
.fetch_obis <- function(species, wkt = NULL, start = NULL, end = NULL) {
  Sys.sleep(POLITE_DELAY)
  robis::occurrence(
    scientificname = species,
    geometry = wkt,
    startdate = if (!is.null(start)) as.Date(start) else NULL,
    enddate   = if (!is.null(end))   as.Date(end)   else NULL
  )
}

fetch_obis <- memoise::memoise(.fetch_obis)
