# ===== R/commons.R =====

# polite pause between remote calls
POLITE_DELAY <- 0.35

# ---- Load species list from CSV/ ----
full_species <- readr::read_csv("CSV/new_final_species.csv") |>
  dplyr::mutate(
    species_canonical = stringr::str_squish(species_canonical),
    # keep only "Genus species" from longer names
    species_canonical = stringr::word(species_canonical, 1, 2)
  )

client_species <- full_species |>
  dplyr::transmute(species = species_canonical) |>
  dplyr::distinct() |>
  dplyr::arrange(species)

# ---- Helpers ----
bbox_wkt <- function(w, s, e, n) {
  sprintf("POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
          w, s, e, s, e, n, w, n, w, s)
}

safe_event_date <- function(x) {
  d1 <- suppressWarnings(lubridate::ymd_hms(x, quiet = TRUE, tz = "UTC"))
  d2 <- suppressWarnings(lubridate::ymd(x, quiet = TRUE))
  d3 <- suppressWarnings(lubridate::ymd(substr(x, 1, 10), quiet = TRUE))
  out <- d1
  out[is.na(out)] <- d2[is.na(out)]
  out[is.na(out)] <- d3[is.na(out)]
  as.Date(out)
}

# Memoised OBIS fetcher
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
