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

# ---- OBIS: normalise to a stable schema (same logic as app.R) ----
normalize_obis <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(tibble::tibble())
  need_chr <- c("scientificName", "datasetID", "basisOfRecord", "eventDate")
  need_num <- c("decimalLongitude", "decimalLatitude", "depth",
                "individualCount", "temperature", "salinity")
  for (nm in c(need_chr, need_num)) if (!nm %in% names(df)) df[[nm]] <- NA
  
  df |>
    dplyr::transmute(
      scientificName   = as.character(.data$scientificName),
      decimalLongitude = suppressWarnings(as.numeric(.data$decimalLongitude)),
      decimalLatitude  = suppressWarnings(as.numeric(.data$decimalLatitude)),
      eventDate        = safe_event_date(.data$eventDate),
      depth            = suppressWarnings(as.numeric(.data$depth)),
      temperature      = suppressWarnings(as.numeric(.data$temperature)),
      salinity         = suppressWarnings(as.numeric(.data$salinity)),
      individualCount  = suppressWarnings(as.numeric(.data$individualCount)),
      datasetID        = as.character(.data$datasetID),
      basisOfRecord    = as.character(.data$basisOfRecord)
    ) |>
    dplyr::filter(
      !is.na(.data$decimalLongitude), !is.na(.data$decimalLatitude),
      dplyr::between(.data$decimalLongitude, -180, 180),
      dplyr::between(.data$decimalLatitude,  -90,   90)
    )
}

# ---- Memoised OBIS fetcher (same method; renamed here as fetch_obis) ----
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

# ---- Colour bin palette builder (same logic as app.R) ----
make_bin_pal <- function(vals,
                         mode = c("equal","quantile","custom"),
                         cuts = c(0,5,10,20),
                         n = 7) {
  mode <- match.arg(mode)
  vals_ok <- vals[is.finite(vals)]
  if (length(vals_ok) == 0 || length(unique(vals_ok)) <= 1) {
    return(leaflet::colorNumeric("viridis", domain = vals_ok, na.color = "#F0F0F0"))
  }
  if (mode == "quantile") {
    q <- stats::quantile(vals_ok, probs = seq(0, 1, length.out = n), na.rm = TRUE)
    brks <- unique(as.numeric(q))
  } else if (mode == "custom") {
    brks <- sort(unique(cuts))
  } else {
    rng <- range(vals_ok, na.rm = TRUE)
    brks <- unique(pretty(rng, n = n))
  }
  brks <- sort(unique(c(-Inf, brks, Inf)))
  if (length(brks) < 3) {
    rng <- range(vals_ok, na.rm = TRUE)
    brks <- sort(unique(c(-Inf, pretty(rng, n = 4), Inf)))
  }
  leaflet::colorBin("viridis", domain = vals_ok, bins = brks, na.color = "#F0F0F0")
}

parse_cuts <- function(txt) as.numeric(strsplit(gsub("\\s+", "", txt), ",")[[1]])

year_range_from_occ <- function(sp) {
  df <- tryCatch({ robis::occurrence(scientificname = sp, fields = c("eventDate")) }, error = function(e) NULL)
  if (is.null(df) || !"eventDate" %in% names(df)) return("NA – NA")
  yrs <- suppressWarnings(lubridate::year(lubridate::ymd(df$eventDate)))
  yrs <- yrs[!is.na(yrs)]
  if (length(yrs) == 0) return("NA – NA")
  paste0(min(yrs), " – ", max(yrs))
}
