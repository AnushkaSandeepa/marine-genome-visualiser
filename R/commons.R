# ===== File: R/commons.R =====

POLITE_DELAY <- 0.35

# ---- Load species list from CSV/ ----
full_species <- readr::read_csv("CSV/new_final_species.csv", show_col_types = FALSE) |>
  dplyr::mutate(
    species_canonical = stringr::str_squish(.data$species_canonical),
    species_canonical = stringr::word(.data$species_canonical, 1, 2)
  )

client_species <- full_species |>
  dplyr::transmute(species = .data$species_canonical) |>
  dplyr::distinct() |>
  dplyr::arrange(.data$species)

# ---- Helpers ----
bbox_wkt <- function(w, s, e, n) {
  sprintf("POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))", w, s, e, s, e, n, w, n, w, s)
}

safe_event_date <- function(x) {
  d1 <- suppressWarnings(lubridate::ymd_hms(x, quiet = TRUE, tz = "UTC"))
  d2 <- suppressWarnings(lubridate::ymd(x, quiet = TRUE))
  d3 <- suppressWarnings(lubridate::ymd(substr(x, 1, 10), quiet = TRUE))
  out <- d1; out[is.na(out)] <- d2[is.na(out)]; out[is.na(out)] <- d3[is.na(out)]
  as.Date(out)
}

# Standard empty schema used everywhere to avoid missing-column warnings
empty_obis_norm <- function() {
  tibble::tibble(
    scientificName   = character(),
    decimalLongitude = numeric(),
    decimalLatitude  = numeric(),
    eventDate        = as.Date(character()),
    depth            = numeric(),
    temperature      = numeric(),
    salinity         = numeric(),
    individualCount  = numeric(),
    datasetID        = character(),
    basisOfRecord    = character()
  )
}

normalize_obis <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(empty_obis_norm())
  need_chr <- c("scientificName", "datasetID", "basisOfRecord", "eventDate")
  need_num <- c("decimalLongitude", "decimalLatitude", "depth", "individualCount", "temperature", "salinity")
  for (nm in c(need_chr, need_num)) if (!nm %in% names(df)) df[[nm]] <- NA
  
  out <- dplyr::transmute(
    df,
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
  )
  
  out <- dplyr::filter(
    out,
    !is.na(.data$decimalLongitude), !is.na(.data$decimalLatitude),
    dplyr::between(.data$decimalLongitude, -180, 180),
    dplyr::between(.data$decimalLatitude,  -90,   90)
  )
  if (nrow(out) == 0) return(empty_obis_norm())
  out
}
