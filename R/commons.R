POLITE_DELAY <- 0.35

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(robis)
  library(worrms)
  library(memoise)
})

# ---------- Load species list from CSV ----------
full_species <- readr::read_csv("CSV/new_final_species.csv", show_col_types = FALSE) |>
  dplyr::mutate(
    species_canonical = stringr::str_squish(.data$species_canonical),
    species_canonical = stringr::word(.data$species_canonical, 1, 2)
  )

client_species <- full_species |>
  dplyr::transmute(species = .data$species_canonical) |>
  dplyr::distinct() |>
  dplyr::arrange(.data$species)

# ---------- Helpers ----------
normalize_name <- function(x) {
  stringr::str_replace_all(x, "[[:space:]]+", " ") |>
    stringr::str_trim() |>
    (\(s) stringr::str_replace(s, "^([A-Z][a-zA-Z-]+\\s+[a-zA-Z-]+).*", "\\1"))() |>
    tolower()
}

guess_col <- function(df, patterns) {
  nms <- names(df)
  for (p in patterns) {
    hit <- nms[stringr::str_detect(tolower(nms), p)][1]
    if (!is.na(hit)) return(hit)
  }
  NA_character_
}

# ---------- Build lookups from your CSV ----------
COL_APHIA <- guess_col(full_species, c("^aphia", "aphiaid", "worms"))
COL_NCBI  <- guess_col(full_species, c("^ncbi", "taxon", "taxid"))
COL_NAME  <- guess_col(full_species, c("scientific", "^species", "canonical"))

sp_lookup <- full_species |>
  dplyr::transmute(
    aphia_id = suppressWarnings(as.integer(.data[[COL_APHIA]])),
    ncbi_id  = suppressWarnings(as.integer(.data[[COL_NCBI]])),
    sci_name = .data[[COL_NAME]],
    sci_name_std = normalize_name(.data[[COL_NAME]])
  )

SYN_COLS <- names(full_species)[stringr::str_detect(tolower(names(full_species)), "synonym")]
syn_long <- if (length(SYN_COLS)) {
  full_species |>
    dplyr::select(dplyr::all_of(c(COL_APHIA, SYN_COLS))) |>
    dplyr::rename(aphia_id = !!COL_APHIA) |>
    tidyr::pivot_longer(cols = dplyr::all_of(SYN_COLS), values_to = "synonyms", names_to = "syn_col") |>
    dplyr::filter(!is.na(.data$synonyms), .data$synonyms != "") |>
    dplyr::mutate(synonyms = stringr::str_split(.data$synonyms, "\\s*[,;|/]\\s*")) |>
    tidyr::unnest(synonyms) |>
    dplyr::mutate(
      synonym_std = normalize_name(.data$synonyms),
      aphia_id = suppressWarnings(as.integer(.data$aphia_id))
    ) |>
    dplyr::filter(!is.na(.data$aphia_id), .data$synonym_std != "") |>
    dplyr::distinct(.data$aphia_id, .data$synonym_std)
} else {
  tibble::tibble(aphia_id = integer(), synonym_std = character())
}

# ---------- Cached external lookups ----------
wm_records_name_cached <- memoise::memoise(function(name) {
  suppressWarnings(worrms::wm_records_name(name, marine_only = FALSE))
})

fetch_obis_cached <- memoise::memoise(function(aphia_id, fields = NULL, ...) {
  robis::occurrence(taxonid = aphia_id, fields = fields, ...)
})

# ---------- Resolver: any input -> AphiaID ----------
resolve_to_aphia <- function(q) {
  if (is.null(q) || is.na(q) || q == "") return(NA_integer_)
  q <- stringr::str_squish(q)
  
  # pattern like "... [Aphia: 12345]" or "aphia:12345"
  m_lbl <- stringr::str_match(tolower(q), "aphia\\s*[:\\]]\\s*(\\d+)")
  if (!all(is.na(m_lbl))) return(suppressWarnings(as.integer(m_lbl[2])))
  
  # explicit aphia:/ncbi:
  m <- stringr::str_match(tolower(q), "^(aphia|ncbi)\\s*:\\s*(\\d+)$")
  if (!all(is.na(m))) {
    if (m[2] == "aphia") return(suppressWarnings(as.integer(m[3])))
    if (m[2] == "ncbi") {
      hit <- sp_lookup |> dplyr::filter(.data$ncbi_id == as.integer(m[3])) |> dplyr::slice_head(n = 1)
      return(if (nrow(hit)) hit$aphia_id[[1]] else NA_integer_)
    }
  }
  
  # bare number → try csv Aphia/NCBI; else treat as Aphia
  if (stringr::str_detect(q, "^[0-9]+$")) {
    hit <- sp_lookup |>
      dplyr::filter(.data$aphia_id == as.integer(q) | .data$ncbi_id == as.integer(q)) |>
      dplyr::slice_head(n = 1)
    return(if (nrow(hit)) hit$aphia_id[[1]] else suppressWarnings(as.integer(q)))
  }
  
  # name exact in CSV
  q_std <- normalize_name(q)
  hit <- sp_lookup |> dplyr::filter(.data$sci_name_std == q_std) |> dplyr::slice_head(n = 1)
  if (nrow(hit)) return(hit$aphia_id[[1]])
  
  # name is a synonym in CSV
  srow <- syn_long |> dplyr::filter(.data$synonym_std == q_std) |> dplyr::slice_head(n = 1)
  if (nrow(srow)) return(srow$aphia_id[[1]])
  
  # WoRMS fallback (accepted/valid AphiaID)
  recs <- wm_records_name_cached(q)
  if (is.data.frame(recs) && nrow(recs)) {
    recs <- dplyr::mutate(recs,
                          chosen_aphia = dplyr::coalesce(.data$valid_AphiaID, .data$AphiaID),
                          is_accepted  = tolower(.data$status) == "accepted"
    )
    best <- recs |> dplyr::arrange(dplyr::desc(.data$is_accepted)) |> dplyr::slice_head(n = 1)
    return(suppressWarnings(as.integer(best$chosen_aphia[[1]])))
  }
  
  NA_integer_
}

# ---------- Find the CSV row for a query (name or ID) ----------
match_species_row <- function(q) {
  if (is.null(q) || is.na(q) || q == "") return(tibble::tibble())
  q <- stringr::str_squish(q)
  
  # by canonical name
  row <- full_species |> dplyr::filter(.data$species_canonical == q) |> dplyr::slice(1)
  if (nrow(row)) return(row)
  
  # by AphiaID after resolving
  aph <- resolve_to_aphia(q)
  if (!is.na(aph) && !is.na(COL_APHIA)) {
    row <- full_species |> dplyr::filter(.data[[COL_APHIA]] == aph) |> dplyr::slice(1)
    if (nrow(row)) return(row)
  }
  
  # by explicit NCBI/bare number
  m <- stringr::str_match(tolower(q), "ncbi\\s*:\\s*(\\d+)")
  if (!all(is.na(m)) && !is.na(COL_NCBI)) {
    row <- full_species |> dplyr::filter(.data[[COL_NCBI]] == as.integer(m[2])) |> dplyr::slice(1)
    if (nrow(row)) return(row)
  }
  if (stringr::str_detect(q, "^[0-9]+$")) {
    row <- full_species |>
      dplyr::filter(.data[[COL_APHIA]] == as.integer(q) | .data[[COL_NCBI]] == as.integer(q)) |>
      dplyr::slice(1)
    if (nrow(row)) return(row)
  }
  
  tibble::tibble()
}

# ---------- Collapse all synonym* columns in a CSV row ----------
collapse_synonyms <- function(row) {
  syn_cols <- names(full_species)[grepl("synonym", tolower(names(full_species)))]
  if (!length(syn_cols) || nrow(row) == 0) return("None")
  syns <- row |>
    dplyr::select(dplyr::all_of(syn_cols)) |>
    tidyr::pivot_longer(dplyr::everything(), values_to = "syn", names_to = "col") |>
    dplyr::filter(!is.na(.data$syn), .data$syn != "") |>
    dplyr::mutate(syn = stringr::str_split(.data$syn, "\\s*[,;|/]\\s*")) |>
    tidyr::unnest(syn) |>
    dplyr::mutate(syn = stringr::str_squish(.data$syn)) |>
    dplyr::distinct(.data$syn) |>
    dplyr::pull(.data$syn)
  if (!length(syns)) "None" else paste(syns, collapse = "; ")
}

# ---------- OBIS by free text (always taxonid) ----------
get_obis_by_query <- function(q,
                              fields   = c("scientificName","decimalLongitude","decimalLatitude","eventDate","basisOfRecord","depth","individualCount"),
                              geometry = NULL, startdate = NULL, enddate = NULL) {
  aph <- resolve_to_aphia(q)
  if (is.na(aph)) return(tibble::tibble())
  fetch_obis_cached(
    aphia_id = aph, fields = fields,
    geometry = geometry,
    startdate = if (!is.null(startdate)) as.Date(startdate) else NULL,
    enddate   = if (!is.null(enddate))   as.Date(enddate)   else NULL
  )
}

# ---------- Other shared helpers ----------
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
