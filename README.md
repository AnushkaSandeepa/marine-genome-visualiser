# marine-genome-visualiser
This repository hosts the UWA Capstone Project for the Master of Data Science program. Our team is collaborating with the Minderoo OceanOmics Centre to design and develop an interactive data visualisation platform. The goal is to track and communicate global progress in generating high-quality reference genomes for marine vertebrate species.


# 1) Reinstall packages under this R so the manifest captures them
install.packages(c(
  "shiny","bslib","leaflet","leaflet.extras","robis","dplyr","purrr","lubridate",
  "DT","scales","memoise","tibble","stringr","htmltools","readr","jsonlite",
  "tidyr","httr","reticulate", "worrms"
))


# if u wanna reconnect
unlink("manifest.json")
rsconnect::writeManifest(appDir = ".")


# 2) Regenerate manifest (updates the "platform" field to 4.4.3)
install.packages("rsconnect")
rsconnect::writeManifest(
  appDir = ".",
  appMode = "shiny",
  appPrimaryDoc = "app.R",
  verbose = TRUE
)

# 3) (Optional) check
jsonlite::fromJSON("manifest.json")$platform
sessionInfo()$R.version$version.string
