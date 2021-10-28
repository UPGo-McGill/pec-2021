#### 09 IMAGE MATCH ############################################################

#' This script is extremely time-consuming and memory-intensive to run, so it
#' should only be rerun when image matching needs to be rebuilt from scratch. In
#' addition, the script downloads hundreds of thousands of photos to a specified
#' folder, so it requires approximately 50 GB of free storage space.
#'
#' Output:
#' - `img_sigs.qsm`
#' - `matches_raw.qsm`
#' - `match_changes.qsm`
#'
#' Script dependencies:
#' - ``
#'
#' External dependencies:
#' - Access to the UPGo database

source("R/01_startup.R")
library(matchr)
library(progressr)
handlers(global = TRUE)


# Load previous data ------------------------------------------------------

qload("output/str_raw.qsm", nthreads = availableCores())
rm(daily, daily_all, host, exchange_rates)


# Specify location on drive to download photos ----------------------------

dl_location <- "/Volumes/Data 2/Scrape photos/pec" #PEC


# Get image URLs ----------------------------------------------------------

# Get urls
ab_urls <-
  property %>%
  mutate(urls = if_else(is.na(ab_image_url), ha_image_url, ab_image_url)) %>%
  pull(urls) %>%
  str_replace('(?<=(jpg|JPEG|jpeg|JPG)).*', '')

# Get AB IDs
ab_ids <- property$property_ID

# Remove already downloaded images
ab_paths <-
  list.files(paste0(dl_location, "/ab")) %>%
  str_remove(".(jpg|jpeg|JPG|JPEG)") %>%
  str_remove("-[:digit:]$")

ab_urls <- ab_urls[!ab_ids %in% ab_paths]
ab_ids <- ab_ids[!ab_ids %in% ab_paths]


# Make download subfolders ------------------------------------------------

if (!dir.exists(dl_location)) dir.create(dl_location,
                                               recursive = TRUE)

if (!dir.exists(paste0(dl_location, "/ab"))) {
  dir.create(paste0(dl_location, "/ab"))
}


# Download images ---------------------------------------------------------

download_images(destination = paste0(dl_location, "/ab/"),
                path = ab_urls, id = ab_ids)


# Get new paths -----------------------------------------------------------

ab_paths <- list.files(paste0(dl_location, "/ab"), full.names = TRUE)
rm(dl_location, ab_urls, ab_ids)


# Get signatures ----------------------------------------------------------

ab_sigs <- create_signature(ab_paths)
qsave(ab_sigs, file = "output/img_sigs.qs", nthreads = availableCores())
rm(ab_paths)


# Match images ------------------------------------------------------------

ab_matrix <- match_signatures(ab_sigs)
ab_matches <- identify_matches(ab_matrix)
ab_changes <- confirm_matches(ab_matches)
ab_matches <- integrate_changes(ab_matches, ab_changes)

qsave(ab_matches, file = "output/matches_raw.qs", nthreads = availableCores())
rm(ab_matrix)
qsave(ab_changes, file = "output/match_changes.qs", nthreads = availableCores())
