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
#' - Listings scraped from Kijiji and Craigslist with upgo::upgo_scrape_kj and
#'   upgo::upgo_scrape_cl

source("R/01_startup.R")
library(matchr)
library(foreach)
library(doParallel)
doParallel::registerDoParallel()
library(progressr)
handlers(global = TRUE)


# Load previous data ------------------------------------------------------

qload("output/str_raw.qsm", nthreads = availableCores())
rm(daily, host, exchange_rates)


# Specify location on drive to download photos ----------------------------

dl_location <- "/Volumes/Data 2/Scrape photos/toronto" #PEC


# Get AB image URLs -------------------------------------------------------

# Get AB urls
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

foreach(i = seq_along(ab_urls)) %dopar% {
  try(download.file(ab_urls[[i]], paste0(
    dl_location, "/ab/", ab_ids[[i]], "-", seq_along(ab_urls[[i]]), ".jpg")))
}


# Get new paths -----------------------------------------------------------

ab_paths <- list.files(paste0(dl_location, "/ab"), full.names = TRUE)

rm(dl_location, ab_urls, ab_ids)


# Get signatures ----------------------------------------------------------

# Add to previous sigs
qload("output/img_sigs_old.qsm")

ab_sigs_new <-
  create_signature(ab_paths[!ab_paths %in% vctrs::field(ab_sigs, "file")])
ab_sigs <- c(ab_sigs, ab_sigs_new)
qsavem(ab_sigs, file = "output/img_sigs.qsm", nthreads = availableCores())

rm(ab_paths)
rm(ab_sigs_new)


# Match images ------------------------------------------------------------

ab_matrix <- match_signatures(ab_sigs)
ab_matches <- identify_matches(ab_matrix)
ab_matches_new <- ab_matches

ab_matches <-
  ab_matches_new %>%
  anti_join(ab_matches, by = c("x_sig", "y_sig")) %>%
  mutate(confirmed = FALSE) %>%
  bind_rows(ab_matches)

ab_changes <- compare_images(ab_matches)
ab_matches <- integrate_changes(ab_matches, ab_changes)

qsavem(ab_matches, file = "output/matches_raw.qsm", nthreads = availableCores())
rm(ab_matrix)

qload("output/matches_raw_old.qsm", nthreads = 32)



# Save output -------------------------------------------------------------

qsavem(ab_matches, file = "output/matches_raw.qsm",
       nthreads = availableCores())
qsavem(ab_changes, file = "output/match_changes.qsm",
       nthreads = availableCores())
