#### 13 FREH MODEL #############################################################

#' This script is moderately time-consuming to run; it should be rerun whenever
#' STR data changes.
#'
#' Output:
#' - `str_processed.qsm` (updated)
#' - `FREH_model.qsm`
#'
#' Script dependencies:
#' - `10_str_processing.R`
#'
#' External dependencies:
#' - None

source("R/01_startup.R")
library(caret)


# Load data ---------------------------------------------------------------

qload("output/str_processed.qsm", nthreads = availableCores())


# FREH_6 since strr_FREH is having issues for these arguments -------------

# The strong seasonality in PEC makes a 3 months model based on the yearly FREH
# impossible. The next loop rolls over 180 days and considers FREH on every date
# if it has more than rate * 180 days of activity.

rate <- 0.9

daily <- 
  daily %>% 
  mutate(FREH_6 = FALSE)

for (i in min(daily$date):max(daily$date)) { # Number of days
  
  print(as.Date(i, origin = "1970-01-01"))
  
  # Date range for the rolling window of 180 days.
  d_1 <- as.Date(i - 180, origin = "1970-01-01")
  d_2 <- as.Date(i, origin = "1970-01-01")
  
  freh_prop <- 
    daily %>% 
    filter(listing_type == "Entire home/apt", date > d_1, date <= d_2,
           status %in% c("A", "R")) %>% 
    count(property_ID) %>% 
    filter(n >= 180 * rate) %>% 
    pull(property_ID)
  
  # Consider as FREH, at the certain date, every properties that had 180 * rate 
  # days of activity
  daily <- 
    daily %>% 
    mutate(FREH_6 = if_else(property_ID %in% freh_prop & date == d_2, TRUE,
                            FREH_6))
  
}


# Save output -------------------------------------------------------------

qsavem(property, daily, daily_all, GH, file = "output/str_processed.qsm",
       nthreads = availableCores())
