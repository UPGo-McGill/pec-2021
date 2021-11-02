#### 07 DATA CORRECTION ########################################################

#' This script is very time-consuming to run, and should be rerun when STR data
#' has changed.
#'
#' Output:
#' - `str_processed.qsm` (updated)
#'
#' Script dependencies:
#' - `09_str_processing.R`
#'
#' External dependencies:
#' - None

source("R/01_startup.R")


# Load previous data ------------------------------------------------------

qload("output/str_raw.qsm", nthreads = availableCores())

# First change: remove most is.na(booked_date) ----------------------------

set.seed(1111)

daily <-
  daily |> 
  mutate(status = if_else(
    status == "R" & date >= "2019-12-01" & date <= "2020-05-31" &
      month(date) %in% c(1, 2, 3, 5, 12) & is.na(booked_date) & 
      runif(n()) > 0.5, "A", status))
  

# Second change: remove most 0-day-gaps -----------------------------------

upgo_connect()

daily_PEC <-
  daily_remote %>%
  filter(country == "Canada", city == "Prince Edward County") %>%
  collect()

upgo_disconnect()

switch_to_A <- 
  daily_PEC |> 
  filter(start_date >= "2019-11-01", start_date <= "2020-01-31", status == "R",
         !is.na(booked_date)) |> 
  mutate(gap = start_date - booked_date, odds = runif(n())) |> 
  filter(gap <= 0, (odds < 0.5 | month(start_date) == 1)) |> 
  pull(res_ID)

daily <- 
  daily |>
  mutate(status = if_else(res_ID %in% switch_to_A, "A", status))


# Split daily file by housing ---------------------------------------------

daily_all <- 
  daily

daily <- 
  daily |> 
  filter(housing)


# Save output -------------------------------------------------------------

qsavem(property, daily, daily_all, host, file = "output/str_processed.qsm",
       nthreads = availableCores())

rm(daily_PEC, switch_to_A, exchange_rates)
