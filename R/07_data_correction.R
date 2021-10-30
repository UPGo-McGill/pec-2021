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

# First change: remove is.na(booked_date) ---------------------------------

daily <- daily <- 
  daily |> 
  mutate(status = if_else(
    status == "R" & date >= "2019-12-01" & date <= "2020-03-31" &
      month(date) %in% c(2, 3, 12) & is.na(booked_date), "B", status))


# Second change: remove most 0-day-gap ------------------------------------

booking_gaps <- 
  daily |> 
  filter(!is.na(res_ID)) |> 
  group_by(res_ID) |> 
  summarize(
    start_year = year(min(date)),
    start_month = month(min(date)),
    length = max(date) - min(date) + 1,
    gap = min(date) - min(booked_date))
  
adj <-
  booking_gaps |> 
  summarize(nov_2019 = mean(gap[start_year == 2019 & start_month == 11] <= 0),
            nov_prev = mean(gap[start_year < 2019 & start_month == 11] <= 0),
            dec_2019 = mean(gap[start_year == 2019 & start_month == 12] <= 0),
            dec_prev = mean(gap[start_year < 2019 & start_month == 12] <= 0),
            jan_2020 = mean(gap[start_year == 2020 & start_month == 1] <= 0),
            jan_prev = mean(gap[start_year < 2020 & start_month == 1] <= 0)) |> 
  transmute(nov_to_drop = (nov_2019 - nov_prev) / (nov_2019 * (1 - nov_prev)),
            dec_to_drop = (dec_2019 - dec_prev) / (dec_2019 * (1 - dec_prev)),
            jan_to_drop = (jan_2020 - jan_prev) / (jan_2020 * (1 - jan_prev)))

set.seed(1111)

to_change <- 
  booking_gaps |> 
  filter(gap <= 0) |> 
  mutate(rand = runif(n()))

changed <- 
  to_change |> 
  filter((start_year == 2019 & start_month == 11 & rand <= adj$nov_to_drop) |
           (start_year == 2019 & start_month == 12 & rand <= adj$dec_to_drop) |
           (start_year == 2020 & start_month == 1 & rand <= adj$jan_to_drop))

daily <- 
  daily |> 
  mutate(status = if_else(status == "R" & res_ID %in% changed$res_ID, "B", 
                          status)) |> 
  mutate(res_ID = if_else(status != "R", NA_integer_, res_ID))


# Split daily file by housing ---------------------------------------------

daily_all <- 
  daily

daily <- 
  daily |> 
  filter(housing)


# Save output -------------------------------------------------------------

qsavem(property, daily, daily_all, host, file = "output/str_processed.qsm",
       nthreads = availableCores())

rm(adj, booking_gaps, changed, to_change)
