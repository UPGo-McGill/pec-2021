#### 03 STR DATA IMPORT ########################################################

#' This script is time-consuming to run, so it should only be rerun when STR
#' data needs to be rebuilt from scratch.
#'
#' Output:
#' - `str_raw.qsm`
#'
#' Script dependencies:
#' - `02_geometry_import.R`
#'
#' External dependencies:
#' - Access to the UPGo database


source("R/01_startup.R")


# Load previous data ------------------------------------------------------

qload("output/geometry.qsm", nthreads = availableCores())


# Get data ----------------------------------------------------------------

upgo_connect(daily_inactive = TRUE)

property <-
  property_remote %>%
  filter(country == "Canada", city == "Prince Edward County") %>%
  collect() %>%
  strr_as_sf(32618) %>%
  st_filter(city)

daily <-
  daily_remote %>%
  filter(country == "Canada", city == "Prince Edward County") %>%
  collect() %>%
  strr_expand()

daily_inactive <-
  daily_inactive_remote %>%
  filter(country == "Canada", city == "Prince Edward County") %>%
  collect() %>%
  strr_expand()

host <-
  host_remote %>%
  filter(host_ID %in% !!property$host_ID) %>%
  collect() %>%
  strr_expand()

upgo_disconnect()


# Filter to city boundaries ------------------------------------------------

property <- 
  property %>% 
  st_filter(city)

daily <- daily %>% 
  filter(property_ID %in% property$property_ID)

host <- host %>% 
  filter(host_ID %in% property$host_ID)


# Manually fix wonky created dates ----------------------------------------

property <-
  property %>%
  mutate(created = if_else(is.na(created), first_active, created),
         scraped = if_else(is.na(scraped), last_active, scraped)) %>%
  filter(!is.na(created))


# Convert currency --------------------------------------------------------

exchange_rates <- convert_currency(start_date = min(daily$date),
                                   end_date = max(daily$date))

daily <-
  daily %>%
  mutate(year_month = substr(date, 1, 7)) %>%
  left_join(exchange_rates) %>%
  mutate(price = price * exchange_rate) %>%
  select(-year_month, -exchange_rate)


# Process the property and daily files ------------------------------------

# Run raffle to assign a DA to each listing
property <-
  property %>%
  strr_raffle(DA, GeoUID, dwellings, seed = 1)

# Add area to property file
property <-
  property %>%
  st_join(select(EW, -dwellings, -type)) %>%
  relocate(ward, .after = GeoUID)

# Add DA and area to daily file
daily <-
  property %>%
  st_drop_geometry() %>%
  select(property_ID, GeoUID, ward) %>%
  left_join(daily, ., by = "property_ID")


# Get rid of non-housing listings -----------------------------------------

# Need to manually update housing status
property <- 
  property |> 
  select(-housing) |> 
  strr_housing() |> 
  relocate(housing, .after = last_active)

daily <- 
  daily |> 
  select(-housing) |> 
  left_join(select(st_drop_geometry(property), property_ID, housing)) |> 
  relocate(housing, .after = listing_type)

daily_all <- 
  daily

daily <- 
  daily |> 
  filter(housing)


# Save output -------------------------------------------------------------

qsavem(property, daily, daily_all, host, exchange_rates, 
       file = "output/str_raw.qsm", nthreads = availableCores())
