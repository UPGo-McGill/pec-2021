#### Get other cities

source("R/01_startup.R")
library(future)
plan(multisession)

upgo_connect()

prop_CT <- 
  property_remote |> 
  filter(country == "Canada", region == "Prince Edward Island", city == "Charlottetown") |> 
  collect()

daily_CT <- 
  daily_remote |> 
  filter(property_ID %in% !!prop_CT$property_ID) |> 
  collect()

CD_CB <- 
  cancensus::get_census("CA16", list(PR = "12"), "CD", geo_format = "sf") |> 
  as_tibble() |> 
  st_as_sf() |> 
  filter(GeoUID %in% c(1215, 1216, 1217)) |> 
  st_transform(4326)

PID_CB <- 
  property_remote |> 
  filter(country == "Canada", region == "Nova Scotia") |> 
  collect() |> 
  strr_as_sf() |> 
  st_filter(CD_CB) |>
  pull(property_ID)

prop_CB <- 
  property_remote |> 
  filter(property_ID %in% !!PID_CB) |> 
  collect()

daily_CB <- 
  daily_remote |> 
  filter(property_ID %in% !!prop_CB$property_ID) |> 
  collect()

prop_TF <- 
  property_remote |> 
  filter(country == "Canada", region == "British Columbia", city == "Tofino") |> 
  collect()

daily_TF <- 
  daily_remote |> 
  filter(property_ID %in% !!prop_TF$property_ID) |> 
  collect()

prop_WL <- 
  property_remote |> 
  filter(country == "Canada", region == "British Columbia", city == "Whistler") |> 
  collect()

daily_WL <- 
  daily_remote |> 
  filter(property_ID %in% !!prop_WL$property_ID) |> 
  collect()

prop_MT <-
  property_remote |> 
  filter(country == "Canada", region == "Québec", city == "Mont-Tremblant") |> 
  collect()

daily_MT <- 
  daily_remote |> 
  filter(property_ID %in% !!prop_MT$property_ID) |> 
  collect()

prop_BM <-
  property_remote |>
  filter(country == "Canada", region == "Ontario", city == "The Blue Mountains") |> 
  collect()

daily_BM <- 
  daily_remote |> 
  filter(property_ID %in% !!prop_BM$property_ID) |> 
  collect()

CD_GP <- 
  cancensus::get_census("CA16", list(PR = "24"), "CD", geo_format = "sf") |> 
  as_tibble() |> 
  st_as_sf() |> 
  filter(GeoUID %in% c(2401, 2402, 2403, 2404, 2405, 2406, 2407, 2408)) |> 
  st_transform(4326)

PID_GP <- 
  property_remote |> 
  filter(country == "Canada", region == "Québec") |> 
  collect() |> 
  strr_as_sf() |> 
  st_filter(CD_GP) |>
  pull(property_ID)

prop_GP <- 
  property_remote |> 
  filter(property_ID %in% !!PID_GP) |> 
  collect()

daily_GP <- 
  daily_remote |> 
  filter(property_ID %in% !!prop_GP$property_ID) |> 
  collect()

upgo_disconnect()


# Process data ------------------------------------------------------------

prop_comp <- 
  bind_rows(mutate(prop_BM, location = "The Blue Mountains"),
            mutate(prop_CB, location = "Cape Breton Island"),
            mutate(prop_CT, location = "Charlottetown"),
            mutate(prop_GP, location = "Gaspésie-Iles-de-la-Madeleine"),
            mutate(prop_MT, location = "Mont-Tremblant"),
            mutate(prop_TF, location = "Tofino"),
            mutate(prop_WL, location = "Whistler")
  )

daily_comp_raw <- 
  bind_rows(daily_BM, daily_CB, daily_CT, daily_GP, daily_MT, daily_TF, daily_WL)
  
daily_comp <-
  bind_rows(mutate(strr_expand(daily_BM), location = "The Blue Mountains"),
            mutate(strr_expand(daily_CB), location = "Cape Breton Island"),
            mutate(strr_expand(daily_CT), location = "Charlottetown"),
            mutate(strr_expand(daily_GP), location = "Gaspésie-Iles-de-la-Madeleine"),
            mutate(strr_expand(daily_MT), location = "Mont-Tremblant"),
            mutate(strr_expand(daily_TF), location = "Tofino"),
            mutate(strr_expand(daily_WL), location = "Whistler")
            )

prop_comp <- 
  prop_comp |> 
  select(-housing) |> 
  strr_housing() |> 
  relocate(housing, .after = last_active)

daily_comp_raw <- 
  daily_comp_raw |> 
  select(-housing) |> 
  left_join(select(prop_comp, property_ID, housing)) |> 
  relocate(housing, .after = listing_type)

daily_comp <- 
  daily_comp |> 
  select(-housing) |> 
  left_join(select(prop_comp, property_ID, housing)) |> 
  relocate(housing, .after = listing_type)


# Apply 2019-2020 fix? ----------------------------------------------------

set.seed(1111)

daily_comp <-
  daily_comp |> 
  mutate(status = if_else(
    status == "R" & date >= "2019-12-01" & date <= "2020-05-31" &
      month(date) %in% c(1, 2, 3, 5, 12) & is.na(booked_date) & 
      runif(n()) > 0.5, "A", status))

switch_to_A_comp <- 
  daily_comp_raw |> 
  filter(start_date >= "2019-11-01", start_date <= "2020-01-31", status == "R",
         !is.na(booked_date)) |> 
  mutate(gap = start_date - booked_date, odds = runif(n())) |> 
  filter(gap <= 0, (odds < 0.5 | month(start_date) == 1)) |> 
  pull(res_ID)

daily_comp <- 
  daily_comp |>
  mutate(status = if_else(res_ID %in% switch_to_A_comp, "A", status))


# Split daily file by housing ---------------------------------------------

daily_comp <- 
  daily_comp |> 
  filter(housing)

exchange_rates <- convert_currency(start_date = min(daily_comp$date),
                                   end_date = max(daily_comp$date))

daily_comp <-
  daily_comp %>%
  mutate(year_month = substr(date, 1, 7)) %>%
  left_join(exchange_rates) %>%
  mutate(price = price * exchange_rate) %>%
  select(-year_month, -exchange_rate)


# Add PEC data ------------------------------------------------------------

qload("output/str_processed.qsm", nthreads = availableCores())

daily_comp <- 
  daily |> 
  select(property_ID:city) |> 
  mutate(location = "Prince Edward County") |> 
  bind_rows(daily_comp) |> 
  filter(date >= "2017-06-01")

qsave(daily_comp, file = "output/national_comparison.qs", 
       nthreads = availableCores())

rm(CD_CB, CD_GP, daily_BM, daily_CB, daily_comp_raw, daily_CT, daily_GP, 
   daily_MT, daily_TF, daily_WL, exchange_rates, prop_BM, prop_CB, prop_comp, 
   prop_CT, prop_GP, prop_MT, prop_TF, prop_WL, PID_CB, PID_GP, 
   switch_to_A_comp)
