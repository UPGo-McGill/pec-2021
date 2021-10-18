#### 03 REGISTRATION DATA IMPORT ########################################################

#' Output:
#' - `home_sales.qs`
#'
#' Script dependencies:
#' - none

source("R/01_startup.R")


# Import home sales -------------------------------------------------------

home_sales <- 
  read_xlsx("data/home_sales.xlsx") |>
  set_names(c("MLS", "PIN", "address", "ward", "list_price", "sale_price",
              "listing_date", "sale_date", "DOM", "new_con",
              "water")) |> 
  mutate(sale_date = as.Date(sale_date),
         listing_date = sale_date - DOM,
         new_con = if_else(new_con == "Yes", TRUE, FALSE),
         water = if_else(water == "Yes", TRUE, FALSE))


# Uniform street address --------------------------------------------------

home_sales <- 
  home_sales %>%
  mutate(address = str_to_lower(address), 
         address = str_remove(address, "\\.")) %>% 
  mutate(address = str_replace(address, " road", " rd"),
         address = str_replace(address, " street", " st"),
         address = case_when(str_detect(address, "\\d road") ~ str_remove(address, " road$"),
                             TRUE ~ address),# ROAD 3 ROAD, take the second rd off...
         address = str_replace(address, " crescent", " cr"),
         address = str_replace(address, " drive", " dr"),
         address = str_replace(address, " lane", " ln"),
         address = str_replace(address, " highway", " hwy"),
         address = str_replace(address, " parkway", " pkwy"),
         address = str_replace(address, " avenue", " ave"),
         address = str_replace_all(address, "[[:space:]]+", " "),
         address = case_when(str_detect(address, "loyalist pkwy") ~ str_extract(address, "^\\d* loyalist pkwy"),
                             TRUE ~ address),# ROAD 3 ROAD, take the second rd off...
         address = str_remove(address, " n$"),
         address = str_remove(address, " highway$")) %>% 
  mutate(address = str_trim(str_to_lower(address))) 


# Geocode -----------------------------------------------------------------

home_sales <- 
  home_sales %>% 
  mutate(address = as.character(str_glue("{address}, prince edward county"))) %>%
  ggmap::mutate_geocode(location = address) %>%
  mutate(address = str_remove(address, ", prince edward county"))

home_sales <- 
  home_sales %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(32617)


# Save --------------------------------------------------------------------

qsave(home_sales, file = "output/home_sales.qs")
