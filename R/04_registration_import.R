#### 03 REGISTRATION DATA IMPORT ########################################################

#' Output:
#' - `STA.qs`
#'
#' Script dependencies:
#' - `02_geometry_import.R`

source("R/01_startup.R")

# STA import ------------------------------------------------------------------

STA_raw <- readxl::read_xls("data/STA_2019_11_2020_09.xls")

STA <- 
  STA_raw %>% 
  set_names(c("id","exp_date", "address", "unit_number", "ward", "city", 
              "principal_residence", "bedrooms", "declare_date", "fee", "status")) %>% 
  mutate(exp_date = as_date(exp_date),
         declare_date = as_date(declare_date),
         address = str_c(address, ward, city, sep = ", "),
         principal_residence = ifelse(principal_residence == 0, FALSE, TRUE)) %>% 
  select(-city) 


# Save the STA file for geocoding with a google account ----------------------

qsave(STA, file = "output/STA_without_geocode.qs", nthreads = availableCores())

#  Lines to get the addresses geocoded ---------------------------------------

STA <- qread("output/STA_without_geocode.qs", nthreads = availableCores())

STA <- 
  STA %>% 
  ggmap::mutate_geocode(address)

STA <- 
  STA %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(32617)

qsave(STA, file = "output/STA.qs", nthreads = availableCores())

