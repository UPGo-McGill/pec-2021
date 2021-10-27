#### 04 REGISTRATION DATA IMPORT ###############################################

#' Output:
#' - `STA.qs`
#'
#' Script dependencies:
#' - none

source("R/01_startup.R")


# STA import --------------------------------------------------------------

STA <- 
  read_sf("data/McGill/STA.shp") %>% 
  transmute(id = LicenceID, 
            created = created_da,
            expiry = Expiry,
            address = StreetAdd,
            bedrooms = NumBeds,
            num_units = NumUnits_1,
            max_guests = MaxOccupan,
            entire_apt = WholeHome,
            status = Status) %>% 
  mutate(entire_apt = ifelse(entire_apt == 0, FALSE, TRUE)) %>% 
  st_transform(32618)


# Uniform street address --------------------------------------------------

STA <- STA %>% 
  mutate(address = str_to_lower(address), 
         address = str_remove(address, "\\.")) %>% 
  mutate(address = str_replace(address, " road", " rd"),
         address = str_replace(address, " street", " st"),
         address = case_when(str_detect(address, "\\d road") ~ 
                               str_remove(address, " road$"),
                             TRUE ~ address), # ROAD 3 ROAD, take the second rd off...
         address = str_replace(address, " crescent", " cr"),
         address = str_replace(address, " drive", " dr"),
         address = str_replace(address, " lane", " ln"),
         address = str_replace(address, " highway", " hwy"),
         address = str_replace(address, " parkway", " pkwy"),
         address = str_replace(address, " avenue", " ave"),
         address = str_replace_all(address, "[[:space:]]+", " "),
         address = case_when(str_detect(address, "loyalist pkwy") ~ 
                               str_extract(address, "^\\d* loyalist pkwy"),
                             TRUE ~ address),# ROAD 3 ROAD, take the second rd off...
         address = str_remove(address, " n$"),
         address = str_remove(address, " highway$")) %>% 
  mutate(address = str_trim(str_to_lower(address))) 


# Save --------------------------------------------------------------------

qsave(STA, file = "output/STA.qs", nthreads = availableCores())

