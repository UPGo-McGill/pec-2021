#### 02 GEOMETRY IMPORT ########################################################

#' This script should only be rerun when geometry needs to be rebuilt from
#' scratch.
#'
#' Output:
#' - `geometry.qsm`
#'
#' Script dependencies:
#' - None
#'
#' External dependencies:
#' 

source("R/01_startup.R")
library(cancensus)
library(osmdata)

# ELECTORAL WARDS (EW) ------------------------------------------------------

EW <- 
  read_sf("data/McGill/Wards.shp") %>% 
  st_transform(32617) %>% 
  select(NAME, MUNITYP) %>% 
  set_names(c("ward", "type", "geometry"))
  

# Zoning --------------------------------------------------------------------

ZN <- 
  read_sf("data/McGill/Zoning.shp") %>% 
  st_transform(32617) %>% 
  select(ZONING, ZONE, WARD, BYLAW, FileNumber) %>% 
  set_names(c("zoning", "zone", "ward", "bylaw", "file_number", "geometry")) %>% 
  mutate(ward = case_when(ward == 1 ~ "Picton",
                          ward == 2 ~ "Bloomfield",
                          ward == 3 ~ "Wellington",
                          ward == 4 ~ "Ameliasburgh",
                          ward == 5 ~ "Athol",
                          ward == 6 ~ "Hallowell",
                          ward == 7 ~ "Hillier",
                          ward == 8 ~ "North Marysburgh",
                          ward == 9 ~ "South Marysburgh",
                          ward == 10 ~ "Sophiasburgh"))


# ON province -------------------------------------------------------------

province <-
  get_census("CA16", regions = list(PR = "35"), geo_format = "sf") %>%
  st_transform(32617) %>%
  select(geometry)

# PEC ---------------------------------------------------------------------

city <-
  get_census(
    dataset = "CA16", regions = list(CSD = "3513020"), level = "CSD",
    geo_format = "sf") %>%
  st_transform(32617) %>%
  select(GeoUID, Dwellings) %>%
  set_names(c("GeoUID", "dwellings", "geometry")) %>%
  st_set_agr("constant")


# PEC DAs -------------------------------------------------------------

DA <-
  get_census(
    dataset = "CA16", regions = list(CSD = "3513020"), level = "DA",
    geo_format = "sf") %>%
  st_transform(32617) %>%
  select(GeoUID, Dwellings) %>%
  set_names(c("GeoUID", "dwellings", "geometry")) %>%
  st_set_agr("constant")

# Streets -----------------------------------------------------------------

streets <-
 (getbb("County of Prince Edward") * c(1.01, 0.99, 0.99, 1.01)) %>%
 opq(timeout = 200) %>%
 add_osm_feature(key = "highway") %>%
 osmdata_sf()

streets <-
 rbind(
   streets$osm_polygons %>% st_set_agr("constant") %>% st_cast("LINESTRING"),
   streets$osm_lines) %>%
 as_tibble() %>%
 st_as_sf() %>%
 st_transform(32617) %>%
 st_set_agr("constant") %>% 
 st_intersection(city)

streets <-
 streets %>%
 filter(highway %in% c("primary", "secondary", "residential", "tertiary")) %>%
 select(osm_id, name, highway, geometry)


# Save output -------------------------------------------------------------

qsavem(province, city, DA, EW, ZN, streets,
       file = "output/geometry.qsm", nthreads = availableCores())
