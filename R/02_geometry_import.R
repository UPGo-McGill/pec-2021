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

## ELECTORAL WARDS (EW)
#' https://www.thecounty.ca/wp-content/uploads/2020/08/Current-Ten-Ward-Map.pdf
#' There are boundaries called Electoral Wards in Prince Edward County.
#' There is no .shp available (that I found of). We can possibly take the PDF
#' and create polygons with a raster manipulation since colors are very distinct.
#' We are using the same projection and coordinate system (UTM 18N), but I can't
#' quite superpose our polygon with theirs. TBD
#' 
#' We can call the dataframe "EW", and each line an "electoral_ward"


# ON province -------------------------------------------------------------

province <-
  get_census("CA16", regions = list(PR = "35"), geo_format = "sf") %>%
  st_transform(32618) %>%
  select(geometry)

# PEC -------------------------------------------------------------

city <-
  get_census(
    dataset = "CA16", regions = list(CSD = "3513020"), level = "CSD",
    geo_format = "sf") %>%
  st_transform(32618) %>%
  select(GeoUID, Dwellings) %>%
  set_names(c("GeoUID", "dwellings", "geometry")) %>%
  st_set_agr("constant")


# PEC DAs -------------------------------------------------------------

DA <-
  get_census(
    dataset = "CA16", regions = list(CSD = "3513020"), level = "DA",
    geo_format = "sf") %>%
  st_transform(32618) %>%
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
 st_transform(32618) %>%
 st_set_agr("constant") %>% 
 st_intersection(city)

streets <-
 streets %>%
 filter(highway %in% c("primary", "secondary", "residential", "tertiary")) %>%
 select(osm_id, name, highway, geometry)


# Save output -------------------------------------------------------------

qsavem(province, city, DA, streets,
       file = "output/geometry.qsm", nthreads = availableCores())
