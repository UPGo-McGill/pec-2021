#### 01 STARTUP ################################################################

# There is usually no need to run this script directly; it is sourced from the
# other scripts which need it.


# Optionally install packages from GitHub ---------------------------------

# remotes::install_github("UPGo-McGill/upgo")
# remotes::install_github("UPGo-McGill/strr")
# remotes::install_github("UPGo-McGill/matchr")


# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(upgo)
library(strr)
library(sf)
library(future)
library(progressr)
library(slider)
library(data.table)
library(gt)
library(extrafont)
library(patchwork)
library(qs)


# Set global variables ----------------------------------------------------

if (Sys.info()["sysname"] != "Windows") plan(multisession)
start_2020 <- as.Date("2019-06-01")
end_2020 <- as.Date("2020-05-31")
start_2021 <- as.Date("2020-06-01")
end_2021 <- as.Date("2021-05-31")
col_palette <-
  c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Optionally install and activate fonts -----------------------------------

# suppressWarnings(font_import(paths = "data/fonts", prompt = FALSE))
#
# read_csv(system.file("fontmap", "fonttable.csv", package = "extrafontdb")) %>%
#   mutate(FamilyName = if_else(str_detect(FontName, "Condensed") == TRUE,
#                               "Futura Condensed", FamilyName)) %>%
#   write_csv(system.file("fontmap", "fonttable.csv", package = "extrafontdb"))
#
# extrafont::loadfonts()
