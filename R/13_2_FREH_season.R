#### 13 FREH MODEL #############################################################

#' This script is moderately time-consuming to run; it should be rerun whenever
#' STR data changes.
#'
#' Output:
#' - `FREH_seasonal` (updated)
#' - `FREH_model.qsm`
#'
#' Script dependencies:
#' - `12_str_processing.R`
#'
#' External dependencies:
#' - None

source("R/01_startup.R")

qload("output/str_processed.R")

winter_start <- as.Date("2017-10-01") - years(1)
winter_end <- as.Date("2018-05-01") - years(1)


# Function that looks at each season, and tells the amount of FREH --------

fun_seasonal_FREH <- function(start_date, end_date, threshold = 0.90) {
  
  # how many years to look at FREH seasons
  nb_years <- year(max(daily$date)) - 2017
  
  # create a list of numbers of seasons
  seasons_list <- list(1:nb_years*2)
  
  # how many active properties at a date, out of FREH properties in that winter
  for(i in 1:nb_years) {
    
    max_days <- 
      daily %>% 
      filter(date >= start_date + years(i), date < end_date + years(i)) %>% 
      summarize((max(date)-min(date))) %>% pull() %>% as.numeric()
    
    props <-
      daily %>% 
      filter(housing,
             listing_type == "Entire home/apt",
             date >= start_date + years(i), date < end_date + years(i)) %>% 
      group_by(property_ID) %>% 
      filter(sum(status %in% c("A", "R")) >= max_days*threshold) %>% 
      pull(property_ID)
    
    odd_nb <- which(1:(nb_years*2) %% 2  == 1)
    
    seasons_list[[odd_nb[i]]] <- 
      daily %>% 
      filter(property_ID %in% props,
             date >= start_date + years(i), date < end_date + years(i),
             status != "B") %>%
      count(date) %>% 
      mutate(season = "winter")
    
  }
  
  # how many active properties at a date, out of FREH properties in that summer
  for(i in 1:nb_years) {
    
    max_days <- 
      daily %>% 
      filter(date < start_date + years(i+1), date >= end_date + years(i)) %>% 
      summarize((max(date)-min(date))) %>% pull() %>% as.numeric()
    
    props <- 
      daily %>% 
      filter(housing,
             listing_type == "Entire home/apt",
             date < start_date + years(i+1), date >= end_date + years(i)) %>% 
      group_by(property_ID) %>% 
      filter(sum(status %in% c("A", "R")) >= max_days*threshold) %>% 
      pull(property_ID)
    
    even_nb <- which(1:(nb_years*2) %% 2  == 0)
    
    seasons_list[[even_nb[i]]] <- 
      daily %>% 
      filter(property_ID %in% props,
             date < start_date + years(i+1), date >= end_date + years(i),
             status != "B") %>%
      count(date) %>% 
      mutate(season = "summer")
    
    
  }
  
  # bind the prior list
  do.call("rbind", seasons_list)
  
}

seasonal_FREH <- fun_seasonal_FREH(winter_start, winter_end, threshold = 0.9)

# Save --------------------------------------------------------------------

qsave(seasonal_FREH, file = "output/seasonal_FREH.qs")
