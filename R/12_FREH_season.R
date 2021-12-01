#### 12 FREH SEASONAL MODEL ####################################################

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

qload("output/str_processed.qsm")

summer_start <- as.Date("2017-05-01")
summer_end <- as.Date("2017-10-01")


# Function that looks at each season, and tells the amount of FREH --------

fun_seasonal_FREH <- function(start_date, end_date) {
  
  # how many years to look at FREH seasons
  nb_years <- 5
  
  # create a list of numbers of seasons
  seasons_list <- list()
  
  lst_summer <- list()
  lst_winter <- list()
  
  lst_ft_props <- list()
  
  # how many active properties at a date, out of FREH properties in that summer
  for(i in 1:nb_years) {
    
    total_days <- 
      daily %>% 
      filter(date >= start_date + years(i-1), 
             date < end_date + years(i-1)) %>% 
      group_by(property_ID) %>% 
      summarize(max_days = as.numeric((max(date)-min(date)))) %>% 
      filter(max_days >= as.numeric(end_date - start_date)/2)
    
    props <- 
      daily %>% 
      filter(housing,
             listing_type == "Entire home/apt",
             date >= start_date + years(i-1), 
             date < end_date + years(i-1)) %>% 
      inner_join(total_days, by = "property_ID") %>% 
      group_by(property_ID) %>% 
      filter(sum(status %in% c("A", "R")) >= max_days*0.75,
             sum(status == "R") >= max_days*0.25) %>% 
      pull(property_ID) %>% 
      unique()
    
    odd_nb <- which(1:(nb_years*2) %% 2  == 1)
    
    seasons_list[[odd_nb[i]]] <- 
      daily %>% 
      filter(property_ID %in% props,
             date >= start_date + years(i-1), 
             date < end_date + years(i-1)) %>%
      count(date) %>% 
      mutate(season = "summer") %>% 
      mutate(year_season = paste0("Summer ", year(min(date))))
    
    lst_summer[[i]] <- list(props)
  }
  
  # how many active properties at a date, out of FREH properties in that winter
  for(i in 1:nb_years) {
    
    total_days <- 
      daily %>% 
      filter(date >= end_date + years(i-1), 
             date < start_date + years(i)) %>% 
      group_by(property_ID) %>% 
      summarize(max_days = as.numeric((max(date)-min(date)))) %>% 
      filter(max_days >= as.numeric(end_date - start_date)/2)
    
    props <-
      daily %>% 
      filter(housing,
             listing_type == "Entire home/apt",
             date >= end_date + years(i-1), 
             date < start_date + years(i)) %>% 
      inner_join(total_days, by = "property_ID") %>% 
      group_by(property_ID) %>% 
      filter(sum(status %in% c("A", "R")) >= max_days*0.75,
             sum(status == "R") >= 7) %>% 
      pull(property_ID) %>% 
      unique()
    
    even_nb <- which(1:(nb_years*2) %% 2  == 0)[-length(which(1:(nb_years*2) %% 2  == 0))]
    
    if(i<5) {
      
      seasons_list[[even_nb[i]]] <- 
        daily %>% 
        filter(property_ID %in% props,
               date >= end_date + years(i-1), 
               date < start_date + years(i)) %>%
        mutate(season = if_else(property_ID %in% unlist(list(
          unlist(lst_summer[[i]]), unlist(lst_summer[[i+1]]))), 
          "full_time", 
          "winter")) %>% 
        group_by(season) %>%
        count(date) %>% 
        group_by(season) %>% 
        mutate(n = mean(n)) %>% 
        mutate(year_season = paste0("Winter ", year(min(date))))
      
      # ONLY TO RETRIEVE A LIST FOR FULL-TIME PROPERTIES
      lst_ft_props[[even_nb[i]]] <- 
        daily %>% 
        filter(property_ID %in% props,
               date >= end_date + years(i-1), 
               date < start_date + years(i)) %>%
        # filter(property_ID %in% unlist(list(
        #   unlist(lst_summer[[i]]), unlist(lst_summer[[i+1]])))) %>% 
        mutate(year_season = paste0("Winter ", year(min(date)))) %>% 
        select(property_ID, year_season) %>% 
        distinct()
    }
    
    
    lst_winter[[i]] <- list(props)
  }
  
  # how many active properties at a date, out of FREH properties in that summer
  for(i in 1:(nb_years-1)) {
    
    total_days <- 
      daily %>% 
      filter(date >= start_date + years(i), 
             date < end_date + years(i)) %>% 
      group_by(property_ID) %>% 
      summarize(max_days = as.numeric((max(date)-min(date)))) %>% 
      filter(max_days >= as.numeric(end_date - start_date)/2)
    
    props <- 
      daily %>% 
      filter(housing,
             listing_type == "Entire home/apt",
             date >= start_date + years(i), 
             date < end_date + years(i)) %>% 
      inner_join(total_days, by = "property_ID") %>% 
      group_by(property_ID) %>% 
      filter(sum(status %in% c("A", "R")) >= max_days*0.75,
             sum(status == "R") >= max_days*0.25) %>% 
      pull(property_ID) %>% 
      unique()
    
    odd_nb <- which(1:(nb_years*2) %% 2  == 1)[-1]
    
    seasons_list[[odd_nb[i]]] <-
      daily %>% 
      filter(property_ID %in% props,
             date >= start_date + years(i), 
             date < end_date + years(i)) %>%
      mutate(season = if_else(property_ID %in% unlist(list(
        unlist(lst_winter[[i]]), unlist(lst_winter[[i+1]]))), 
        "full_time", 
        "summer")) %>% 
      group_by(season) %>%
      count(date) %>% 
      group_by(season) %>% 
      mutate(n = mean(n)) %>% 
      mutate(year_season = paste0("Summer ", year(min(date))))
    
    # ONLY TO RETRIEVE A LIST FOR FULL-TIME PROPERTIES
    lst_ft_props[[odd_nb[i]]] <- 
      daily %>% 
      filter(property_ID %in% props,
             date >= start_date + years(i), 
             date < end_date + years(i)) %>%
      # filter(property_ID %in% unlist(list(
      #   unlist(lst_winter[[i]]), unlist(lst_winter[[i+1]])))) %>% 
      mutate(year_season = paste0("Summer ", year(min(date)))) %>% 
      select(property_ID, year_season) %>% 
      distinct()
    
    
  }
  
  # bind the prior list of seasons
  out <- do.call("rbind", seasons_list)
  
  # Making sure that seasons are in order
  season_levels <- 
    out %>% 
    arrange(date) %>% 
    pull(year_season) %>% 
    unique()
  
  # Average per season, distinct
  out <- 
    out %>% 
    arrange(date) %>% 
    mutate(season = case_when(season == "full_time" ~ "Year-round",
                              season == "summer" ~ "Summer",
                              season == "winter" ~ "Winter"),
           year_season = factor(year_season, levels = season_levels),
           season = factor(season, levels = c("Summer", "Winter", "Year-round"))) %>%
    filter(year_season != "Summer 2017") %>% 
    select(-date) %>% 
    distinct() 
  
  
  # a list of all full time properties with their seasons attached
  full_time_properties <- do.call("rbind", lst_ft_props)
  
  
  list(out, full_time_properties)
}

fun_seasonal_FREH_output <- fun_seasonal_FREH(summer_start, summer_end)
full_time_listings <- fun_seasonal_FREH_output[[2]]
seasonal_FREH <- fun_seasonal_FREH_output[[1]]

# Plot
seasonal_plot <- 
  seasonal_FREH %>% 
  ggplot()+
  geom_bar(aes(year_season, n, fill = season), stat= "identity")+
  theme(plot.title = element_text(size=10),
        legend.title = element_blank())+
  scale_fill_manual("legend", 
                    values = c("Year-round" = col_palette[3],
                               "Summer" = col_palette[1],
                               "Winter" = col_palette[2]))+
  scale_y_continuous(name = NULL, label = scales::comma) +
  theme_minimal()+
  xlab("")+
  ylab("")+
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.grid.minor.x = element_blank())

ggsave("output/figures/housing_loss.pdf", 
       # plot = figure_5_1_fun("Futura", "Futura Condensed"), 
       plot = seasonal_plot, 
       width = 9, height = 4.2, units = "in", useDingbats = FALSE)


# Save --------------------------------------------------------------------

qsave(seasonal_FREH, file = "output/seasonal_FREH.qs")
qsave(full_time_listings, file = "output/full_time_listings.qs")
