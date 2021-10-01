# Draft from full time status assessment

monthly %>% 
  filter(year >= 2018) %>% 
  filter(FREH == TRUE) %>% 
  mutate(date = paste0(year, "-", month, "-01", sep = "")) %>% 
  mutate(date = lubridate::as_date(date)) %>% 
  count(date) %>% 
  ggplot(aes(date, n)) +
  geom_col(lwd = 0, fill = col_palette[2])+
  scale_x_date(name = NULL, limits = c(as.Date("2018-01-01"), NA)) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  theme_minimal()

winter_months <- c(1, 2, 3, 4)
seasonal_months <- c(5, 6, 7, 8, 9, 10, 11, 12)

monthly %>% 
  mutate(full_time_month = ifelse(R+A >= 16, TRUE, FALSE)) %>% 
  group_by(property_ID, year, month <= 4) %>% 
  summarize(n_ft = sum(full_time_month)) %>% 
  mutate(p_ft = ifelse(`month <= 4` == TRUE, n_ft/4, n_ft/8)) %>% 
  mutate(ft = ifelse(p_ft >= 0.5, TRUE, FALSE)) %>% 
  group_by(property_ID, year) %>% 
  summarize(ft_off_season = `month <= 4` == TRUE & ft == TRUE,
            ft_season = `month <= 4` == FALSE & ft == TRUE) %>% 
  mutate(ft_year = ifelse(ft_off_season == TRUE & ft_season == TRUE, TRUE, FALSE)) %>% 
  View()

# Monthly share of revenues and reservations

count_r_year <- 
  daily %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(status == "R",
         date >= "2017-01-01", date < "2020-12-31") %>% 
  group_by(year) %>% 
  summarize(n=n(), total_revenue = sum(price))

p_r <- 
  daily %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(status == "R",
         date >= "2017-01-01", date < "2020-12-31") %>% 
  group_by(year, month) %>% 
  summarize(n_months = n(), revenue = sum(price)) %>% 
  left_join(., count_r_year, by = "year") %>% 
  mutate(p_r = n_months/n, p_p = revenue/total_revenue)

monthly_shares_figure <- 
  p_r %>%
  select(year, month, p_r, p_p) %>% 
  pivot_longer(cols = c(p_r, p_p)) %>% 
  mutate(date = paste0(year, "-", month, "-01", sep="")) %>% 
  mutate(date = as_date(date),
         month_year = zoo::as.yearmon(date,"%Y-%m-%d")) %>% 
  ggplot() +
  geom_line(aes(x=date, y=value, color=name))+
  scale_y_continuous(name = NULL, label = scales::percent_format(accuracy = 1),
                     breaks = c(0, 0.05, 0.1, 0.15, 0.2))+
  scale_x_date(name=NULL)+
  scale_color_manual(name = NULL, 
                     labels = c("Share of revenue", "Share of reservations"), 
                     values = col_palette[c(3, 6)]) +
  theme_minimal()+
  theme(legend.position = "bottom")
         
         
  
  
  