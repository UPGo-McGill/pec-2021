#### 21 CHAPTER 2 FIGURES ######################################################

#' This script produces the graphs and maps for chapter 2. It runs quickly.
#' 
#' Output:
#' - `figure_2_1.pdf`
#' - `figure_2_2.pdf`
#' - `figure_2_3.pdf`
#' - `figure_2_4.pdf`
#' - `figure_2_5.pdf`
#' - `figure_2_6.pdf`
#' - `figure_2_7.pdf`
#' - `figure_2_8.pdf`
#' 
#' Script dependencies:
#' - `09_str_processing.R`
#' - `13_condo_analysis.R`
#' 
#' External dependencies:
#' - The Futura and Futura Condensed fonts, which can be imported in 
#'   `01_startup.R`

source("R/01_startup.R")
library(feasts)
library(fabletools)

load("output/str_processed.Rdata")
load("output/geometry.Rdata")


# Figure 2.1.1 Active daily listings ----------------------------------------

active_listings <- 
  daily %>% 
  filter(housing, status != "B") %>% 
  count(date, listing_type) %>% 
  group_by(listing_type) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6, .complete = TRUE)) %>% 
  ungroup()

active_listings <- 
  daily %>% 
  filter(housing, status != "B") %>% 
  count(date) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6, .complete = TRUE),
         listing_type = "All listings") %>% 
  bind_rows(active_listings) %>% 
  arrange(date, listing_type)

figure_2_1_1 <-
  active_listings %>% 
  filter(date >= "2017-06-07") %>%
  ggplot(aes(date, n, colour = listing_type, size = listing_type)) +
  geom_line() +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_date(name = NULL, limits = c(as.Date("2017-06-07"), NA)) +
  scale_colour_manual(name = NULL, values = col_palette[c(5, 1:3)],
                      guide = guide_legend(
                        override.aes = list(size = c(1.5, 0.75, 0.75, 0.75)))) +
  scale_size_manual(values = c("All listings" = 1.5, "Entire home/apt" = 0.75,
                               "Private room" = 0.75, "Shared room" = 0.75),
                    guide = "none") +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura"))

ggsave("output/figures/figure_2_1_1.pdf", plot = figure_2_1_1, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_2_1_1.pdf")




# Figure 2.1.2 All displayed daily listings -----------------------------------

active_listings <- 
  daily %>% 
  filter(housing) %>% 
  count(date, listing_type) %>% 
  group_by(listing_type) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6, .complete = TRUE)) %>% 
  ungroup()

active_listings <- 
  daily %>% 
  filter(housing) %>% 
  count(date) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6, .complete = TRUE),
         listing_type = "All listings") %>% 
  bind_rows(active_listings) %>% 
  arrange(date, listing_type)

figure_2_1_2 <-
  active_listings %>% 
  ggplot(aes(date, n, colour = listing_type, size = listing_type)) +
  geom_line() +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_date(name = NULL, limits = c(as.Date("2017-06-01"), NA)) +
  scale_colour_manual(name = NULL, values = col_palette[c(5, 1:3)],
                      guide = guide_legend(
                        override.aes = list(size = c(1.5, 0.75, 0.75, 0.75)))) +
  scale_size_manual(values = c("All listings" = 1.5, "Entire home/apt" = 0.75,
                               "Private room" = 0.75, "Shared room" = 0.75),
                    guide = "none") +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura"))

ggsave("output/figures/figure_2_1_2.pdf", plot = figure_2_1_2, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_2_1_2.pdf")


# Figure 2.2 YOY listing and revenue growth rates -------------------------

daily_variation <- 
  daily %>% 
  filter(housing, status != "B", date >= "2015-12-16", date != "2020-02-29") %>% 
  group_by(date) %>% 
  summarize("Active listings" = n(), Revenue = sum(price[status == "R"])) %>% 
  mutate(across(where(is.numeric), 
                function(x) slide_dbl(x, ~{(.x[366] - .x[1]) / .x[1]}, 
                                      .before = 365, .complete = FALSE))) %>% 
  pivot_longer(-date, names_to = "var", values_to = "value") %>% 
  group_by(var) %>% 
  mutate(value = slide_dbl(value, mean, .before = 13, .complete = TRUE)) %>% 
  ungroup() %>% 
  filter(date >= "2017-05-01")

figure_2_2 <- 
  daily_variation %>% 
  filter(date >= "2018-06-01") %>%
  ggplot(aes(date, value, colour = var)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(lwd = 1) +
  scale_x_date(name = NULL, limits = c(as.Date("2018-06-01"), NA)) +
  scale_y_continuous(name = NULL, limits = c(-1, NA), 
                     labels = scales::percent) +
  scale_color_manual(name = NULL, values = col_palette[c(5, 1)]) +
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura"))

ggsave("output/figures/figure_2_2.pdf", plot = figure_2_2, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_2_2.pdf")


# Figure 2.3 Active listings as a share of dwellings ----------------------

active_area <-
  daily %>%
  filter(housing, status != "B", date >= LTM_start_date, 
         date <= LTM_end_date) %>%
  count(area, date) %>% 
  group_by(area) %>% 
  summarize(n = mean(n, na.rm = TRUE)) %>%
  left_join(LA, .) %>% 
  mutate(percentage = n / dwellings, n = round(n, digit = -1)) %>% 
  select(area, n, dwellings, percentage)

active_DA <-
  daily %>%
  filter(housing, status != "B", date >= "2021-01-01", date <= "2021-12-31") %>%
  left_join(select(st_drop_geometry(property), property_ID, GeoUID)) %>% 
  count(GeoUID, date) %>% 
  group_by(GeoUID) %>% 
  summarize(n = mean(n, na.rm = TRUE)) %>%
  left_join(DA, .) %>% 
  mutate(percentage = n / dwellings, n = round(n, digit = -1),
         percentage = if_else(dwellings <= 4, NA_real_, percentage)) %>% 
  relocate(geometry, .after = last_col())

make_listing_map <- function(df) {
  ggplot(df) +
    geom_sf(data = province, colour = "transparent", fill = "grey93") +
    geom_sf(aes(fill = percentage),
            colour = if (nrow(df) == 19) "white" else "transparent") +
    scale_fill_stepsn(colors = col_palette[c(5, 3, 1)], na.value = "grey80",
                         limits = c(0, 0.1), oob = scales::squish, 
                         labels = scales::percent)  +
    guides(fill = guide_colourbar(title = "STRs/\ndwelling",
                                  title.vjust = 1)) + 
    gg_bbox(df) +
    theme_void() +
    theme(text = element_text(family = "Futura", face = "plain"),
          legend.title = element_text(family = "Futura", face = "bold",
                                      size = 7),
          legend.title.align = 0.9,
          legend.text = element_text(family = "Futura", size = 5),
          panel.border = element_rect(colour = "white", size = 2))
}

# figure_2_3_left <- make_listing_map(active_area)

figure_2_3_right <- 
  make_listing_map(active_DA) #+
  # geom_rect(xmin = 607000, ymin = 5038000, xmax = 614000, ymax = 5045000,
  #           fill = NA, colour = "black", size = 0.3)

# fig_2_3_zoom <- 
#   figure_2_3_right +
#   geom_sf(data = streets_downtown, size = 0.3, colour = "white") +
#   coord_sf(xlim = c(607000, 614000), ylim = c(5038000, 5045000),
#            expand = FALSE) +
#   theme(legend.position = "none",
#         panel.border = element_rect(fill = NA, colour = "black", size = 0.6))

# layout <- c(
#   area(1, 1, 42, 40),
#   area(1, 41, 42, 80),
#   area(3, 41, 22, 60)
# )

figure_2_3 <- 
  # figure_2_3_left + 
  figure_2_3_right #+ #fig_2_3_zoom + 
  # plot_layout(design = layout) + plot_layout(guides = 'collect') & 
  # theme(legend.position = "bottom", legend.key.width = unit(1.8, "lines"))

ggsave("output/figures/figure_2_3.pdf", plot = figure_2_3, width = 8, 
       height = 4.2, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_2_3.pdf")


# Figure 2.6 Host revenue distribution ------------------------------------

revenue_colour <- colorRampPalette(col_palette[c(1, 3, 6, 4, 2, 5)])(10)

host_deciles <-
  daily %>%
  filter(housing, date >= "2021-01-01", date <= "2021-12-31", 
         status == "R", !is.na(host_ID)) %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price)) %>% 
  summarize(all = sum(rev),
            top_10 = sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_20 = sum(rev[rev > quantile(rev, c(0.80))] / all) - 
              sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_30 = sum(rev[rev > quantile(rev, c(0.70))] / all) - 
              sum(rev[rev > quantile(rev, c(0.80))] / all),
            top_40 = sum(rev[rev > quantile(rev, c(0.60))] / all) - 
              sum(rev[rev > quantile(rev, c(0.70))] / all),
            top_50 = sum(rev[rev > quantile(rev, c(0.50))] / all) - 
              sum(rev[rev > quantile(rev, c(0.60))] / all),
            top_60 = sum(rev[rev > quantile(rev, c(0.40))] / all) - 
              sum(rev[rev > quantile(rev, c(0.50))] / all),
            top_70 = sum(rev[rev > quantile(rev, c(0.30))] / all) - 
              sum(rev[rev > quantile(rev, c(0.40))] / all),
            top_80 = sum(rev[rev > quantile(rev, c(0.20))] / all) - 
              sum(rev[rev > quantile(rev, c(0.30))] / all),
            top_90 = sum(rev[rev > quantile(rev, c(0.10))] / all) - 
              sum(rev[rev > quantile(rev, c(0.20))] / all),
            top_100 = sum(rev[rev > quantile(rev, c(0.00))] / all) - 
              sum(rev[rev > quantile(rev, c(0.10))] / all)) %>% 
  select(-all) %>% 
  pivot_longer(everything(), names_to = "percentile", values_to = "value") %>% 
  mutate(percentile = factor(percentile, levels = paste0("top_", 1:10 * 10))) %>% 
  mutate(perfect_distribution = 0.1,
         decile = 1:10,
         dummy_1 = perfect_distribution,
         dummy_2 = value) %>%  
  rename("0" = perfect_distribution, "1" = value, "0.25" = dummy_1, 
         "0.75" = dummy_2) %>%
  pivot_longer(c("0","0.25", "0.75", "1"), names_to = "position") %>% 
  mutate(position = as.numeric(position),
         display_val = scales::percent(value, .1)) %>% 
  group_by(position) %>% 
  mutate(absolute_val = slide_dbl(value, ~{.x[1] / 2 + sum(.x[-1])}, 
                                  .after = 9)) %>% 
  ungroup() %>% 
  mutate(
    display_val = paste0("earned ", display_val, "\nof revenue"),
    display_percentile = case_when(
      percentile == "top_10" ~ "Top 10% of hosts...",
      percentile == "top_20" ~ "Next 10% of hosts...",
      TRUE ~ NA_character_))

figure_2_6 <- 
  host_deciles %>% 
  ggplot(aes(position, value, group = decile, fill = decile)) +
  geom_area(colour = "white", lwd = 1.2) +
  geom_text(aes(x = 0.02, y = absolute_val, label = display_percentile), 
            data = filter(host_deciles, position == 0, decile <= 2),
            family = "Futura", 
            hjust = 0) +
  geom_text(aes(x = 0.98, y = absolute_val, label = display_val), 
            data = filter(host_deciles, position == 1, decile <= 2),
            family = "Futura", 
            hjust = 1) +
  scale_y_continuous(name = "Host decile", label = scales::label_percent(1),
                     breaks = seq(0, 1, by = 0.1), limits = c(0, 1),
                     sec.axis = sec_axis(~., 
                                         name = "% of total revenue",
                                         labels = derive(), 
                                         breaks = derive())) +
  scale_fill_gradientn(colours = revenue_colour) +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "Futura"),
        axis.text.y = element_text(hjust = 1),
        axis.title.y.left = element_text(
          angle = 90, margin = margin(0, 10, 0, 0)),
        axis.title.y.right = element_text(
          angle = 270, margin = margin(0, 0, 0, 10)),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank())

ggsave("output/figures/figure_2_6.pdf", plot = figure_2_6, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_2_6.pdf")


# Figure 2.7 Multilistings ------------------------------------------------

ML <- 
  daily %>% 
  filter(status != "B") %>% 
  group_by(date) %>% 
  summarize(Listings = mean(multi),
            Revenue = sum(price[status == "R" & multi], na.rm = TRUE) / 
              sum(price[status == "R"], na.rm = TRUE)) %>% 
  mutate(across(where(is.numeric), slide_dbl, mean, .before = 13)) %>% 
  pivot_longer(c(Listings, Revenue), names_to = "Multilisting percentage",
               values_to = "value")

figure_2_7 <- 
  ML %>% 
  filter(date >= "2017-06-01") %>% 
  ggplot() +
  geom_line(aes(date, value, colour = `Multilisting percentage`), lwd = 1) +
  scale_y_continuous(name = NULL, 
                     label = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(values = col_palette[c(5, 1)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold"),
        legend.text = element_text(family = "Futura"))

ggsave("output/figures/figure_2_7.pdf", plot = figure_2_7, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_2_7.pdf")


# Figure 2.8 Commercialization of STR listings ----------------------------

commercial_listings <- 
  daily %>% 
  filter(status != "B", date >= "2016-01-01") %>% 
  mutate(commercial = if_else(FREH_3 < 0.5 & !multi, FALSE, TRUE)) %>% 
  count(date, commercial) %>% 
  group_by(commercial) %>% 
  mutate(n = slide_dbl(n, mean, .before = 6)) %>% 
  ungroup()

figure_2_8 <- 
  commercial_listings %>% 
  filter(date >= "2017-06-01") %>%
  ggplot() +
  geom_line(aes(date, n, color = commercial), lwd = 1) +
  scale_x_date(name = NULL, limits = c(as.Date("2017-06-01"), NA)) +
  scale_y_continuous(name = NULL) +
  scale_colour_manual(name = "Listing type",
                      values = col_palette[c(5, 1)],
                      labels = c("Non-commercial", "Commercial")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold"),
        legend.text = element_text(family = "Futura"))

ggsave("output/figures/figure_2_8.pdf", plot = figure_2_8, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figures/figure_2_8.pdf")


# Clean up ----------------------------------------------------------------

rm(active_area, active_condos_area, active_DA, active_listings,
   LA, LA_raw, city, commercial_listings, condo_scatter, DA,
   DA_probabilities_2017, DA_probabilities_2019, daily_variation, fig_2_3_zoom,
   fig_2_4_zoom, figure_2_1, figure_2_2, figure_2_3, figure_2_3_left,
   figure_2_3_right, figure_2_4, figure_2_4_left, figure_2_4_right,
   figure_2_5, figure_2_6, figure_2_7, figure_2_8, host_deciles, layout,
   listing_probabilities_2017, listing_probabilities_2019, ML, province,
   streets, streets_downtown, revenue_colour, make_condo_map, make_listing_map)
