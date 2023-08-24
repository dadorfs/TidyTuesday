## -------------------------------------------------
##
## Script name: Tidy Tuesday - Refugees
##
## Purpose of script: Explore UNHCR refugee data 
## from 2010-2022
##
## Author: Daniel Dorfsman
##
## Date Created: 2023-08-23
##
## -------------------------------------------------
##
## Notes:
##   
##
# -------------------------------------------------

## Load packages

library(tidytuesdayR)
library(tidyverse)
library(here)
library(scales)
library(countrycode)
library(patchwork)

# -------------------------------------------------
## Load data

tt_dat <- tt_load("2023-08-22")
population <- tt_dat$population

# -------------------------------------------------
## Explore data

# Top 20 asylum countries 2010-2022
top20_asylum_overall <-
  summarize(
    population,
    .by = c(coa_name, coa_iso),
    ref_2010_to_2022 = sum(refugees, na.rm = TRUE)
  ) %>%
  slice_max(ref_2010_to_2022, n = 20)

top20_asylum_plot <- top20_asylum_overall %>%
  mutate(coa_name = fct_reorder(coa_name, ref_2010_to_2022)) %>%
  ggplot() +
  geom_col(
    aes(x = coa_name, y = ref_2010_to_2022),
    fill = "royalblue1",
    color = "black",
    alpha = 0.8
  ) +
  labs(title = "Top 20 Asylum Countries") +
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6)) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = rel(0.9), hjust = 0.5),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.title = element_blank())

# Top 20 refugee countires of origin 2010-2022
top20_coo_overall <- 
  summarize(
    population,
    .by = c(coo_name, coo_iso),
    ref_2010_to_2022 = sum(refugees, na.rm = TRUE)
  ) %>% 
  slice_max(ref_2010_to_2022, n = 20)
  
top20_coo_plot <- top20_coo_overall %>%
  mutate(coo_name = fct_reorder(coo_name, ref_2010_to_2022)) %>%
  ggplot() +
  geom_col(
    aes(x = coo_name, ref_2010_to_2022),
    fill = "royalblue1",
    color = "black",
    alpha = 0.8
  ) +
  labs(title = "Top 20 Countries of Origin") +
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6)) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = rel(0.9), hjust = 0.5),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.title = element_blank())
  
  
# -------------------------------------------------
## Map data

# Load world map
world_map <- map_data(map = "world")

# Join ISO code to world map
converted <- countrycode::countrycode(world_map$region, origin = "country.name", destination = "iso3c")
world_map$ISO <- converted

# Total refugees within each country between 2010-202
total_refugees_by_country <- 
  summarize(
    population,
    .by = c(coa_name, coa_iso),
    total_refugees = sum(refugees, na.rm = TRUE)
  )

# Join refugees counts to map data
world_map <- world_map %>% 
  left_join(total_refugees_by_country, by = c("ISO" = "coa_iso")) %>% 
  filter(region != "Antarctica")

# Plot
world_map_plot <- ggplot() +
  geom_polygon(data = world_map,
               aes(x = long, y = lat, group = group, fill = total_refugees),
               color = "black",
               linewidth = 0.2) +
    scale_fill_gradient(low = "royalblue", high = "gold",
                      labels = label_number(suffix = "M", scale = 1e-6),
                      name = "Harbored Refugees") + 
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    title = element_text(size = rel(0.9)),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )
  
# Combine plots
plots_all <- (top20_asylum_plot | top20_coo_plot) / world_map_plot +
  plot_annotation(
    title = "Refugee Asylum and Countries of Origin (2010-2022)",
    theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = rel(1.5)))
  )

# -------------------------------------------------
## save plots

# ggsave("tt_20230822_refugees.png", plots_all, width = 13, height = 10)
