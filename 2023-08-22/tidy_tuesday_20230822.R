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
library(scales)
library(countrycode)
library(here)

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
  labs(title = "Top 20 Asylum Countries From 2010-2022") +
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6)) +
  coord_flip() +
  theme_minimal() +
  theme(title = element_text(size = rel(1.25)),
        plot.background = element_rect(fill = "white"),
        axis.title = element_blank())

# Top 20 refugee countries-of-origin 2010-2022
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
  labs(title = "Top 20 Refugee Countries of Origin 2010-2022") +
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6)) +
  coord_flip() +
  theme_minimal() +
  theme(title = element_text(size = rel(1.25)),
        plot.background = element_rect(fill = "white"),
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
                      name = "Total Refugees") +
  labs(title = "2010-2022 Refugees Accepted Per Country") +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    title = element_text(size = rel(1.25)),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )
  

# -------------------------------------------------
## save plots

# ggsave("top20_asylum_countries.png", top20_asylum_plot)
# ggsave("top20_coo.png", top20_coo_plot)
# ggsave("refugee_map.png", world_map_plot)
