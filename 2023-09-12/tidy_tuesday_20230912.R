## -------------------------------------------------
##
## Script name: Tidy Tuesday - Global Human Day
##
## Purpose of script:
##
## Author: Daniel Dorfsman
##
## Date Created: 2023-09-11
##
## -------------------------------------------------
##
## Notes:
##   
##
## -------------------------------------------------


## Load packages

library(tidytuesdayR)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(showtext)
library(here)

## -------------------------------------------------
## Load data

tt <- tt_load('2023-09-12')

all_countries <- tt$all_countries
country_regions <- tt$country_regions

## -------------------------------------------------
## Process Data

# Join country name to all_countries
all_countries_name <- all_countries %>% 
  left_join(country_regions[, c("country_iso3", "country_name")], by = "country_iso3")

all_countries_zz <- all_countries_name %>% 
  filter(Subcategory == "Sleep & bedrest") %>% 
  select(country_iso3,
         sleephrs = hoursPerDayCombined)

## -------------------------------------------------
## Mapping

# Load world map
world_map <- ne_countries(returnclass = "sf") %>% 
  filter(name != "Antarctica")

world_map_zz <- world_map %>% 
  left_join(all_countries_zz, by = c("adm0_iso" = "country_iso3"))

world_map_zz_eqearth <- st_transform(world_map_zz, crs = "+proj=eqearth +wktext")
countries_center <- st_point_on_surface(world_map_zz_eqearth)

least_sleep_country <- filter(countries_center, sleephrs == min(sleephrs, na.rm = TRUE))
least_sleep_coord <- c(least_sleep_country$geometry[[1]][1], least_sleep_country$geometry[[1]][2])

most_sleep_country <- filter(countries_center, sleephrs == max(sleephrs, na.rm = TRUE))
most_sleep_coord <- c(most_sleep_country$geometry[[1]][1], most_sleep_country$geometry[[1]][2])

# Plot

# Add font
font_add_google("Roboto Slab", family = "robo")
showtext.opts(dpi = 200)
showtext_auto()

bkgrd_col <- "#F9F3E9"
main <- "Well-Rested?"
sub <- "Most countries are getting at least 8 hours of sleep per day, on average.\nIraq and Japan have the highest and lowest levels of sleep, respectively."
  
ggplot(world_map_zz_eqearth, size = 0.125) +
  geom_sf(aes(fill = sleephrs)) +
  geom_curve(aes(x = 15e6, y = 2e6, xend = least_sleep_coord[1], yend = least_sleep_coord[2]),
             curvature = 0.2, arrow = grid::arrow(length = unit(2, "pt"), type = "closed")) +
  geom_curve(aes(x = 7e6, y = -1e6, xend = most_sleep_coord[1], yend = most_sleep_coord[2]),
             curvature = 0.2, arrow = grid::arrow(length = unit(2, "pt"), type = "closed")) + 
  geom_text(x = 15e6, y = 1.5e6, label = "Japan (8.01 hrs)") +
  geom_text(x = 7.3e6, y = -1.5e6, label = "Iraq (10.21 hrs)") +
  labs(title = main, subtitle = sub) + 
  scale_fill_gradient2(
    low = "#FDD33D",
    high = "#2E1680",
    mid = "#875AD7",
    breaks = c(8, 9.5, 11),
    midpoint = 9.5,
    limits = c(8, 11)
  ) +
  guides(fill = guide_colorbar(
    barwidth = unit(20, "lines"),
    barheight = unit(0.5, "lines")
  )) + 
  theme(
    plot.background = element_rect(fill = bkgrd_col),
    panel.background = element_rect(fill = bkgrd_col),
    plot.margin = margin(30, 0, 0, 0, unit = "pt"),
    plot.title = element_text(hjust = 0.5, size = 24, family = "robo", face = "bold"), 
    plot.subtitle = element_text(hjust = 0.5, size = 14, family = "robo", face = "bold"),
    panel.grid = element_line(linewidth = 0.1, color = "grey25"),
    legend.position = 'bottom',
    legend.title = element_blank(),
    legend.background = element_rect(fill = bkgrd_col), 
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
    
  )

  # ggsave("tt_20230912_humanday.png", dpi = 200)

