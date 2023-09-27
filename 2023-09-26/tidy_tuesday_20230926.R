## -------------------------------------------------
##
## Script name: Tidy Tuesday
##
## Purpose of script:
##
## Author: Daniel Dorfsman
##
## Date Created: 2023-09-26
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
library(ggtext)
library(ggforce)
library(here)

## -------------------------------------------------
## Load data

tt <- tt_load("2023-09-26")
richmondway <- tt$richmondway

## -------------------------------------------------
## Plot

richmondway$Season_label <- with(richmondway, paste("Season", Season))

sys_font <- "Impact"
theme_set(theme_gray(base_family = sys_font))

bomb <- "<span style='font-family:Wingdings-Regular'>&#xF04D;</span>"
bkgd_col <- "#263239"

plot <- ggplot(richmondway) +
  geom_link2(
    aes(x = Episode_order, y = F_count_RK, color = F_count_RK)
    ) +
  geom_richtext(
    aes(x = Episode_order, y = F_count_RK, color = F_count_RK, label = bomb),
    size = 12,
    fill = NA,
    label.color = NA,
    hjust = 0.34,
    vjust = 0.5
  ) +
  labs(title = "Roy Kent: Man of F-Words...",
       y = "Number of F-Bombs") + 
  scale_color_gradient(low = "gold", high = "red2") +
  scale_x_continuous(breaks = 1:34) + 
  facet_grid(~Season_label, scales = "free", switch = "x") + 
  theme(
    plot.background = element_rect(fill = bkgd_col),
    plot.margin = margin(30, 30, 30, 30, unit = "pt"),
    plot.title = (element_text(color = "white", size = 24, vjust = 5)),
    panel.background = element_rect(fill = bkgd_col),
    panel.grid = element_blank(),
    panel.spacing = unit(2, "lines"),
    axis.text = element_text(color = "white"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "white", vjust = 10),
    strip.text = element_text(color = "white"),
    strip.background = element_rect(
      fill = bkgd_col,
      color = "white"
    ),
    legend.position = "none"
  )
  
# I had trouble saving the plot using ggsave and maintaining the 
# bomb font. I saved the plot directly from the image viewer in RStudio
# as a workaround


