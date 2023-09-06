## -------------------------------------------------
##
## Script name: Tidy Tuesday - 
##
## Purpose of script:
##
## Author: Daniel Dorfsman
##
## Date Created: 2023-09-05
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
library(gghighlight)
library(showtext)
library(here)

## -------------------------------------------------
## Load data

wages <- read_csv("wages.csv")

## -------------------------------------------------
## Process data

# Drop duplicates and pivot longer
wages_long <- distinct(wages) %>%
  pivot_longer(
    cols = c(union_wage, nonunion_wage, wage),
    names_to = "category",
    values_to = "mean_wage"
  ) %>%
  mutate(category = str_replace_all(category, c("_wage" = "", "wage" = "overall"))) %>%
  mutate(
    subfacet = case_when(
      str_detect(facet, ":") ~ str_replace(facet, "^(.*):.*", "\\1"),
      TRUE ~ NA_character_
    ),
    subfacet_group = case_when(
      str_detect(facet, ":") ~ str_replace(facet, "^.*:(.*)$", "\\1"),
      TRUE ~ NA_character_
    )
  )

## -------------------------------------------------
## Explore

# Add fonts 
font_add_google("Roboto Slab", family = "robo")
showtext_opts(dpi = 200)
showtext_auto()

# Mean wage of union workers consistently ahead of nonunion 
filter(wages_long, facet == "all wage and salary workers") %>% 
  ggplot(aes(x = year, y = mean_wage, color = category)) + 
  geom_line()

# Plot salary premium of union vs non-union over time by demographic group
title_main <- "It Pays to Unionize"
title_caption <- 
  "Union members consistenly earn higher wages than their non-union counterparts.\nHowever, for certain demgraphics the extent of this benefit is in decline."

filter(wages_long, subfacet == "demographics") %>%
  
  ## factor demographic groups in order of increasing pay premium vs time regression coefficient
  mutate(.by = subfacet_group, lm_coeff = coef(lm(union_wage_premium_adjusted ~ year))[2]) %>%
  mutate(subfacet_group = fct_reorder(subfacet_group, lm_coeff)) %>%
  
  ## plot
  ggplot(aes(x = year, y = union_wage_premium_adjusted, color = subfacet_group)) +
  geom_line() +
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(linewidth = 0.5)) +
  facet_wrap(~ subfacet_group, nrow = 3) +
  labs(x = "Year", y = "Salary Compared to Non-Union", 
       title = title_main, subtitle = title_caption) +
  scale_y_continuous(labels = scales::label_percent(scale = 100, prefix = "+")) +
  ggsci::scale_color_cosmic() +
  theme(
    plot.title = element_text(family = "robo", size = 20, face = "bold", vjust = 4),
    plot.subtitle = element_text(family = "robo", size = 10, vjust = 7),
    plot.background = element_rect(fill = "#F3F2EC"),
    plot.margin = margin(2, 2, 1, 1, unit = "lines"),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(
      color = "black",
      linewidth = 0.1,
      linetype = "dotted"
    ),
    panel.background = element_rect(fill = "#F3F2EC"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", family = "robo"),
    strip.background = element_blank(),
    axis.title = element_text(family = "robo"),
    axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1),
    legend.position = "none",
  ) +
  geom_smooth(
    method = "lm",
    alpha = 0.5,
    color = "black",
    se = TRUE,
    linewidth = 0.3
  ) 

ggsave("tt_20230905_unions.png", height = 9, width = 12, dpi = 200)
  
