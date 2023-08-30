## -------------------------------------------------
##
## Script name: Tidy Tuesday - Fair Use
##
## Purpose of script:
##
## Author: Daniel Dorfsman
##
## Date Created: 2023-08-29
##
## -------------------------------------------------
##
## Notes:
##   
##
## -------------------------------------------------


## Load packages

## -------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(here)

## -------------------------------------------------
## Load data

tt_dat <- tt_load("2023-08-29")

fair_use_cases <- tt_dat$fair_use_cases
fair_use_findings <- tt_dat$fair_use_findings

unique(unlist(strsplit(fair_use_cases$categories, "[;,]\\s*")))

## -------------------------------------------------
## Process data

# Each case is labeled with one or more categories
# find unique categories
head(fair_use_cases)

fair_use_cases$categories <- tolower(fair_use_cases$categories)

# Extract unique categories 
fair_use_categories<- unlist(
  strsplit(fair_use_cases$categories, "[,;]\\s*")
)

# Some categories differ by spelling/wording
sort(table(fair_use_categories_1))

# Clean up categories
fair_use_cases$categories_fixed <- 
  str_replace_all(
    fair_use_cases$categories, 
    c("edu.*/\\w*/\\w*" = "education/scholarship/research",
      "parody(/satire)*" = "parody/satire",
      "films*/audiovisual" = "film/audiovisual",
      "internet(/digitization)*" = "internet/digitization",
      "photography*" = "photograph",
      "news reporting photograph" = "news reporting; photograph")
  )
  
fair_use_categories_fixed <- unlist(
  strsplit(fair_use_cases$categories_fixed, "[,;]\\s*")
)

# Better
sort(table(fair_use_categories_fixed))

# Separate rows by category
# drop categories with < 10 observations
fair_use_cases_long <- fair_use_cases %>% 
  separate_longer_delim(categories_fixed, regex("[,;]\\s")) %>% 
  filter(.by = categories_fixed, n() > 10 & year >= 1950)

## -------------------------------------------------
## Explore

fair_use_cases_long %>% 
  mutate(categories_fixed = fct_infreq(categories_fixed)) %>% 
  ggplot(aes(x = categories_fixed)) + 
  geom_bar(aes(fill = fair_use_found)) + 
  coord_flip() +
  theme_minimal()

ggplot(fair_use_cases_long) + 
  geom_freqpoly(aes(x = year, color = categories_fixed)) + 
  facet_wrap(~ categories_fixed) + 
  scale_color_discrete() +
  labs(title = "Fair Use Cases by Category 1950-2022") +
  scale_x_continuous(breaks = seq(1950, 2022, by = 10)) + 
  theme(
    plot.background = element_rect(fill = "black"),
    plot.margin = margin(2, 2, 1, 1, unit = "lines"),
    plot.title = element_text(color = "white", face = "bold", size = 16, vjust = 5),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black"),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white", face = "bold"),
    axis.text = element_text(color = "white"), 
    axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1),
    legend.position = "none"
  )  
  
# ggsave("tt_20230829_fair_use.png", width = 9, height = 6)


