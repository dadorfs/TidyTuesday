## -------------------------------------------------
##
## Script name: Tidy Tuesday - CRAN Collaboration
##
## Purpose of script:
##
## Author: Daniel Dorfsman
##
## Date Created: 2023-09-18
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
library(ggstream)
library(ggtext)
library(ggsci)
library(here)

# Just learned about this function during this tt
packageDescription("here")

## -------------------------------------------------
## Load data

tt <- tt_load('2023-09-19')

cran_20230905 <- tt$cran_20230905

## -------------------------------------------------
## Process data

cran_20230905$year <- year(cran_20230905$Published)

# Core tidyverse packages
core_tverse <-
  c("ggplot2",
    "dplyr",
    "tidyr",
    "readr",
    "purrr",
    "tibble",
    "stringr",
    "forcats")

# Count occurrences of each core tidy package in 'Imports'
tidy_imports <- core_tverse %>%
  map( ~ str_detect(cran_20230905$Imports, .x) %>%
         as_tibble_col(column_name = .x)) %>%
  list_cbind()

tidy_imports$package <- cran_20230905$Package
tidy_imports$year <- cran_20230905$year

tidy_imports_long <- tidy_imports %>% 
  pivot_longer(-c(year, package), names_to = "tidy_package", values_to = "imports")
  
# Count imports by year and tidy package
tidy_counts <- tidy_imports_long %>% 
  summarize(.by = c(year, tidy_package), num_imports = sum(imports == TRUE, na.rm = TRUE)) %>% 
  filter(!is.na(year))
            
## -------------------------------------------------
## Visualize 

text_df <- 
  data.frame(text = "The Tidyverse's rise to prominence is expemplified by the growing number of packages that depend on its functionalities.",
             year = 2018,
             num_imports = 5000)

sys_font <- "Futura"

theme_set(theme_gray(base_family = sys_font))

ggplot(data = tidy_counts) +
  geom_stream(
    aes(x = year, y = num_imports, fill = tidy_package),
    color = "white",
    type = "ridge",
    lwd = 0.1,
    bw = 0.8,
    extra_span = 0.2
  ) +
  geom_textbox(
    data = text_df,
    aes(x = year, y = num_imports, label = text),
    width = 0.65,
    box.color = "black",
    box.size = 0.15,
    alpha = 0.9, 
    family = sys_font
  ) +
  labs(title = "Tidy Up!",
       y = "Num. published packages importing Tidyverse") +
  ggsci::scale_fill_futurama(name = "Tidyverse Package") +
  scale_x_continuous(limits = c(2015, 2023),
                     breaks = seq(2013, 2023, by = 2)) +
  scale_y_continuous(breaks = seq(0, 6000, by = 2000)) +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20, unit = "pt"),
    plot.background = element_rect(fill = "#E0E0E0"),
    panel.background = element_rect(fill = "#E0E0E0"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(vjust = 5),
    panel.grid.major.y = element_line(
      color = "black",
      linewidth = 0.1,
      linetype = "dotted"
    ),
    legend.background = element_rect(fill = "#E0E0E0")
  )


# ggsave("tt_20230919_cran.png")
  
  
  
