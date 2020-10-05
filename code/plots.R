## ---------------------------
##
## Script name: Donations Plots
##
## Purpose of script:
## Visualize Parkers contributions
## Author: Manuel Alcalá Kovalski
##
## Date Created: 2020-10-05
##
## Email: malcalakovalski@brookings.edu
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

# I prefer to view outputs in non-scientific notation
options(scipen = 6, digits = 4) 

## ---------------------------

## load up the packages we will need:

library('writexl')
library('tidyverse')
library('stringr')
library('lubridate')
library('ggplot2')
library('ggthemes')


# Functions ---------------------------------------------------------------

manu_theme <- function() {
  theme(
    # modify grid 1)
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.spacing = unit(0,"cm"), 
    # modify text, axis and colour 2) 
    axis.text = element_text(family = "Times New Roman"),
    axis.title = element_text(family = "Times New Roman"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.line = element_line(colour = "black"),
    # legend at the bottom 3)
    legend.position = "bottom",
    # background
    panel.background = element_blank(),
    panel.border = element_blank(),
    # title and caption
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
    # Color  discrete
  )
}

parker_donations  <-
  readRDS("data/parker_donations.RDS") %>%
  mutate(Amount = as.numeric(Amount))


# Plots -------------------------------------------------------------------
parker_donations %>%
  select(Date, Party, Amount) %>%
  drop_na() %>%
  ggplot(aes(x = Party,
              y = Amount)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~ year(Date)) + 
  scale_y_continuous(name = '', labels = scales::dollar) +
  theme_hc()

parker_donations %>% 
  melt(id = "Party")

parker_donations %>%
  select(Date, Party, Amount) %>%
  pivot_longer(cols = - c(Date, Party), names_to =  "Type", values_to = "Value") %>%
  select(-Type) %>%
  melt(id = c("Party", "Date")) %>%
  drop_na() %>%
  ggplot(aes(x = Party,
             y = value,
            fill =  Party)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c('dodgerblue', 'firebrick1'))  +
  facet_wrap(~ year(Date)) + 
  scale_y_continuous(name = '', labels = scales::dollar) +
  theme_hc() 
  

  
  
