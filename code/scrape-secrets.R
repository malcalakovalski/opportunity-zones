## ---------------------------
##
## Script name: 
##
## Purpose of script: 
## Download the data on Sean Parker’s 2020 campaign contributions 
## into an Excel file
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

## load up the packages we will need:  (uncomment as required)

library('tidyverse')
library('rvest')
library('stringr')
library('lubridate')
library('writexl')

## Load websites ---------------------------

open_secrets_url <- 
  "https://www.opensecrets.org/search?order=desc&page=1&q=sean+parker&sort=D&type=donors"
url_p1 <- 
  "https://www.opensecrets.org/search?order=desc&page="
url_p2 <- 
  "&q=sean+parker&sort=D&type=donors"

all_url  <-
  str_c(url_p1, 1:10, url_p2)

open_secrets  <-
  lapply(all_url, read_html)


# Get tables --------------------------------------------------------------

tables <- 
  lapply(open_secrets, html_table)

parker_donations <-
  tables %>%
  bind_rows %>%
  as_tibble() %>%
  mutate(Date = as.Date(Date, format = "%m-%d-%Y"),
         Amount = as.numeric(
           gsub("[\\$,]", "", parker_donations$Amount)
           ),
         Party = case_when(grepl("(D)", Recipient) == TRUE ~ "D",
                           grepl("(R)", Recipient) == TRUE  ~ "R"
                           )
           ) %>%
  separate(Contributor, c("Contributor", "Address"), "\n") %>%
  mutate(across(where(is.character), str_trim)) %>%
  select(Date, Category, Contributor, Address, Occupation, Recipient, Party, Amount) %>%
  map_dfr(str_to_title, c('Occupation', 'Contributor')) %>%
  mutate(Address = gsub("Ca", "CA", parker_donations$Address)) %>%
  filter(Amount > 0)

#save as xlxs
write_xlsx(parker_donations, "data/sean_parker_donations.xlsx")
saveRDS(parker_donations, "data/parker_donations.RDS")

