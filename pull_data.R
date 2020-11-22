# pull_data.R -------------------------------------------------------------
#
# This script pulls & aggregates data from 3 .csv files stored here:
# https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
#
# The data contains COVID-19 confirmed cases, recoveries and deaths by country by day


# Packages used -----------------------------------------------------------

library(tidyverse)
library(countrycode)
library(rvest)

# Source Helper Functions --------------------------------------------------------
source("functions.R")

# Data Preparation --------------------------------------------------------

url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_%s_global.csv'


# map each csv file
nested_data <- tibble(cdr = c('Confirmed', 'Deaths', 'Recovered')) %>% 
  
  mutate(
    
    # download each dataset
    raw_data = purrr::map(cdr, ~pull_data(sprintf(url, tolower(.x)))),
    
    # clean & munge data
    tidy_data = purrr::map(raw_data, tidy_data)
    
  ) 


data <- nested_data %>% 
  
  select(-raw_data) %>% 
  # unnest into a single frame
  unnest(tidy_data) %>% 
  
  select(country, cdr, date, country_total, next_day_total) %>% 
  
  pivot_wider(id_cols = c(country, date), names_from = cdr, values_from = country_total) %>% 
  
  left_join({
    
    tibble(
      country = unique(.$country),
      iso3 = countrycode(country, origin = 'country.name', destination = 'iso3c')
    )
  })

# Write to the data folder ------------------------------------------------

if(!dir.exists('data')) dir.create('data')
readr::write_csv(data, 'data/cdr-covid19-cases.csv')
if(!file.exists('data/country-data.csv')) readr::write_csv(scrape_data(), 'data/country-data.csv')
