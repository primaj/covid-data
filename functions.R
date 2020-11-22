# Takes a .csv url and downloads it
pull_data <- function(url){
  
  readr::read_csv(url) %>% 
    rename(
      
      province_state = `Province/State`,
      country = `Country/Region`
      
    )
}

convert_numeric <- function(x) str_remove_all(x, '[:punct:]') %>% as.numeric()

# Scrape other country data
scrape_data <- function(){
  
  read_html('https://www.worldometers.info/world-population/population-by-country/') %>%
    html_node('#example2') %>% html_table() %>% 
    select(c(country = 2, pop = 3, yoy = 4, net_change = 5, density = 6, med_age = 10, urban_pop = 11)) %>% 
    mutate_at(2:7, convert_numeric) %>% 
    mutate(iso3 = countrycode(country, origin = 'country.name', destination = 'iso3c'),
           iso2 = countrycode(country, origin = 'country.name', destination = 'iso2c'))
}

# pivot date columns, clean data, and summarise to total cases by day
tidy_data <- function(data){
  
  data %>% 
    
    # pivot date columns
    pivot_longer(
      cols = contains('/'),
      names_to = "date",
      values_to = "count"
    ) %>% 
    
    # convert date 
    mutate(
      
      date = as.Date(strptime(date, format = "%m/%d/%y")),
      count = as.numeric(count)
      
    ) %>% 
    
    # replace NAs with 0
    mutate(count = if_else(is.na(count), 0, count)) %>% 
    
    # summarise to country total by day
    group_by(country, date) %>%
    
    summarise(country_total = sum(count)) %>% 
    
    # compute differential
    mutate(next_day_total = lead(country_total, 1),
           d_total = next_day_total - country_total,
           grad_angle = (180*atan(d_total))/pi)
}

