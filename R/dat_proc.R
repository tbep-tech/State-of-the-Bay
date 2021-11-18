library(tidycensus)
library(tidyverse)
library(here)

mykey <- Sys.getenv("census_key")

census_api_key(mykey)

##
# american community survey data (started in 2005)

# variables for acs (acs1, 3, or 5 are one, three, and five year results) from load_variables('2005', 'acs1')
# pop variable is B01003_001

yrs <- seq(2005, 2019)
out <- NULL
for(yr in yrs){
  
  tmp <- get_acs('metropolitan statistical area/micropolitan statistical area', variables = 'B01003_001', survey = 'acs1', year = yr)

  tmp <- tmp %>% 
    mutate(yr = !!yr)
  
  out <- bind_rows(out, tmp)
  
}

acsdat <- out %>% 
  filter(grepl('^Tampa', NAME)) %>% 
  select(yr, pop = estimate)

##
# ten year census data
dec2000 <- get_decennial('county', variables = 'P001001', year = 2000, state = 'FL', county = c('Pinellas', 'Hillsborough', 'Manatee')) %>% 
  mutate(
    yr = 2000
  )
# no api endpoint for 1990, https://www.census.gov/data/tables/time-series/demo/popest/estimates-and-change-1990-2000.html
dec1990 <- tibble(
    GEOID = c('12057', '12081', '12103'),
    NAME = c('Hillsborough County, Florida', 'Manatee County, Florida', 'Pinellas County, Florida'),
    variable = rep('P001001', 3), 
    value = c(834054, 211707, 851659)
  ) %>% 
  mutate(
    yr = 1990
  )

decdat <- bind_rows(dec1990, dec2000) %>% 
  group_by(yr) %>% 
  summarise(
    pop = sum(value)
  )

## 
# combine both
popdat <- bind_rows(decdat, acsdat) %>% 
  arrange(yr)

save(popdat, file = here('data/popdat.RData'))



