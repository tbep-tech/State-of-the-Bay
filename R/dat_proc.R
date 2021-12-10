library(tidycensus)
library(tidyverse)
library(here)
library(rsmartsheet)

cenkey <- Sys.getenv("census_key")
smrkey <- Sys.getenv("smartsheets_key")

census_api_key(cenkey)
set_smartsheet_api_key(smrkey)

# population data ---------------------------------------------------------

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
# from ES project, see https://github.com/tbep-tech/State-of-the-Bay/issues/19

legdat <- read.csv(here('data-raw/uscb_pop_estimates_tb_metro.csv')) %>% 
  select(
    yr = year, 
    pop = USCB_POP_EST_TB_Metro
  ) %>% 
  mutate(
    pop = gsub('\\,', '', pop),
    pop = as.numeric(pop)
  )

## 
# combine both
popdat <- bind_rows(legdat, acsdat) %>% 
  group_by(yr) %>% 
  summarise(pop = mean(pop), .groups = 'drop') %>% 
  arrange(yr)

save(popdat, file = here('data/popdat.RData'))

# social media data -------------------------------------------------------

datraw <- get_sheet_as_csv('Comms. Metrics Sheet') %>% 
  textConnection %>% 
  read.table(sep = ',', header = T)

parents <- c('TBEP Facebook', 'TBEP IG', 'Tarpon Tag', 'Be Floridian FB', 'TBEP LinkTree', 'TBEP Unsplash', 
             'TBEP Twitter', 'Constant Contact', 'TBEP YouTube', 'GSC: TBEP.ORG', 'GSC: Be Floridian', 
             'GA: tbep.org', 'TBEP: Google My Business', 'Reddit', 'Outreach Materials Request')

comdat <- datraw %>% 
  mutate(
    parent = case_when(
      PLATFORM %in% parents ~ PLATFORM, 
      T ~ NA_character_
    )
  ) %>% 
  fill(parent) %>% 
  filter(!PLATFORM %in% parents) %>% 
  select(-contains('Change')) %>% 
  pivot_longer(names_to = 'date', values_to = 'val', cols = -matches('PLATFORM|parent')) %>% 
  separate(date, into = c('month', 'year'), sep = '\\.') %>% 
  mutate(
    year = str_pad(year, width = 4, pad = '0'),
    year = substr(year, 3, 4), 
    year = paste0('20', year), 
    year = as.numeric(year),
    month = substr(month, 1, 3),
    month = tolower(month),
    uni = case_when(
      grepl('%', val) ~ 'percent', 
      T ~ 'count'
    ), 
    val = gsub('\\,|\\%|N/A', '', val),
    val = as.numeric(val)
  ) %>% 
  select(platform = parent, metric = PLATFORM, everything()) %>% 
  filter(!is.na(val))

save(comdat, file = here('data/comdat.RData'))

# give a day from smartsheets ---------------------------------------------

datraw <- get_sheet_as_csv('Give-A-Day_Project Management') %>% 
  textConnection %>% 
  read.table(sep = ',', header = T)

gaddat <- datraw %>% 
  select(
    status = Status, 
    event = Task.Name, 
    nevent = `X..Events`, 
    nadults = `X..Adults`, 
    nyouth = `X..Youth`,
    lat = Latitude, 
    lng = Longitude,
    npartner = Number.of.Partners,
    nplants = `X..Plants.Installed`, 
    nlbs = `Lbs.of.Debris.Removed`,
    descrip = `Project.Description..Comments`
  ) %>% 
  mutate(
    year = case_when(
      grepl('^Give-A-Day Activities', event) ~ event, 
      T ~ NA_character_
    )
  ) %>% 
  fill(year) %>% 
  filter(!year == event) %>% 
  mutate(
    year = gsub('^.*FY', '', year)
  ) %>% 
  filter(status == 'Complete') %>% 
  select(-status) %>% 
  select(year, everything())

save(gaddat, file = here('data/gaddat.RData'))

# tberf funding data ------------------------------------------------------

tberfraw <- get_sheet_as_csv('TBERF_Budget_Index') %>% 
  textConnection %>% 
  read.table(sep = ',', header = T)

tberfdat <- tberfraw %>% 
  select(year = Year, title = Title, lead = Lead, total = `Project.Total`, matching = `Matching.Funds`) %>% 
  filter(lead != '') %>% 
  mutate(
    total = gsub('\\$|,', '', total), 
    total = as.numeric(total), 
    matching = gsub('\\$|,', '', matching), 
    matching = as.numeric(matching), 
    lead = case_when(
      lead == 'Eckerd' ~ 'Eckerd College', 
      T ~ lead
    )
  )

save(tberfdat, file = here('data/tberfdat.RData'))
