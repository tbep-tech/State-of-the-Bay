library(tidycensus)
library(tidyverse)
library(lubridate)
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

##
# manually add 2020, 2021 (not in tidycensus yet)
# https://www.statista.com/statistics/815278/tampa-metro-area-population/

popdat <- popdat %>% 
  bind_rows(
    tibble(
      yr = c(2020, 2021), 
      pop = c(3183385, 3219514))
  )

save(popdat, file = here('data/popdat.RData'))

# social media data -------------------------------------------------------

datraw <- get_sheet_as_csv('Comms. Metrics Sheet') %>% 
  textConnection %>% 
  read.table(sep = ',', header = T)

parents <- c('TBEP Facebook', 'TBEP IG', 'Tarpon Tag', 'Be Floridian FB', 'TBEP LinkTree', 'TBEP Unsplash', 
             'TBEP Twitter', 'Constant Contact', 'TBEP YouTube', 'GSC: TBEP.ORG', 'GSC: Be Floridian', 
             'GA: tbep.org', '#LTB Hashtags on IG', 'TBEP: Google My Business', 
             'Reddit', 'Outreach Materials Request')

comdat <- datraw %>% 
  mutate(
    parent = case_when(
      PLATFORM %in% parents ~ PLATFORM, 
      T ~ NA_character_
    )
  ) %>% 
  fill(parent) %>% 
  filter(!PLATFORM %in% parents) %>% 
  select(-contains(c('Change', 'Column'))) %>% 
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

datraw <- get_sheet_as_csv('Give-A-Day_SOB_Rollup_Test') %>% 
  textConnection %>% 
  read.table(sep = ',', header = T)

gaddat <- datraw %>% 
  select(
    year = Task.Name,
    nevent = `X..Events`, 
    nadults = `X..Adult`, 
    nyouth = `X..Youth`,
    npartner = Number.of.Partners,
    nplants = `X..Plants.Installed`, 
    nlbs = `Lbs.of.Debris.Removed`,
    nfeet = `Area.Improved..linear.Ft.`#,
    # underserved = `Underserved.Community`
  ) %>% 
  mutate(
    year = gsub('^Give-A-Day Activities_FY', '', year), 
    year = paste0('20', year)
  ) %>% 
  mutate_if(is.numeric, round, 0)

save(gaddat, file = here('data/gaddat.RData'))

# tberf funding data ------------------------------------------------------

tberfraw <- get_sheet_as_csv('TBERF_Budget_Index') %>% 
  textConnection %>% 
  read.table(sep = ',', header = T)

tberfdat <- tberfraw %>% 
  select(year = Year, title = Title, lead = Lead, total = `Project.Totals`, admin_total = `Admin.Totals`, matching = `Matching.Funds`) %>% 
  filter(lead != '') %>% 
  mutate(
    total = gsub('\\$|,', '', total), 
    total = as.numeric(total), 
    admin_total = gsub('\\$|,', '', admin_total),
    admin_total = as.numeric(admin_total), 
    matching = gsub('\\$|,', '', matching), 
    matching = as.numeric(matching), 
    lead = case_when(
      lead == 'Eckerd' ~ 'Eckerd College', 
      T ~ lead
    )
  )

save(tberfdat, file = here('data/tberfdat.RData'))

# bay mini grant funding data ---------------------------------------------

bmgraw <- get_sheet_as_csv('Bay Mini-Grant_Budget Index') %>% 
  textConnection %>% 
  read.table(sep = ',', header = T)

bmgdat <- bmgraw %>% 
  select(year = Year, title = Project.TItle, lead = Lead, total = Tarpon.Tag.Funds) %>% 
  mutate_all(function(x) ifelse(x == '', NA, x)) %>% 
  mutate(
    total = gsub('\\$|,', '', total),
    total = as.numeric(total)
  ) %>% 
  filter(year >= 2000)

save(bmgdat, file = here('data/bmgdat.RData'))

# digital challenge grant -------------------------------------------------

dcgraw <- get_sheet_as_csv('Digital_Challenge_Grants') %>% 
  textConnection %>% 
  read.table(sep = ',', header = T)

dcgdat <- dcgraw %>% 
  filter(grepl('^PO\\s', Task.Name)) %>% 
  select(lead = Task.Name, year = Start, total = Grant.Budget) %>% 
  mutate_all(function(x) ifelse(x == '', NA, x)) %>% 
  mutate(
    year = mdy(year), 
    year = year(year),
    total = gsub('\\$|,', '', total),
    total = as.numeric(total), 
    lead = gsub('^.*:\\s*', '', lead)
  )

save(dcgdat, file = here('data/dcgdat.RData'))


