# setup ---------------------------------------------------------------------------------------

library(tidycensus)
library(tidyverse)
library(lubridate)
library(here)
library(rsmartsheet)
library(readxl)
library(httr)
library(jsonlite)
library(sf)
library(tbeptools)

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

# raw smartsheet data
datraw <- get_sheet_as_csv('Reach_Index_KPI') %>% 
  textConnection %>% 
  read.table(sep = ',', header = T)

parentskeep <- c('TBEP Facebook', 'TBEP IG', 'Tarpon Tag', 'Constant Contact', 'TBEP YouTube', 'GSC: tbep.org', 'GA: tbep.org')

parents <- c('TBEP Facebook', 'TBEP IG', 'Tarpon Tag', 'Be Floridian FB', 'TBEP LinkTree', 'TBEP Unsplash', 
             'TBEP Twitter', 'Constant Contact', 'TBEP YouTube', 'GSC: tbep.org', 'GSC: Be Floridian', 
             'GA: tbep.org', '#LTB Hashtags on IG', 'TBEP: Google My Business', 
             'Reddit', 'Outreach Materials Request')

comdat <- datraw %>% 
  rename(metric = METRIC) %>% 
  mutate(
    parent = case_when(
      metric %in% parents ~ metric, 
      T ~ NA_character_
    )
  ) %>% 
  fill(parent) %>% 
  filter(!metric %in% parents) %>% 
  pivot_longer(names_to = 'date', values_to = 'val', cols = -matches('metric|parent')) %>% 
  separate(date, into = c('month', 'year'), sep = '\\.') %>% 
  rename(
    platform = parent
  ) %>% 
  mutate(
    year = str_pad(year, width = 4, pad = '0'),
    year = substr(year, 3, 4), 
    year = paste0('20', year), 
    year = as.numeric(year),
    month = substr(month, 1, 3),
    month = tolower(month),
    month = factor(month, levels = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')),
    uni = case_when(
      grepl('%', val) ~ 'percent', 
      T ~ 'count'
    ), 
    val = gsub('\\,|\\%|N/A', '', val),
    val = as.numeric(val), 
    metric = case_when(
      metric %in% c('Total Fans/Followers', 'Total Followers', 'Total Subscribers') ~ 'Followers',
      metric == 'Net New Contacts' ~ 'New Contacts',
      T ~ metric
    )
  ) %>% 
  select(platform, metric, everything()) %>% 
  filter(!is.na(val)) %>% 
  filter(
    metric %in% c('New Contacts', 'Click Rate', 'Open Rate', 'Impressions/Reach', 'Followers', 'Unique Page Views', 'Total Clicks', 'Statewide Registrations', 'Total Views') &
      platform %in% parentskeep
  ) %>% 
  arrange(platform, metric, year, month) 

save(comdat, file = here('data/comdat.RData'))

# give a day from smartsheets ---------------------------------------------

datraw <- get_sheet_as_csv('Give-A-Day_SOB_Rollup_Test') %>% 
  textConnection %>% 
  read.table(sep = ',', header = T)

gaddat <- datraw %>% 
  select(
    year = Task.Name,
    nevent = `X..Events`, 
    nadult = `X..Adult`, 
    nyouth = `X..Youth`,
    npartner = Number.of.Partners,
    nplant = `X..Plants.Installed`, 
    nlbinv  = `Lbs.of.Invasives.Removed`,
    nlb = `Lbs.of.Debris.Removed`,
    nfeet = `Area.Improved..linear.Ft.`#,
    # underserved = `Underserved.Community`
  ) %>% 
  filter(!year == '') %>% 
  mutate(
    year = gsub('^Give-A-Day Activities_FY', '', year), 
    year = paste0('20', year)
  ) %>% 
  mutate_if(is.numeric, round, 0)

# manually correct nevent and npartner for 2024 to match https://tbep.org/get-involved/volunteer/
gaddat <- gaddat %>% 
  mutate(
    nevent = ifelse(year == 2024, 7, nevent), 
    npartner = ifelse(year == 2024, 13, npartner)
  )

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

bmgraw <- get_sheet_as_csv("BMG Project Rollup - All Years") %>% 
  textConnection %>% 
  read.table(sep = ',', header = T)

bmgdat <- bmgraw %>% 
  select(year = Year, title = Project.Name, lead = Lead.Entity, total = BMG.Budget) %>% 
  mutate_all(function(x) ifelse(x == '', NA, x)) %>% 
  mutate(
    total = gsub('\\$|,', '', total),
    total = as.numeric(total)#,
    # year = ifelse(year > 2022, year - 1, year) # post 2022, JL moved year up 1 to reflect year work will be done, not when funded (previous year)
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

# pyro data -----------------------------------------------------------------------------------

# https://f50006a.eos-intl.net/F50006A/OPAC/Details/Record.aspx?BibCode=5635517
datall <- read.csv('https://f50006a.eos-intl.net/ELIBSQL12_F50006A_Documents/OTBMP_Pyrodinium_Chl_2011-2020_v101922.csv') %>%
  select(
    station = Station_ID,
    yr = Year,
    date = Sample_Date,
    Latitude,
    Longitude,
    pyro = P..bahamense.Abundance..cells.L.
  ) %>%
  mutate(date = mdy(date))

# 2021 only
dat2021 <- read.csv(url('https://raw.githubusercontent.com/tbep-tech/tbep-os-presentations/master/data/Pyrodinium_Chl_2021_OTBMP_mbeck.csv')) %>%
  select(
    station = Station_ID,
    date = Sample_Date,
    Latitude,
    Longitude,
    pyro = Pbahamense..cells.L.
  ) %>%
  mutate(
    date = case_when(
      grepl('[a-z]', date) ~ dmy(date),
      T ~ mdy(date)
    )
  )

# 2022 only
dat2022 <- read.csv(url('https://raw.githubusercontent.com/tbep-tech/tbep-os-presentations/master/data/Pyrodinium_Chla_OTBMP_2022.csv')) %>%
  select(
    station = Station.ID,
    date = Date,
    Latitude,
    Longitude,
    pyro = Pyrodinium..Cells.L.
  ) %>%
  mutate(date = mdy(date))

# 2023 only
tmpfile <- tempfile(fileext = '.xlsx')
download.file('https://github.com/tbep-tech/tbep-os-presentations/raw/master/data/2023%20OTB%20Pyrodinium%20bahamense%20abundance%20data.xlsx', 
              tmpfile, 
              mode = 'wb')
dat2023raw <- read_excel(tmpfile)
unlink(tmpfile)
dat2023 <- dat2023raw %>% 
  select(
    station = `Station ID`,
    date = `Sample Date`,
    Latitude,
    Longitude,
    pyro = `Pyrodinium bahamense abundance (cells/L)`
  ) %>%
  mutate(date = ymd(date))

brks <- c(-Inf, 1e4, 1e5, 1e6, Inf)
labs <- c('No bloom', 'Low', 'Medium', 'High')

pyrdat <- bind_rows(datall, dat2021, dat2022, dat2023) %>%
  mutate(
    yr = year(date),
    doy = yday(date),
    pyro = ifelse(pyro == 0, NA, pyro),
    pyrocat = cut(pyro, breaks = brks, labels = labs)
  )

save(pyrdat, file = here::here('data/pyrdat.Rdata'))

# k brevis data -------------------------------------------------------------------------------

# query api 
path <- 'https://gis.ncdc.noaa.gov/arcgis/rest/services/ms/HABSOS_CellCounts/MapServer/0/query?'

request <- GET(
  url = path,
  query= list(       
    # where = "STATE_ID='FL'",
    where = "LATITUDE < 28.2 AND LATITUDE > 27 AND LONGITUDE > -83.4 AND LONGITUDE < -82.08",
    outFields = 'DESCRIPTION,SAMPLE_DATE,LATITUDE,LONGITUDE,SALINITY,SALINITY_UNIT,WATER_TEMP,WATER_TEMP_UNIT,GENUS,SPECIES,CATEGORY,CELLCOUNT,CELLCOUNT_UNIT',
    f = 'pjson'
  )
)

response <- content(request, as = "text", encoding = "UTF-8")
results <- fromJSON(response, flatten = T)

# format data
kbrdat <- results$features %>% 
  rename_all(function(x) gsub('^attributes\\.', '', x)) %>% 
  rename_all(tolower) %>% 
  mutate(
    date = format(sample_date, scientific = F),
    date = as.numeric(gsub('000$', '', date)), 
    date = as.POSIXct(date, origin = c('1970-01-01'), tz = 'UTC'), 
    date = as.Date(date)
  ) %>% 
  select(
    date, station = description, sal_ppt = salinity, temp_c = water_temp, kb_100kcelll = cellcount, longitude, latitude
  ) %>% 
  gather('var', 'val', -date, -station, -longitude, -latitude) %>% 
  separate(var, c('var', 'uni'), sep = '_') %>% 
  filter(!is.na(val)) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>% 
  .[tbshed, ] %>%
  .[tbseg, ]

save(kbrdat, file = here::here('data/kbrdat.RData'))

# debris derby data ---------------------------------------------------------------------------

datraw <- get_sheet_as_csv('TBDD_Metrics_SOB_Rollup') %>% 
  textConnection %>% 
  read.table(sep = ',', header = T)

dddat <- datraw %>% 
  select(
    year = Task.Name,
    nteam = `X..Teams`, 
    nvolunteer = `X..Volunteers`, 
    nlb = `Lbs.of.Debris.Removed`, 
    nsponsor = `X..Sponsors`,
    comments = Comments
  ) %>% 
  mutate(
    year = as.numeric(gsub('(^\\d+).*', '\\1', year))
  ) %>% 
  filter(!is.na(year))

save(dddat, file = here('data/dddat.RData'))

# experiential education data -----------------------------------------------------------------

datraw <- get_sheet_as_csv('Experiential_Education_Metrics_SOB_Rollup') %>% 
  textConnection %>% 
  read.table(sep = ',', header = T)

expeddat <- datraw %>% 
  select(
    year = Task.Name,
    nevent = `X..Events`, 
    nadult = `X.Adults`, 
    nyouth = `X..Youth`,
    nejevent = `X..EJ.Specific.Event.`, 
    ncorporatepartner = `X..Unique.Corporate.Partners`
  ) %>% 
  mutate(
    year = gsub('^FY(\\d+).*', '\\1', year), 
    year = as.numeric(paste0('20', year)), 
    nparticipant = nadult + nyouth
  ) 

save(expeddat, file = here('data/expeddat.RData'))

# all seven segments chlorophyll and secchi data for sob period -------------------------------

# ra shapefile for bcbs spatial subset for pinellas data, includes areas W7 and parts of W6
bcbsseg <- st_read(here('data-raw/tampabay_ra_seg_watersheds.shp')) %>%
  st_transform(crs = 4326) %>%
  filter(BAY_SEGMEN == 5)

# chlorophyll data from 2024 TBNMC compliance assessment report
load(file = url('https://github.com/tbep-tech/tbnmc-compliance-assessment-2024/raw/refs/heads/main/data/chldat.RData'))

# secchi data 2019-2024 for BCB (labelled as BCBS, subset later), MR, TCB
# BCB 2224 removed for combo with complete data below (incomplete here)
secdat <- tibble(
    fl = list.files(pattern = '1924', here('data-raw/'), full.names = T)
  ) %>% 
  group_nest(fl) %>% 
  mutate(
    data = purrr::map(fl, read.table, sep = '\t', header = T),
    data = purrr::map(data, function(x) x %>% mutate(StationName = as.character(StationName)))
  ) %>% 
  unnest(data) %>% 
  filter(Characteristic == 'Secchi disk depth') %>% 
  mutate(
    SampleDate = mdy_hms(SampleDate),
    mo = month(SampleDate), 
    yr = year(SampleDate), 
    sd_m = Result_Value * 0.3048, # verified all secchi in ft
    bay_segment = case_when(
      WaterBodyName == 'Boca Ciega Bay' ~ 'BCBS',
      WaterBodyName == 'Terra Ceia Bay' ~ 'TCB',
      WaterBodyName == 'Manatee River Estuary' ~ 'MR'
    ), 
    Actual_StationID = gsub('^\\=', '', Actual_StationID),
    Actual_StationID = gsub('\\-\\d{2}\\-\\d{2}$', '', Actual_StationID)
  ) %>% 
  filter(!(bay_segment %in% 'BCBS' & yr >= 2022)) %>% # remove BCB data 2022 and later to add data from AM
  select(bay_segment, station = Actual_StationID, yr, mo, lat = Actual_Latitude, lon = Actual_Longitude, sd_m = Result_Value) %>% 
  pivot_longer(names_to = 'var', values_to = 'val', sd_m)

# BCB data 2224, in meters, via email 4/15/2025 from AM
bcbssecdat2224 <- readxl::read_excel(here('data-raw/BCB_Secchi_2022_2024.xlsx')) %>% 
  mutate(
    bay_segment = 'BCBS', 
    yr = year(Date), 
    mo = month(Date)
  ) %>% 
  select(bay_segment, station = Site, yr, mo, lat = Latitude, lon = Longitude, sd_m = Secchi) %>% 
  pivot_longer(names_to = 'var', values_to = 'val', sd_m)

# cobmine bcbssecdat with secdat
secdat <- bind_rows(secdat, bcbssecdat2224) %>% 
  arrange(bay_segment, station, yr, mo)

# subset BCB data to BCBS
secdatbcbs <- secdat %>% 
  filter(bay_segment == 'BCBS') %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
  .[bcbsseg, ] %>% 
  st_set_geometry(NULL)

# recombine BCBS data with secdat
secdat <- secdat %>% 
  filter(bay_segment != 'BCBS') %>% 
  bind_rows(secdatbcbs) %>% 
  select(-lat, -lon) %>% 
  arrange(bay_segment, station, yr, mo)

# get chlorophyll data for mr, tcb, bcbs
chldat <- chldat %>% 
  filter(bay_segment %in% c('MR', 'TCB', 'BCBS')) %>% 
  filter(yr >= 2019) %>% 
  select(-SampleTime, -Latitude, -Longitude, -chla_q) %>% 
  pivot_longer(names_to = 'var', values_to = 'val', chla)

# chlorophyll, secchi for otb, hb, mtb, ltb
epcdat <- epcdata %>% 
  select(bay_segment, station = epchc_station, yr,  mo, chla, sd_m) %>% 
  filter(yr >= 2019) %>% 
  mutate(station = as.character(station)) %>% 
  pivot_longer(names_to = 'var', values_to = 'val', chla:sd_m)

sobdat <- bind_rows(secdat, chldat, epcdat)

save(sobdat, file = here('data/sobdat.RData'))

