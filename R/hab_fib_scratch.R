library(tbeptools)
library(tidyverse)
library(readxl)
library(httr)
library(jsonlite)
library(sf)

# pyro ----------------------------------------------------------------------------------------

# https://f50006a.eos-intl.net/F50006A/OPAC/Details/Record.aspx?BibCode=5635517
datall <- read.csv('https://f50006a.eos-intl.net/ELIBSQL12_F50006A_Documents/OTBMP_Pyrodinium_Chl_2011-2020_v101922.csv') %>%
  select(
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
    date = `Sample Date`,
    Latitude,
    Longitude,
    pyro = `Pyrodinium bahamense abundance (cells/L)`
  ) %>%
  mutate(date = ymd(date))

brks <- c(-Inf, 1e4, 1e5, 1e6, Inf)
labs <- c('No bloom', 'Low', 'Medium', 'High')

dat <- bind_rows(datall, dat2021, dat2022, dat2023) %>%
  mutate(
    yr = year(date),
    doy = yday(date),
    pyro = ifelse(pyro == 0, NA, pyro),
    pyrocat = cut(pyro, breaks = brks, labels = labs),
    pyro = pmin(3e6, pyro)
  )

toplo <- dat %>% 
  filter(yr > 2011)

toplomed <- toplo %>% 
  reframe(
    medv = median(pyro, na.rm = T), 
    .by = yr
  ) %>% 
  mutate(
    pyrocat = cut(medv, breaks = brks, labels = labs), 
  )
    
p1 <- ggplot(toplo, aes(x = yr, y = pyro, group = yr)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.75, color = 'grey') +
  scale_y_log10(breaks = c(1e3, 1e4, 1e5, 1e6), labels = parse(text = c('10^3', 'Low~(10^4)', 'Medium~(10^5)', 'High~(10^6)'))) +
  scale_x_continuous(breaks = seq(min(toplo$yr), max(toplo$yr), 1)) +
  geom_segment(data = toplomed,
               aes(x = yr - 0.25, xend = yr + 0.25,
                   y = medv, yend = medv),
               linewidth = 1
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  labs(
    y = 'Bloom intensity (cells / L)', 
    x = NULL,
    title = expression(paste(italic('Pyrodinium bahamense'), ' bloom intensity in Old Tampa Bay')), 
    subtitle = 'Observed cell counts and annual medians', 
    caption = 'Data source: Florida Fish and Wildlife Conservation Commission'
  )


# karenia -------------------------------------------------------------------------------------

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


# habdat p1
toplo <- kbrdat %>%
  st_set_geometry(NULL) %>%
  filter(var == 'kb') %>% 
  filter(date < as.Date('2024-01-01') & date > as.Date('1960-01-01')) %>% 
  # filter(month(date) > 3 & month(date) < 10) %>% 
  mutate(
    yr = year(date)
  )

# plot
p2 <- ggplot(toplo, aes(x = yr, y = 1 + val)) +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.9) +
  scale_y_log10(breaks = c(1e3, 1e4, 1e5, 1e6), labels = parse(text = c('10^3', 'Low~(10^4)', 'Medium~(10^5)', 'High~(10^6)')), limits= c(1000, NA)) +
  # scale_x_discrete(breaks = seq(1950, 2025, by = 5)) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = NULL
    y = 'Cells / L (log-scale)'
  )

