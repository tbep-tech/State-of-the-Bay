# setup ---------------------------------------------------------------------------------------

library(tbeptools)
library(tidyverse)
library(extrafont)
library(patchwork)
library(sf)
library(readxl)
library(ggfx)
library(grid)
library(here)
library(maptiles)
library(tidyterra)
library(networkD3)
library(htmltools)
library(units)
library(here)

windowsFonts(Lato = windowsFont("Lato"))
source(here('R/funcs.R'))

fml <- "Lato"

# wq matrix map -----------------------------------------------------------

maxyr <- 2023
txtcol <- 'black'
p <- show_sitemap(epcdata, yrsel = maxyr) +
  theme(
    plot.background = element_rect(fill = NA, color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

jpeg('figures/wqmap.jpg', family = fml, height = 7, width = 6, units = 'in', res = 300)
print(p)
dev.off()

# tbni score --------------------------------------------------------------

tbniscr <- anlz_tbniscr(fimdata)
p1 <- show_tbniscrall(tbniscr) +
  theme(
    text = element_text(family = fml),
    axis.text.x = element_blank()
  ) +
  ggtitle('Tampa Bay Nekton Index')
p2 <- show_tbniscr(tbniscr) +
  theme(
    text = element_text(family = fml)
  )

p <- p1 + p2 + plot_layout(ncol = 1, heights = c(1, 1))

jpeg('figures/tbnits.jpg', family = fml, height = 7, width = 6, units = 'in', res = 300)
print(p)
dev.off()

jpeg('figures/tbnitsbyseg.jpg', family = fml, height = 4, width = 6, units = 'in', res = 300)
print(p2)
dev.off()

# tbni report card --------------------------------------------------------

tbniscr <- anlz_tbniscr(fimdata)

# w/ text
p <- show_tbnimatrix(tbniscr, family = fml) +
  theme(
    text = element_text(family = fml),
    axis.text.y = element_text(family = fml)
  )
jpeg('figures/tbnireport.jpg', family = fml, height = 6, width = 3, units = 'in', res = 300)
print(p)
dev.off()

# w/o text
p <- show_tbnimatrix(tbniscr, family = fml, txtsz = NULL) +
  theme(
    text = element_text(family = fml),
    axis.text.y = element_text(family = fml)
  )
jpeg('figures/tbnireportnotxt.jpg', family = fml, height = 6, width = 3, units = 'in', res = 300)
print(p)
dev.off()

# tbni report card detailed -------------------------------------------------------------------

tbniscr <- anlz_tbniscr(fimdata)

p1 <- show_tbnimatrix(tbniscr, txtsz = NULL, position = 'bottom', rev = T, family = fml) +
  scale_y_continuous(expand = c(0,0), breaks = sort(unique(tbniscr$Year))) + 
  coord_flip() +
  theme(
    axis.text.x = element_blank()
  )
p2 <- show_tbniscr(tbniscr, family = fml)

p <- p1 + p2 + plot_layout(ncol = 1, heights = c(0.25, 1))

jpeg('figures/tbnidetreport.jpg', family = fml, height = 6, width = 7, units = 'in', res = 300)
print(p)
dev.off()

# tbbi report card --------------------------------------------------------

maxyr <- 2023
tbbiscr <- anlz_tbbiscr(benthicdata)
p <- show_tbbimatrix(tbbiscr, family = fml, yrrng = c(1993, maxyr)) +
  theme(
    text = element_text(family = fml),
    axis.text.y = element_text(family = fml)
  ) +
  ggtitle('Tampa Bay Benthic Index Report Card')

jpeg('figures/tbbireport.jpg', family = fml, height = 7, width = 5, units = 'in', res = 300)
print(p)
dev.off()

# tidal creek report ------------------------------------------------------

tidcrk <- tidalcreeks[tbshed, ]
tidres <- anlz_tdlcrk(tidcrk, iwrraw, yr = 2023)

out <- show_tdlcrkmatrix(tidres)

jpeg('figures/tidalcreekreport.jpg', family = fml, height = 7, width = 6, units = 'in', res = 300)
print(out)
dev.off()

# tidal creek map ---------------------------------------------------------

cols <- c('lightblue', 'green', 'yellow', 'orange', 'coral')
names(cols) <- c('No Data', 'Monitor', 'Caution', 'Investigate', 'Prioritize')

# join data to tidalcreeks sf
tomap <- tidalcreeks %>%
  dplyr::inner_join(tidres, by = c('id', 'wbid', 'JEI', 'class', 'name')) %>%
  dplyr::mutate(
    score = factor(score, levels = c('No Data', 'Monitor', 'Caution', 'Investigate', 'Prioritize'))
  ) %>%
  .[tbshed, ]

# base tiles
bbx <- tomap %>% 
  sf::st_bbox() %>% 
  sf::st_as_sfc() %>% 
  sf::st_buffer(dist = units::set_units(5, kilometer)) %>%
  sf::st_transform(crs = 4326) %>% 
  sf::st_bbox()

tls <- maptiles::get_tiles(bbx, provider = "CartoDB.PositronNoLabels", zoom = 10)
dat_ext <- sf::st_as_sfc(bbx) %>% 
  sf::st_transform(crs = 4326) %>% 
  sf::st_bbox()

p <- ggplot() + 
  geom_spatraster_rgb(data = tls, maxcell = 1e8) +
  geom_sf(data = tomap, aes(colour = score, fill = score), inherit.aes = F, linewidth = 1, pch = 21) +
  scale_fill_manual(values = cols, drop = F, guide = guide_legend(reverse = T)) +
  scale_colour_manual(values = cols, drop = F, guide = guide_legend(reverse = T)) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 7),
    legend.position = 'inside',
    legend.position.inside = c(0.15, 0.15),
    legend.background = element_blank(),
    legend.title = element_blank()
  ) +
  ggspatial::annotation_scale(location = 'tr', unit_category = 'metric') +
  coord_sf(xlim = dat_ext[c(1, 3)], ylim = dat_ext[c(2, 4)], expand = FALSE, crs = 4326)

jpeg('figures/tidalcreekmap.jpg', family = fml, height = 8, width = 4, units = 'in', res = 300)
print(p)
dev.off()

# seagrass coverage -------------------------------------------------------

##
# save plot

png('figures/seagrasscov.png', height = 3.25, width = 6, res = 300, unit = 'in')
sgcov_plo(seagrass, family = fml)
dev.off()

# habs ----------------------------------------------------------------------------------------

load(file = here::here('data/pyrdat.RData'))
load(file = here::here('data/kbrdat.RData'))

# pyro
toplo1 <- pyrdat %>% 
  filter(yr > 2011)

toplomed1 <- toplo1 %>% 
  reframe(
    medv = median(pyro, na.rm = T), 
    .by = yr
  )

p1 <- ggplot(toplo1, aes(x = yr, y = pyro, group = yr)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.75, color = 'grey') +
  scale_y_log10(breaks = c(1e3, 1e4, 1e5, 1e6), labels = parse(text = c('10^3', 'Low~(10^4)', 'Medium~(10^5)', 'High~(10^6)')),
                limits = c(1e3, NA)) +
  scale_x_continuous(breaks = seq(min(toplo1$yr), max(toplo1$yr), 1)) +
  geom_segment(data = toplomed1,
               aes(x = yr - 0.25, xend = yr + 0.25,
                   y = medv, yend = medv),
               linewidth = 1
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1)
  ) +
  labs(
    y = 'Bloom intensity (cells / L)', 
    x = NULL,
    title = expression(paste(italic('Pyrodinium bahamense'), ' bloom intensity in Old Tampa Bay')), 
    subtitle = 'Observed cell counts > 0 and annual medians', 
    caption = 'Source: Florida Fish and Wildlife Conservation Commission'
  )

# karenia
toplo2 <- kbrdat %>%
  st_set_geometry(NULL) %>%
  filter(var == 'kb') %>% 
  filter(date < as.Date('2024-01-01') & date > as.Date('1960-01-01')) %>% 
  # filter(month(date) > 3 & month(date) < 10) %>% 
  mutate(
    yr = year(date), 
    val = ifelse(val == 0, NA, val)
  ) %>% 
  filter(yr >= 1990)

toplomed2 <- toplo2 %>% 
  reframe(
    medv = median(val, na.rm = T), 
    .by = yr
  )

# plot
p2 <- ggplot(toplo2, aes(x = yr, y = val)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.75, color = 'grey') +
  scale_y_log10(breaks = c(1e3, 1e4, 1e5, 1e6), labels = parse(text = c('10^3', 'Low~(10^4)', 'Medium~(10^5)', 'High~(10^6)')), 
                limits= c(1e3, NA)) +
  scale_x_continuous(breaks = seq(min(toplo2$yr), max(toplo2$yr), 1)) +
  geom_segment(data = toplomed2,
               aes(x = yr - 0.25, xend = yr + 0.25,
                   y = medv, yend = medv),
               linewidth = 1
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = NULL,
    y = 'Bloom intensity (cells / L)', 
    title = expression(paste(italic('Karenia brevis'), ' bloom intensity in Tampa Bay')), 
    subtitle = 'Observed cell counts > 0 and annual medians', 
    caption = 'Source: NOAA NCEI Harmful Algal BloomS Observing System (HABSOS)'
  )

p <- p1 + p2 + plot_layout(ncol = 1, axis_titles = 'collect')

jpeg('figures/habs.jpg', family = fml, height = 5, width = 9, units = 'in', res = 300)
print(p)
dev.off()

# simplified land use change ------------------------------------------------------------------

load(url("https://github.com/tbep-tech/hmpu-workflow/raw/master/data/chgdat.RData"))

toplo <- chgdat %>%
  filter(grepl('1990', source)) %>% 
  filter(grepl('2020', target)) %>% 
  mutate(
    source = case_when(
      grepl('Mangrove|Salt|Wetlands|Uplands', source) ~ 'Forests/Wetlands, 1990',
      T ~ source
    ),
    target = case_when(
      grepl('Mangrove|Salt|Wetlands|Uplands', target) ~ 'Forests/Wetlands, 2020',
      T ~ target
    )
  ) %>% 
  summarise(
    value = sum(value), 
    .by = c('source', 'target')
  ) %>% 
  filter(!grepl('other', source)) %>% 
  filter(!grepl('other', target))

colvec <- c("#FF6347", "#2B775D", "#004F7E", "#82746E")
p <- alluvout2(toplo, family = fml, maxyr = 2020, width = 1000, height = 700, mrg = 95, 
          colvec = colvec, title = F, fontsize = 25)

htmlwidgets::saveWidget(p, here::here('figures/landusechange.html'), selfcontained = T)

webshot::webshot(url = here::here('figures/landusechange.html'), file = here::here('figures/landusechange.png'))

# sea level rise ------------------------------------------------------------------------------

sealevelstations <- tibble::tribble(
  ~station_id, ~station_name,
  # 8726724    , "Clearwater Beach",
  # 8726674    , "East Bay",
  # 8726384    , "Port Manatee",
  8726520    , "St. Petersburg",
  # 8726667    , "McKay Bay",
  # 8726607    , "Old Port Tampa"
)

sealevelstations <- sealevelstations |>
  mutate(
    lst_station = map(station_id, get_stations),
    longitude   = map_dbl(lst_station, "lng"),
    latitude    = map_dbl(lst_station, "lat"),
    lst_details = map(station_id, get_details),
    date_est    = map_chr(lst_details, "established") |>
      as.Date() ) |>
  select(-lst_station, -lst_details)

tds <- sealevelstations %>% 
  select(station_name, station_id) %>% 
  mutate(
    data = purrr::map(station_id, function(station_id){
      
      cat(station_id)
      
      url <- paste0('https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?begin_date=19400101&end_date=20231231&product=monthly_mean&datum=MLLW&application=DataAPI_Sample&station=', station_id, '&time_zone=LST&units=english&format=CSV')
      
      res <- try(read.table(url, sep = ',', header = T), silent = T)
      
      if(inherits(res, 'try-error')){
        cat('\tno data\n')
        return(NULL)
      }
      
      cat('\n')
      
      return(res)
      
    })
  ) %>% 
  unnest(data) 

toplo <- tds %>% 
  select(station_name, yr = Year, mo = Month, msl = MSL) %>% 
  mutate(
    date = lubridate::make_date(yr, mo), 
    dectime = lubridate::decimal_date(date)
  ) %>% 
  # filter(yr >=2000) %>% 
  # filter(!(yr < 1999 & station_name %in% c('McKay Bay', 'Old Port Tampa', 'Port Manatee'))) %>% 
  mutate(
    station_name = case_when(
      grepl('East|McKay', station_name) ~ 'East/McKay Bay', 
      T ~ station_name
    ), 
    time_period = ifelse(yr < 2000, 'pre-2000', '2000-present'), 
    time_period = factor(time_period, c('pre-2000', '2000-present'))
  )

txtplo <- toplo %>% 
  group_nest(station_name, time_period) %>%
  mutate(
    data = map(data, ~{
      mod <- lm(msl ~ dectime, data = .x)
      pyr <- round(12 * coef(mod)[2], 2) # feet/yr to inches/yr
      pdc <- round(10 * pyr, 2) # inches/yr to inches/decade
      out <- glue::glue('{pdc} in/decade')
      return(out)
    })
  ) %>% 
  unnest(data) %>% 
  unite('data', data, time_period, sep = ' ', remove = F) %>% 
  rename(txt = data)

toplo <- toplo %>% 
  full_join(txtplo, by = c('station_name', 'time_period'))

p <- ggplot(toplo, aes(date, msl)) +
  geom_point(color = 'grey', size = 1) +
  # stat_smooth(formula = y ~ x, se = T, method = 'glm', color = 'black', method.args = list(family = gaussian(link = 'log'))) +
  stat_smooth(aes(group = time_period, color = txt, fill = txt), method = 'lm', formula = y ~ x, se = T) +
  # geom_line(data = log.model.df, aes(x, y, color = "Log Model"), size = 1, linetype = 2) + 'black') +
  scale_color_manual(values = c('red', 'red4')) +
  scale_fill_manual(values = c('red', 'red4')) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'top'
  ) + 
  labs(
    x = NULL, 
    y = 'Mean sea level (ft)', 
    title = 'Daily sea level at St. Petersburg, 1947 to present', 
    fill = NULL, 
    color = NULL
  )
# 
# tomap <- sealevelstations %>% 
#   mutate(
#     station_name = case_when(
#       grepl('East|McKay', station_name) ~ 'East/McKay Bay', 
#       T ~ station_name
#     )
#   ) %>% 
#   summarise(
#     longitude = mean(longitude, na.rm = T),
#     latitude  = mean(latitude, na.rm = T), 
#     .by = station_name
#   ) %>% 
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
# 
# dat_ext <- tomap %>% 
#   sf::st_as_sfc() %>% 
#   sf::st_buffer(dist = units::set_units(10, kilometer)) %>%
#   sf::st_transform(crs = 4326) %>% 
#   sf::st_bbox()
# 
# tls <- maptiles::get_tiles(dat_ext, provider = "CartoDB.Positron", zoom = 11)
# 
# m <- ggplot() +
#   tidyterra::geom_spatraster_rgb(data = tls, maxcell = 1e8) +
#   ggplot2::geom_sf(data = tomap, color = 'black', inherit.aes = F, size = 3) +
#   ggplot2::coord_sf(xlim = dat_ext[c(1, 3)], ylim = dat_ext[c(2, 4)], expand = FALSE, crs = 4326) + 
#   ggplot2::facet_wrap(~station_name, ncol = 1) + 
#   theme_minimal() +
#   theme(
#     axis.text = element_blank(), 
#     strip.text = element_blank()
#   ) + 
#   labs(caption = 'Source: NOAA Tides & Currents')
# 
# pout <- p + m + plot_layout(ncol = 2, widths = c(1.5, 1))

png(here::here('figures/sealevel.png'), family = fml, height = 3, width = 5, units = 'in', res = 300)
print(p)
dev.off()

# sea level rise one year ---------------------------------------------------------------------

sealevelstations <- tibble::tribble(
  ~station_id, ~station_name,
  # 8726724    , "Clearwater Beach",
  # 8726674    , "East Bay",
  # 8726384    , "Port Manatee",
  8726520    , "St. Petersburg",
  # 8726667    , "McKay Bay",
  # 8726607    , "Old Port Tampa"
)

sealevelstations <- sealevelstations |>
  mutate(
    lst_station = map(station_id, get_stations),
    longitude   = map_dbl(lst_station, "lng"),
    latitude    = map_dbl(lst_station, "lat"),
    lst_details = map(station_id, get_details),
    date_est    = map_chr(lst_details, "established") |>
      as.Date() ) |>
  select(-lst_station, -lst_details)


tds <- sealevelstations %>% 
  select(station_name, station_id) %>% 
  mutate(
    data = purrr::map(station_id, function(station_id){
      
      cat(station_id)
      
      url <- paste0('https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?begin_date=20230101&end_date=20231231&product=hourly_height&datum=MLLW&application=DataAPI_Sample&station=', station_id, '&time_zone=LST&units=english&format=CSV')
      
      res <- try(read.table(url, sep = ',', header = T), silent = T)
      
      if(inherits(res, 'try-error')){
        cat('\tno data\n')
        return(NULL)
      }
      
      cat('\n')
      
      return(res)
      
    })
  ) %>% 
  unnest(data) 

toplo <- tds %>% 
  mutate(
    Date.Time = ymd_hm(Date.Time, tz = 'America/Jamaica'),
    Date = as.Date(Date.Time)
  ) 

p <- ggplot(toplo, aes(x = Date.Time, y = Water.Level)) +
  geom_line(linewidth = 0.25) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
  ) +
  labs(
    y = 'Water Level (ft)', 
    title = 'Hourly sea level at St. Petersburg, 2023', 
    x = NULL
  )

png(here::here('figures/sealevel2.png'), family = fml, height = 3, width = 5, units = 'in', res = 300)
print(p)
dev.off()

# CCHA change ---------------------------------------------------------------------------------

load(file = url('https://github.com/tbep-tech/ccha-workflow/raw/main/data/vegdat.RData'))

toplo <- vegdat %>% 
  select(site, sample, meter, zone_name_simp) %>%
  distinct() %>% 
  mutate(
    grp = consecutive_id(zone_name_simp), 
    .by = c(site, sample)
  ) %>%
  summarise(
    dist_m = max(meter) - min(meter),
    .by = c(site, sample, zone_name_simp, grp)
  ) %>% 
  pivot_wider(names_from = sample, values_from = dist_m) %>% 
  na.omit() %>% # this removes any where a zone is missing in sample 1 or 3
  summarise(
    `1` = sum(`1`, na.rm = T),
    `3` = sum(`3`, na.rm = T),
    .by = zone_name_simp
  ) %>%
  mutate(
    perchg = (`3` - `1`) / `1`,
    sgn = sign(perchg),
    sgn = factor(ifelse(sgn == 0, -1, sgn))
  ) %>% 
  filter(!zone_name_simp %in% c('Water body'))

p <- ggplot(toplo, aes(y = reorder(zone_name_simp, perchg), x = perchg)) +
  geom_col(aes(fill = sgn), show.legend = F, color = 'black') +
  geom_text(data = toplo[toplo$sgn == 1, ], aes(label = scales::percent(perchg, accuracy = 1)), hjust = -0.1, size = 4.5) +
  geom_text(data = toplo[toplo$sgn == -1, ], aes(label = scales::percent(perchg, accuracy = 1)), hjust = 1.1, size = 4.5) +
  scale_fill_manual(values = c('#004F7E', '#00806E')) + 
  geom_vline(xintercept = 0, linetype = 'solid') +
  scale_x_continuous(expand = c(0.1, 0)) +
  labs(
    x = NULL,
    y = NULL,
    title = '% change in total transect distance of critical coastal habitats',
    subtitle = '2015 to 2023', 
    caption = 'Source: FWC FWRI Coastal Wetlands Research Program'
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_text(size = 12)
  )

png(here::here('figures/cchachange.png'), family = fml, height = 4, width = 8, units = 'in', res = 300)
print(p)
dev.off()

# TN load by source over time -----------------------------------------------------------------

load(url("https://github.com/tbep-tech/load-estimates/raw/main/data/tnanndat.RData"))

toplo <- tnanndat %>% 
  filter(grepl('^All', bay_segment)) %>% 
  mutate(
    source = case_when(
      source == 'AD' ~ 'Atmospheric',
      source == 'GWS' ~ 'Groundwater',
      source %in% c('IPS', 'DPS') ~ 'Point Source',
      source == 'NPS' ~ 'Stormwater'
    )
  ) %>% 
  summarise(
    tn_load = sum(tn_load, na.rm = T), 
    .by = c(year, source)
  ) %>% 
  mutate(
    tn_load = tn_load / sum(tn_load),
    .by = c(year)
  )

cols <- c( '#00806E', '#004F7E', '#5C4A42', '#427355') 

p <- ggplot(toplo, aes(x = year, y = tn_load, group = source, color = source, fill = source)) + 
  geom_point(show.legend = F) +
  stat_smooth(method = 'lm', se = F, show.legend = F, formula = y ~ x) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  facet_wrap(~source, ncol = 4) +
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = NULL, 
    y = '% total load', 
    title = 'Tampa Bay Nitrogen Sources Over Time'
  )

png(here::here('figures/tnloadbysource.png'), family = fml, height = 2.5, width = 7, units = 'in', res = 300)
print(p)
dev.off()

# OTB aug 2024 pyro cyst counts ---------------------------------------------------------------

cystdat <- read.csv(here('data-raw/CystCountsEPCHCFWC2024.csv'))

# join data to tidalcreeks sf
tomap <- cystdat %>%
  rename(
    cysts_pergwetsed = P..bah.Cysts..per.g.wet.sediment.
  ) %>% 
  mutate(
    cyst_cat = cut(cysts_pergwetsed, breaks = c(-Inf, 101, 501, 1001, 5001, 10001, Inf),
                   right = F, labels = c('0 - 100', '101 - 500', '501 - 1000', '1001 - 5000', '5001 - 10000', '> 10000')
    )
  ) %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)

# base tiles
bbx <- tomap %>% 
  sf::st_bbox() %>% 
  sf::st_as_sfc() %>% 
  sf::st_buffer(dist = units::set_units(3, kilometer)) %>%
  sf::st_transform(crs = 4326) %>% 
  sf::st_bbox()

tls <- maptiles::get_tiles(bbx, provider = "CartoDB.PositronNoLabels", zoom = 12)
dat_ext <- sf::st_as_sfc(bbx) %>% 
  sf::st_transform(crs = 4326) %>% 
  sf::st_bbox()

cols <- c('grey', RColorBrewer::brewer.pal(length(levels(tomap$cyst_cat)) - 1, 'Reds'))

p <- ggplot() + 
  geom_spatraster_rgb(data = tls, maxcell = 1e8) +
  geom_sf(data = tomap, aes(fill = cyst_cat, size = cyst_cat), inherit.aes = F, linewidth = 1, pch = 21, color = 'darkgrey', show.legend = T) +
  scale_fill_manual(values = cols, drop = F) +
  scale_size_manual(values = seq(2, 7, length = length(levels(tomap$cyst_cat))), drop = F) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 9),
    legend.position = 'inside',
    legend.position.inside = c(0.15, 0.25),
    legend.background = element_blank(), 
    legend.key = element_blank(), 
    legend.title = element_text(size = 14)
  ) +
  labs(
    fill = 'cysts per gram', 
    size = 'cysts per gram'
  ) +
  annotate('text', x = bbx$xmin, y = bbx$ymax, label = 'Aug 2024', hjust = -0.2, vjust = 2, size = 6) +
  ggspatial::annotation_north_arrow(location = 'tr') +
  ggspatial::annotation_scale(location = 'bl', unit_category = 'metric') +
  coord_sf(xlim = dat_ext[c(1, 3)], ylim = dat_ext[c(2, 4)], expand = FALSE, crs = 4326)

jpeg('figures/pyrocyst.jpg', family = fml, height = 6, width = 7, units = 'in', res = 300)
print(p)
dev.off()

# water quality report card -------------------------------------------------------------------

maxyr <- 2024

# local file path
# xlsx <- here('data-raw/Results_Updated.xls')
xlsx <- here('data-raw/Results_Provisional.xlsx')

# import and download if new
epcdata <- read_importwq(xlsx, download_latest = F)

p <- show_matrix(epcdata, yrrng = c(1975, maxyr), txtsz = 3, abbrev = T, historic = T, family = fml) 

jpeg('figures/waterqualityreportcard.jpg', family = fml, height = 8, width = 2.5, units = 'in', res = 300)
print(p)
dev.off()

# water quality report card map ---------------------------------------------------------------

# local file path
# xlsx <- here('data-raw/Results_Updated.xls')
xlsx <- here('data-raw/Results_Provisional.xlsx')

# import and download if new
epcdata <- read_importwq(xlsx, download_latest = F)

jpeg('figures/waterqualityreportcardmap.jpg', family = fml, height = 6, width = 6, units = 'in', res = 300)
show_sitesegmap(epcdata, yrsel = 2024)
dev.off()

# habitat report card -------------------------------------------------------------------------

p1 <- show_hmpreport(acres = acres, subtacres = subtacres, hmptrgs = hmptrgs, typ = 'targets',
                     strata = 'Subtidal', twocol = T, ycollapse = T, xang = 30)
p2 <- show_hmpreport(acres = acres, subtacres = subtacres, hmptrgs = hmptrgs, typ = 'targets',
                     strata = c('Intertidal', 'Supratidal'), totintertid = F, ycollapse = T, twocol = T, xang = 30)

p <- p1 + p2 + plot_layout(ncol = 2, guides = 'collect', widths = c(0.6, 1)) & labs(title = NULL)

jpeg('figures/habitatreportcard.jpg', family = fml, height = 5.5, width = 6, units = 'in', res = 300)
print(p)
dev.off()

# SSOs ----------------------------------------------------------------------------------------

load(url('https://github.com/tbep-tech/sso-reporting/raw/refs/heads/main/data/vols.RData'))

maxyr <- 2024

toplo1 <- vols |>
  group_by(yr, bay_segment) |>
  summarise(volest = sum(volest), .groups = 'drop') |>
  mutate(
    volest = volest / 1e6, 
    bay_segment = factor(bay_segment, 
                         levels = c('OTB', 'HB', 'MTB', 'LTB', 'BCB', 'MR', 'TCB'), 
                         labels = c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 
                                    'Lower Tampa Bay', 'Boca Ciega Bay', 'Manatee River', 
                                    'Terra Ceia Bay'))
  ) %>% 
  filter(yr >= 2022 & yr <= maxyr)
toplo2 <- vols |>
  filter(yr == maxyr) |>
  group_by(mo, bay_segment) |>
  summarise(volest = sum(volest), .groups = 'drop') |>
  mutate(
    volest = volest / 1e6,
    bay_segment = factor(bay_segment, levels = c('OTB', 'HB', 'MTB', 'LTB', 'BCB', 'MR', 'TCB'),
                         labels = c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 
                                    'Lower Tampa Bay', 'Boca Ciega Bay', 'Manatee River', 
                                    'Terra Ceia Bay')),
    mo = factor(mo, levels = c(1:12), labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
  )

cols <- c("#427355", "#5C4A42", "#958984", "#EA6F17", "#004F7E", "#00806E")
pal <- colorRampPalette(cols)(length(levels(toplo1$bay_segment)))

p1 <- ggplot(toplo1, aes(x = yr, y = volest, fill = bay_segment)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = pal) +
  scale_x_continuous(breaks = unique(toplo1$yr)) +
  labs(
    x = NULL, 
    y = 'Million gallons', 
    title = 'Estimated sewer overflow volume', 
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),  
    panel.grid.major.x = element_blank()
  )

p2 <- ggplot(toplo2, aes(x = mo, y = volest, fill = bay_segment)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = pal) +
  labs(
    x = NULL,
    y = 'Million gallons',
    subtitle = '2024 by month',
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

jpeg('figures/ssoyr.jpg', family = fml, height = 3.5, width = 4, units = 'in', res = 300)
print(p1)
dev.off()

jpeg('figures/ssomo.jpg', family = fml, height = 3.5, width = 6, units = 'in', res = 300)
print(p2)
dev.off()

# Seagrass segment change ---------------------------------------------------------------------

# sgdat2020 <- rdataload('https://github.com/tbep-tech/hmpu-workflow/raw/refs/heads/master/data/sgdat2020.RData')
# sgdat2022 <- rdataload('https://github.com/tbep-tech/hmpu-workflow/raw/refs/heads/master/data/sgdat2022.RData')

# bay segments detailed
segclp <- rdataload('https://github.com/tbep-tech/seagrass-analysis/raw/refs/heads/main/data/segclp.RData')

# data(file = 'sgseg', package = 'tbeptools')
# 
# bnds <- sgseg %>% 
#   filter(segment %in% c('Boca Ciega Bay', 'Hillsborough Bay', 'Old Tampa Bay', 'Middle Tampa Bay',
#                         'Lower Tampa Bay', 'Manatee River', 'Terra Ceia Bay'))
# 
# flcat <- list(
#   code = c('9113', '9116'),
#   name = c('patchy', 'cont.')
# )
# 
# ##
# # process coverage ests by segment and year
# 
# allsegests <- tibble(
#   yr = c(2020, 2022)
#   ) %>% 
#   group_by(yr) %>% 
#   nest() %>% 
#   mutate(
#     data = pmap(list(yr, data), function(yr, data){
#       
#       cat(yr, '\n')
#     
#       # make sure crs is the same, get relevant fluccs
#       sgrs <- get(paste0('sgdat', yr)) %>% 
#         st_transform(crs = st_crs(bnds)) %>% 
#         filter(FLUCCSCODE %in% flcat[['code']])
#       
#       # estimate coverage by flucss, segment
#       ests <- st_intersection(sgrs, bnds) %>% 
#         mutate(area = st_area(.)) %>% 
#         st_set_geometry(NULL) %>% 
#         group_by(segment, FLUCCSCODE) %>% 
#         summarise(area = sum(area), .groups = 'drop') %>% 
#         mutate(
#           Acres = as.numeric(set_units(area, 'acres')), 
#           Hectares = as.numeric(set_units(area, 'hectares'))
#         ) %>% 
#         select(Segment = segment, Habitat = FLUCCSCODE, Acres, Hectares) %>% 
#         mutate(
#           Habitat = factor(Habitat, levels = flcat$code, labels = flcat$name)
#         )
#       
#       return(ests)
#       
#     })
#   ) %>% 
#   unnest('data')

# save(allsegests, file = here('data/allsegests.RData'))

##
# map

# colnm <- 'Segment'
# 
# tomap <- allsegests %>% 
#   filter(Habitat %in% c('patchy', 'cont.')) %>% 
#   group_by(yr, Segment) %>% 
#   summarise(
#     Acres = sum(Acres, na.rm = T), 
#     .groups = 'drop'
#   ) %>% 
#   spread(yr, Acres, fill = 0) %>% 
#   sgchgfun(c('2020', '2022'), colnm) %>% 
#   full_join(segclp, ., by = c('segment'= 'val'))

# get change summary
tomap <- tibble(
    segment = c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Boca Ciega Bay', 'Manatee River', 'Terra Ceia Bay'), 
    chg = c(-327, 756, 230, 403, 344, 2, 1)
  ) %>% 
  full_join(segclp, ., by = 'segment')

maxv <- max(abs(tomap$chg))

# text labels
totxt <- tomap %>% 
  st_centroid() %>% 
  mutate(
    txt = ifelse(sign(chg) == -1, '-', '+'),
    txt = paste0(txt, abs(round(chg, 0))) #paste0(bay_segment, ': ', abs(round(chg, 0)), ' acres ', txt)
  ) # %>% 
  # filter(bay_segment %in% c('OTB', 'HB', 'MTB', 'LTB'))

# colors
colgrn <- c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", 
            "#238B45", "#006D2C", "#00441B")
colred <- c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", 
            "#CB181D", "#A50F15", "#67000D")               
colfun <- leaflet::colorNumeric(
  palette = c(rev(colred), colgrn),
  domain = c(-1 * maxv, maxv)
)

# bbox
dat_ext <- tomap %>% 
  sf::st_as_sfc() %>% 
  sf::st_buffer(dist = units::set_units(2, kilometer)) %>%
  sf::st_transform(crs = 4326) %>% 
  sf::st_bbox()

tls <- maptiles::get_tiles(dat_ext, provider = 'CartoDB.PositronNoLabels', zoom = 10)

m <- ggplot2::ggplot() + 
  tidyterra::geom_spatraster_rgb(data = tls, maxcell = 1e8) +
  ggplot2::geom_sf(data = tomap, ggplot2::aes(fill = chg), color = 'black', inherit.aes = F) +
  # ggplot2::geom_sf_label(data = totxt, ggplot2::aes(label = txt), size = 4, alpha = 0.8, inherit.aes = F) +
  scale_fill_gradientn(
    colors = c(rev(colred), colgrn),
    values = scales::rescale(c(seq(-maxv, 0, length.out = length(colred)),
                               seq(0, maxv, length.out = length(colgrn)))),
    limits = c(-maxv, maxv),
    na.value = "grey50"
  ) +
  # ggplot2::scale_fill_distiller(name = 'Change (acres)', palette = palcol, direction = palcolrev) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(), 
    axis.title = ggplot2::element_blank(), 
    axis.text.y = element_blank(), #ggplot2::element_text(size = ggplot2::rel(0.9)), 
    axis.text.x = element_blank(), #ggplot2::element_text(size = ggplot2::rel(0.9), angle = 30, hjust = 1),
    axis.ticks = element_blank(), #ggplot2::element_line(colour = 'grey'),
    panel.background = ggplot2::element_rect(fill = NA, color = 'black'), 
    legend.position = 'top', 
    legend.title.position = 'top',
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.25, "cm"),
    legend.title = element_text(hjust = 0.5)
  ) +
  # ggspatial::annotation_scale(location = 'br', unit_category = 'metric') +
  # ggspatial::annotation_north_arrow(location = 'tl', which_north = "true") + 
  ggplot2::labs(
    fill = '2022 - 2024 change (acres)'
  )

dat_ext <- dat_ext %>% 
  sf::st_as_sfc(dat_ext) %>% 
  sf::st_transform(crs = 4326) %>% 
  sf::st_bbox()

# set coordinates because vector not clipped
m <- m +
  ggplot2::coord_sf(xlim = dat_ext[c(1, 3)], ylim = dat_ext[c(2, 4)], expand = FALSE, crs = 4326)

jpeg('figures/sgchange.jpg', family = fml, height = 5.5, width = 4.25, units = 'in', res = 300)
print(m)
dev.off()

# rainfall ------------------------------------------------------------------------------------

# monthly rainfall data from swfwmd 
# https://www.swfwmd.state.fl.us/resources/data-maps/rainfall-summary-data-region
# file is from the link "USGS watershed"
download.file(
  'https://www4.swfwmd.state.fl.us/RDDataImages/surf.xlsx?_ga=2.186665249.868698214.1705929229-785009494.1704644825',
  here('data-raw/swfwmdrainfall.xlsx'),
  mode = 'wb'
)

# rain data for relevant TB areas
raindat <- readxl::excel_sheets(here('data-raw/swfwmdrainfall.xlsx')) %>% 
  grep('jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec', ., value = TRUE) %>% 
  tibble(
    mo = .
  ) %>% 
  nest(.by = mo) %>% 
  mutate(
    data = purrr::map(mo, function(mo){
      
      read_excel(here('data-raw/swfwmdrainfall.xlsx'), sheet = mo, skip = 1) %>% 
        filter(Year %in% 2009:2024) %>% 
        select(
          yr = Year, 
          tampacoastal_in = `Tampa Bay/Coastal Areas`, # this just a fringe area around the bay, not the watershed
          hillsborough_in = `Hillsborough River`,
          alafia_in = `Alafia River`,
          littlemanatee_in = `Little Manatee River`,
          manatee_in = `Manatee River`
        ) %>% 
        mutate_all(as.numeric)
      
    })
  ) %>% 
  unnest('data') %>% 
  # mutate(
  #   precip_in = rowSums(select(., -mo, -yr), na.rm = TRUE)
  # ) %>%
  select(mo, yr, precip_in = tampacoastal_in) %>% 
  mutate(
    mo = gsub('\\-usgsbsn$', '', mo),
    mo = as.numeric(factor(mo,
                           levels = c('jan', 'feb', 'mar', 'apr', 'may', 'jun',
                                      'jul', 'aug', 'sep', 'oct', 'nov', 'dec'),
                           labels = 1:12)
    ),
    date = as.Date(paste0(yr, '-', mo, '-01')), 
    precip_mm = precip_in * 25.4
  ) %>% 
  summarise(
    precip_in = sum(precip_in, na.rm = T), 
    .by = yr
  ) %>% 
  mutate(
    ave = mean(precip_in), 
    avediff = precip_in - ave
  )

toplo <- raindat %>% 
  filter(yr > 2021)

p <- ggplot(toplo, aes(x = yr, y = precip_in)) + 
  geom_col(aes(fill = 'Annual\nrainfall'), color = 'black', alpha = 0.8) +
  geom_col(aes(y = avediff, fill = 'Deviation from\n15-yr mean'), width = 0.5, color = 'black', alpha = 0.8) + 
  scale_fill_manual(values = c('#004F7E', '#5C4A42')) +
  scale_color_manual(values = 'darkred') + 
  geom_hline(aes(yintercept = unique(ave), color = '15-yr mean'), linewidth = 1, linetype = 'dashed') + 
  theme_minimal(base_size = 14) + 
  scale_x_continuous(breaks = unique(toplo$yr)) +
  scale_y_continuous(expand= c(0, 0), n.breaks = 10) +
  theme(
    # axis.text.x =  element_blank(),
    # panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = 'top',
    legend.box = 'vertical',
    legend.spacing.y = unit(0, 'cm')
  ) +
  labs(
    x = NULL,
    y = 'Inches',
    color = NULL,
    fill = NULL
    # caption = 'Data source: SWFWMD'
  )

jpeg('figures/rain.jpg', family = fml, height = 4.5, width = 4, units = 'in', res = 300)
print(p)
dev.off()

# chlorophyll trend alternative graphic -------------------------------------------------------

toplo <- epcdata %>%
  filter(yr > 1974 & yr < 2023) %>% 
  summarise(
    avev = mean(chla, na.rm = T), 
    hiv = t.test(chla, na.rm = T)$conf.int[2],
    lov = t.test(chla, na.rm = T)$conf.int[1],
    .by = yr
  )

p <- ggplot(toplo, aes(x = yr, y = avev)) + 
  geom_point() +
  geom_errorbar(aes(ymin = lov, ymax = hiv), width = 0) +
  scale_y_continuous(breaks = seq(4, 20, by = 4)) +
  scale_x_continuous(expand = c(0.07, 0.07)) +
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + 
  labs(
    x = NULL, 
    y = expression(paste(mu, "g/L")),
    title = 'Long-term Chlorophyll-a Trend'#, 
    # caption = 'Source: Environmental Protection Commission of Hillsborough County'
  )

png(here('figures/chla.png'), family = fml, height = 2, width = 7, units = 'in', res = 300)
print(p)
dev.off()

# seagrass trend alternative graphic ----------------------------------------------------------

dumyr <- 1975
toplo <- seagrass %>% 
  select(yr = Year, acres = Acres) %>% 
  filter(yr < 2024) %>% 
  mutate(
    acres = acres / 1000,
    yr = case_when(
      yr == 1950 ~ dumyr,
      T ~ yr
    )
  )

brks <- c(dumyr, seq(1982, 2022, by = 2))
labs <- c(1950, seq(1982, 2022, by = 2))

p <- ggplot(toplo, aes(x = yr, y = acres)) +
  geom_col(fill = '#00806E', color = 'black') +
  scale_x_continuous(breaks = brks, labels = labs) + 
  geom_vline(xintercept = dumyr + ((1982 - dumyr) / 2), color = 'black', linetype = 'dashed') + 
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + 
  labs(
    x = NULL, 
    y = 'Acres (x 1,000)',
    title = 'Long-term seagrass coverage'#, 
    # caption = 'Source: Southwest Florida Water Management District'
  )

png(here('figures/seagrasscovalt.png'), family = fml, height = 2, width = 7, units = 'in', res = 300)
print(p)
dev.off()

# seagrass coverage by segment ----------------------------------------------------------------


load(url('https://github.com/tbep-tech/tbep-os-presentations/raw/refs/heads/master/data/sgsegest.RData'))

# segment coverage targets in 1k acres
segtrgs <- tibble(
  segment = factor(c(levels(sgsegest$segment), 'Total')), 
  trgs = c(11.1, 1.751, 9.4, 7.4, 8.8, 1.1, 0.449, 40)
)  

# worst case coverage ests in 1k acres, 1982
segworst <- tibble(
  segment = factor(c(levels(sgsegest$segment), 'Total')), 
  trgs = c(5.94, 0, 4.04, 5.02, 5.77, 0.75, 0.13, 21.65)
) 

toplo <- sgsegest %>%
  filter(segment %in% c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay')) %>%
  mutate(acres = acres / 1000) %>%
  mutate(segment = forcats::fct_drop(segment))

subsegtrgs <- segtrgs %>%
  filter(segment %in% levels(toplo$segment))

# arrdf <- tibble(
#   segment = factor('Old Tampa Bay', levels = levels(toplo$segment)),
#   x = factor(2022),
#   xend = factor(2022),
#   y = 8,
#   yend =  5
# )

p <- ggplot(toplo, aes(x = factor(year), y = acres)) +
  geom_bar(fill = '#00806E', stat = 'identity', colour = 'black', width = 0.6) +
  geom_hline(data = subsegtrgs, aes(yintercept = trgs, color = 'Target')) +
  # geom_segment(
  #   data = arrdf,
  #   aes(x = x, xend = xend, y = y, yend = yend),
  #   arrow = arrow(length = grid::unit(0.5, "cm")),
  #   size = 2, lineend = 'round', linejoin = 'round', col = 'red'
  # ) +
  scale_color_manual(values = 'red') +
  facet_wrap(~segment, ncol = 2, scales = 'free') +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x =element_blank(),
        # plot.background = element_rect(fill = NA, color = NA),
        axis.text.y = element_text(colour = 'black'),
        plot.title = element_text(size = 22, colour = 'black'),
        legend.text = element_text(size = 16, colour = 'black'),
        axis.text.x = element_text(colour = 'black', angle = 45, size = 8, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(size = 13),
        legend.position = 'none'
  ) +
  labs(
    y = 'Seagrass Coverage (x1,000 acres)',
    x = NULL,
    color = NULL
  )

png(here('figures/seagrassseg.png'), family = fml, height = 4.5, width = 10, units = 'in', res = 300)
print(p)
dev.off()

