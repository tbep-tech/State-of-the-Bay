library(tbeptools)
library(tidyverse)
library(extrafont)
library(patchwork)
library(sf)
library(extrafont)
library(readxl)
library(ggfx)
library(grid)
library(here)
library(maptiles)
library(tidyterra)
library(networkD3)
library(htmltools)

loadfonts(device = 'win', quiet = T)

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

