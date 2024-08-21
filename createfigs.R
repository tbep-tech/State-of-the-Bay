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

maxyr <- 2022
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
  filter(!grepl('Open|other', source)) %>% 
  filter(!grepl('Open|other', target))

p <- alluvout2(toplo, family = fml, maxyr = 2020, width = 1000, height = 700, mrg = 95, 
          colrev = T, title = F)

htmlwidgets::saveWidget(p, here::here('figures/landusechange.html'), selfcontained = T)

webshot::webshot(url = here::here('figures/landusechange.html'), file = here::here('figures/landusechange.png'))
