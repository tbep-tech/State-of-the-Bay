library(tbeptools)
library(tidyverse)
library(extrafont)
library(patchwork)
library(ggmap)
library(sf)
library(extrafont)
library(readxl)
library(ggfx)
library(grid)
library(here)

loadfonts(device = 'win', quiet = T)

gkey <- Sys.getenv('google_key')
register_google(gkey)

source(here('R/funcs.R'))

fml <- "Lato"

# wq matrix map -----------------------------------------------------------

maxyr <- 2021
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

# tbni report card --------------------------------------------------------

p <- show_tbnimatrix(tbniscr, family = fml) +
  theme(
    text = element_text(family = fml),
    axis.text.y = element_text(family = fml)
  ) +
  ggtitle('Tampa Bay Nekton Index\nReport Card')

jpeg('figures/tbnireport.jpg', family = fml, height = 7, width = 3, units = 'in', res = 300)
print(p)
dev.off()

# tbbi report card --------------------------------------------------------

maxyrc <- 2020
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
tidres <- anlz_tdlcrk(tidcrk, iwrraw, yr = 2018)

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

# basemap
bbx <- tbseg %>%
  st_buffer(dist = 0.15) %>%
  st_bbox
names(bbx) <- c('left', 'bottom', 'right', 'top')
bsmap <- get_stamenmap(bbox = bbx, zoom = 10, maptype = 'toner-hybrid')#terrain-background')

cnt <- tbseg %>% group_by(bay_segment) %>% st_union() %>% st_centroid %>% st_coordinates()

# https://stackoverflow.com/questions/51767254/remove-country-and-city-names-from-map
s <- "element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative%7Celement:geometry%7Cvisibility:off&style=feature:administrative.country%7Celement:geometry.stroke%7Ccolor:0x000000%7Cvisibility:on&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Cvisibility:off&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road%7Celement:labels.icon%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit%7Cvisibility:off&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&size=480x360"
bsmap <- get_googlemap(cnt, zoom = 10, style = s)

p <- ggmap::ggmap(bsmap) +
  geom_sf(data = tomap, aes(colour = score, fill = score), inherit.aes = F, size = 1, pch = 21) +
  scale_fill_manual(values = cols, drop = F, guide = guide_legend(reverse = T)) +
  scale_colour_manual(values = cols, drop = F, guide = guide_legend(reverse = T)) +
  scale_x_continuous(limits = c(-82.86, -82.27)) +
  scale_y_continuous(limits = c(27.42, 28.07)) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 7),
    legend.position = c(0.15, 0.15),
    legend.background = element_blank(),
    legend.title = element_blank()
  ) +
  ggsn::scalebar(tomap, dist = 6, dist_unit = "km", st.size = 3, location = 'topright',
                 transform = TRUE, model = "WGS84", height = 0.015)

jpeg('figures/tidalcreekmap.jpg', family = fml, height = 9, width = 5, units = 'in', res = 300)
print(p)
dev.off()

# seagrass coverage -------------------------------------------------------

##
# save plot

png('figures/seagrasscov.png', height = 3.25, width = 6, res = 300, unit = 'in')
sgcov_plo(seagrass, family = fml)
dev.off()
