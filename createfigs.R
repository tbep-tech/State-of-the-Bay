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

loadfonts(device = 'win', quiet = T)

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
# data prep

# extra years for padding
exyrs <- seq(1950, 1953)

toplo <- tibble(
  Year = c(exyrs, seq(1982, 2020))
) %>%
  left_join(seagrass, by = 'Year') %>%
  mutate(
    Acres = Acres / 1000
  )

# label for last bar 
lastlab <- seagrass %>% 
  filter(Year == max(Year)) %>% 
  pull(Acres) %>% 
  round(0) %>% 
  format(big.mark = ',') %>% 
  paste(., 'acres')
 
# y loc for last bar label
lasty <- seagrass %>% 
  filter(Year == max(Year)) %>% 
  pull(Acres) %>% 
  `/`(1000) %>% 
  `-`(1)

##
# base ggplot

# axis labels
lbs <- toplo$Year
lbs[lbs %in% exyrs[-1]] <- ''

p <- ggplot(toplo, aes(x = factor(Year), y = Acres)) +
  with_shadow(geom_bar(fill = '#00806E', stat = 'identity', colour = 'black', width = 1.3), sigma = 2.7, x_offset = 0, y_offset = 0) +
  geom_segment(x = 0, xend = 2, y = 38, yend = 38, col = 'red', size = 2) +
  geom_segment(x = 4, xend = 42, y = 38, yend = 38, col = 'red', size = 2) +
  geom_segment(x = 42, xend = 44, y = 40, yend = 40, col = 'red', size = 2) +
  annotate("text", label = "Seagrass Coverage Goal", x = 4, y = 40.5, color = 'red', size = 5, hjust = 0, family = fml) +
  annotate('text', x = 43, y = lasty, label = lastlab, angle = 90, hjust = 1, vjust = 0.4) + 
  scale_x_discrete(breaks = lbs, labels = lbs, expand = c(0.04, 0.04)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1 * max(toplo$Acres, na.rm = T))) +
  # theme_bw() +
  theme(
    axis.line = element_line(),
    panel.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    axis.title.x = element_blank(),
    legend.position = 'none',
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  labs(
    y = 'Seagrass Coverage (x1,000 acres)'
  )

##
# top, bottom axis line breaks

gt <- ggplotGrob(p)

is_axisb <- which(gt$layout$name == "axis-b")
is_axist <- which(gt$layout$name == "axis-t")
is_axisl <- which(gt$layout$name == "axis-l")
is_axisr <- which(gt$layout$name == "axis-r")

axisb <- gt$grobs[[is_axisb]]
xline <- axisb$children[[1]]

# location of break, break type
xline$y <- unit(rep(1, 4), "npc")
xline$x <- unit(c(0, 0.06, 1, 0.105), "npc")
xline$id <- c(1, 1, 2, 2)
xline$arrow <- arrow(angle = 90, length = unit(0.07, 'inches'))

axisb$children[[1]] <- xline
axist <- xline
axisl <- gt$grobs[[is_axisl]]

gt$grobs[[is_axisb]] <- axisb
gt$grobs[[is_axist]] <- axist
gt$grobs[[is_axisr]] <-axisl$children[[1]]

##
# save plot

png('figures/seagrasscov.png', height = 3.25, width = 6, res = 300, unit = 'in')
grid.newpage(); grid.draw(gt)
dev.off()
