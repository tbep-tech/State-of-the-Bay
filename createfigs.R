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

# tbni report card --------------------------------------------------------

tbniscr <- anlz_tbniscr(fimdata)

p <- show_tbnimatrix(tbniscr, family = fml) +
  theme(
    text = element_text(family = fml),
    axis.text.y = element_text(family = fml)
  )
jpeg('figures/tbnireport.jpg', family = fml, height = 6, width = 3, units = 'in', res = 300)
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
