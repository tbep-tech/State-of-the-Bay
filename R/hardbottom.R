library(sf)
library(ggspatial)
library(ggplot2)
library(tbeptools)
library(here)
library(showtext)
library(dplyr)

# get font
font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
fml <- "roboto"

showtext_auto()
showtext_opts(dpi = 500)

load(url('https://github.com/tbep-tech/hmpu-workflow/raw/master/data/hard.RData'))
hard <- st_make_valid(hard) %>% 
  st_transform(crs = st_crs(tbseg)) %>% 
  st_intersection(., tbseg) %>% 
  mutate(long_name = factor(long_name, levels = c('Old Tampa Bay', 'Middle Tampa Bay', 'Lower Tampa Bay')))
  # st_buffer(dist = 30)

# hard bottom
for(lev in levels(hard$long_name)){
  
  tomap <- hard %>% 
    filter(long_name %in% lev)
  seg <- tbseg %>% 
    filter(long_name %in% lev)
  ttl <- paste('Hard Bottom in', lev)
  subttl <- paste(round(sum(tomap$Acres), 0), 'Acres')
  levshrt <- unique(tomap$bay_segment)
  flnm <- here(paste0('figures/hardbottom', levshrt, '.jpg'))
  
  p <- ggplot() +
    annotation_map_tile(zoom = 11, type = 'cartolight') +
    geom_sf(data = seg, color = NA, fill = NA) + 
    geom_sf(data = tomap, color = 'red', fill = 'red') + 
    theme_minimal(base_family = fml) + 
    theme(
      axis.text= element_blank(), 
      axis.ticks = element_blank()
    ) + 
    labs(
      title = ttl, 
      subtitle = subttl
    )
  
  jpeg(flnm, family = fml, height = 5, width = 5, res = 500, units = 'in')
  print(p)
  dev.off()

}

# segment insets
for(lev in levels(hard$long_name)){

  seg <- tbseg %>% 
    filter(long_name %in% lev)
  levshrt <- seg %>% 
    pull(bay_segment)
  flnm <- here(paste0('figures/hardbottominset', levshrt, '.jpg'))
  
  p <- ggplot() +
    annotation_map_tile(zoom = 11, type = 'cartolight') +
    geom_sf(data = tbseg, color = NA, fill = NA) + 
    geom_sf(data = seg, color = 'black', fill = 'grey') + 
    theme_minimal(base_family = fml) + 
    theme(
      axis.text= element_blank(), 
      axis.ticks = element_blank()
    )
  
  jpeg(flnm, family = fml, height = 5, width = 5, res = 500, units = 'in')
  print(p)
  dev.off()
  
}