library(sf)
library(ggspatial)
library(ggplot2)
library(tbeptools)
library(here)
library(showtext)

# get font
font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
fml <- "roboto"

showtext_auto()
showtext_opts(dpi = 500)

load(url('https://github.com/tbep-tech/hmpu-workflow/raw/master/data/hard.RData'))
hard <- st_make_valid(hard)# %>% 
  # st_buffer(dist = 30)

subttl <- paste(round(sum(hard$Acres), 0), 'Acres')
p <- ggplot() +
  annotation_map_tile(zoom = 11, type = 'cartolight') +
  geom_sf(data = tbseg, color = NA, fill = NA) + 
  geom_sf(data = hard, color = 'red', fill = 'red') + 
  theme_minimal(base_family = fml) + 
  theme(
    axis.text= element_blank(), 
    axis.ticks = element_blank()
  ) + 
  labs(
    title = "Hard-bottom habitat in Tampa Bay", 
    subtitle = subttl
  )

jpeg(here('figures/hardbottom.jpg'), family = fml, height = 5, width = 3.1, res = 500, units = 'in')
print(p)
dev.off()
