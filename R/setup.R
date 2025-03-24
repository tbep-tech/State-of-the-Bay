library(knitr)
library(tbeptools)
library(dplyr)
library(tidyr)
library(tibble)
library(patchwork)
library(ggmap)
library(sf)
library(readxl)
library(grid)
library(here)
library(htmltools)
library(plotly)
library(reactable)
library(reactablefmtr)
library(googlesheets4)
library(googledrive)
library(emojifont)
library(showtext)
library(stringr)
library(crosstalk)
library(leaflet)
library(DT)
library(lubridate)
library(networkD3)
library(fontawesome)

# auth google drive
drive_auth(email = 'mbeck@tbep.org')
gs4_auth(token = drive_token())

# get font
font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
fml <- "roboto"

showtext_auto()
showtext_opts(dpi = 300)

# knitr globals
knitr::opts_chunk$set(message = F, echo = F, warning = F, fig.align = 'center')

# plotly svg download on modebar 
plocnf <- list(
  format = "svg",
  filename = "myplot"
)

source(here('R/funcs.R'))


