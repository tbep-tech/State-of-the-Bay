library(knitr)
library(tbeptools)
library(dplyr)
library(tidyr)
library(tibble)
library(patchwork)
library(ggmap)
library(sf)
library(readxl)
library(ggfx)
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

drive_auth(email = 'mbeck@tbep.org')
gs4_auth(token = drive_token())

font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
fml <- "roboto"

showtext_auto()
showtext_opts(dpi = 300)

knitr::opts_chunk$set(message = F, echo = F, warning = F, fig.align = 'center')

source(here('R/funcs.R'))


