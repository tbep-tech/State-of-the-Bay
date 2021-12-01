library(knitr)
library(tbeptools)
library(dplyr)
library(tidyr)
library(extrafont)
library(patchwork)
library(ggmap)
library(sf)
library(extrafont)
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

drive_deauth()
gs4_deauth()

loadfonts(device = 'win', quiet = T)
fml <- "Lato"

source(here('R/funcs.R'))

knitr::opts_chunk$set(message = F, echo = F, warning = F, fig.align = 'center')
