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

loadfonts(device = 'win', quiet = T)

fml <- "Lato"

knitr::opts_chunk$set(message = F, echo = F, warning = F, fig.align = 'center')
