library(here)
library(rmarkdown)

# pull data ---------------------------------------------------------------

source(here('R/dat_proc.R'))

# knit html ---------------------------------------------------------------

fls <- list.files(here('docs/'), '\\.Rmd$', full.names = T)
fls <- grep('salinity\\-trends\\.Rmd$', fls, value = T, invert = T)

for(fl in fls){
  cat(fl, '\n')
  render(fl, quiet = T)
}
  