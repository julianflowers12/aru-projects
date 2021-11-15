## gam sdm

here::set_here("/Users/julianflowers/Dropbox/Mac (2)/Desktop/spatial_data")

path <- "/Users/julianflowers/Dropbox/Mac (2)/Desktop/spatial_data"
data_path <- "/Users/julianflowers/Dropbox/Mac (2)/Desktop/aru_work/data"

library(pacman)

p_load(sp, raster, maptools, dismo, rgdal, tidyverse, janitor, rgeos, mgcv, mgcViz)

obs <- read_csv(glue::glue(data_path, "/Poecile montanus.csv"), show_col_types = FALSE)

library(dismo); library(broom)
sdmdata <- readRDS("sdmdata.Rds")
dim(sdmdata)
presvals <- readRDS("presence.Rds")

## trial run

glimpse(sdmdata)

test_gam <- bam(pb ~ s(bio1) + s(bio2) + s(bio3) + s(bio4) +
                  s(bio5) + s(bio5) + s(bio6) + s(bio7) +
                  s(bio8) + s(bio9) + s(bio10) + s(bio11) +
                  s(bio12) + s(bio13) + s(bio14) + s(bio15) + s(bio16) +
                  s(bio17) + s(bio18) + s(bio19), data = sdmdata, family = "binomial", method = "fREML", 
                discrete = TRUE, nthreads = 6)

gam.check(test_gam)


preds <- predict.gam(test_gam, sdmdata, type = "response") %>%
  bind_cols(sdmdata) %>%
  mutate(predicted = ifelse(`...1` > 0.5, 1, 0))

table(preds$predicted, preds$pb)

o <- getViz(test_gam)

plot(sm(o, 17)) +
  l_fitLine() +
  l_points()



