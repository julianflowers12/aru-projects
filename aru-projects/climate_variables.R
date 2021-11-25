## climate variables

library(pacman)
p_load(dismo, stars, raster, sf, tidyverse, rgdal, here, cubelyr)
library(dismo); library(stars); library(raster)

current_climate <- raster::getData(name = 'worldclim', var = 'bio', res = 2.5)
future_climate <- raster::getData(name = 'CMIP5', var = 'bio', res = 2.5,
                                  rcp = 85, mod = 'AC', year = 50)

scripts <- here::here("C:/Users/Julian/OneDrive/Desktop/aru-projects/aru-projects/R")
script_files <- list.files(scripts, pattern =  "R", full.names = T)
source(script_files[[8]])

path <- here::here("wc2-5/")
cc_files <- list.files(path, pattern = "bil", full.names = TRUE)

path1 <- here::here("cmip5/2_5m")
fc_files <- list.files(path1, pattern = "tif", full.names = TRUE)

read_stars(cc_files[[3]]) %>%
  plot()

path <- file.path(system.file(package = "dismo"), "ex")
files <- list.files(path, pattern = "grd", full.names = TRUE)
files

predictors <- stack(files)
plot(predictors)

rgdal::summary(fc_files[[2]])

crs(fc_files[[2]])



s <- read_stars(cc_files[[2]])


path2 <- here::here("C:/Users/Julian/OneDrive/Desktop/aru_work/practicals/Practical/week9")

files1 <- list.files(path2, "tif", full.names = T)

d <- files1[[1]]

r <- raster(files1[[1]])


test <- rasterdfize(files1[1])



test


