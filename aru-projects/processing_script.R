## sdm

library(pacman)
p_load(adehabitatHR, sp, sf, rgdal, tidyverse, stars, sdm, usdm, janitor, raster, mapview, here, ggthemes,ggspatial)

installAll()

here()

path <- "D:/Assignment data"

## files
rasters <- here("rasters")
shp <- here("shp")
data <- here("data")
images <- here("images")


## list files

rast_files <- list.files(rasters, pattern = "tif", full.names = T)
shape_files <- list.files(shp, pattern = "shp", full.names = T)
data_files <- list.files(shp, pattern = "csv", full.names = T)



## import shape files and transform to epsg:27700
shps <- map(shape_files[c(1, 4, 5, 8, 9, 10, 13, 16)], ~shapefile(.x)) ## read into R (could use st_read)
shp_sf <- map(shps, st_as_sf)  ## turn into simple feature (see above)
shp_tx <- map(shp_sf, st_transform, 27700)  ## transform to epsg:27700

(map(shp_tx, st_crs))

## seelct wt shapefiles

shp_tx_2010 <- shp_tx[[8]]

plot(shp_tx_2010)

## Check raster files

rast_read <- map(rast_files[4:28], raster)  # # read in files
rast_read <- rast_read[-c(2, 12, 17:18)] # select
rast_res <- map(rast_read, res) ## check resolution - all equal

rast_ext <- map(rast_read, extent) ## check extent
rast_ext ## variable


## change extent of gsw raster
rast_read[[15]] <- setExtent(rast_read[[15]], extent(rast_read[[1]]), keepres = TRUE)
extent(rast_read[[15]]) = extent(rast_read[[1]])

plot(rast_read[[12]])


## keep land cover and species rasters and stack
s <- stack(rast_read[c(1, 15, 16, 21)])
names(s)  ## now have blue tit, great spotted woodpecker and landcover rasters to same extent and resolution

plot(s, col = viridis::rocket(20, direction = -1))  # plot to check

## crop to data extent

sc <- crop(s, extent(wt1970))
plot(sc)
