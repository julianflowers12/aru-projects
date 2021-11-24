## sdm

library(pacman)
p_load(adehabitatHR, sp, sf, rgdal, tidyverse, stars, sdm, usdm, janitor, raster, mapview, here)

installAll()

here()

path <- "D:/Assignment data"

## raster files
rasters <- "C:/Users/Julian/OneDrive/Desktop/aru_work/assignments/Poecile montanus/rasters"
rast_files <- list.files(rasters, pattern = "tif", full.names = T)


shapefiles <- "C:/Users/Julian/OneDrive/Desktop/aru_work/assignments/Poecile montanus/shp"
shapefiles <- list.files(shapefiles, pattern = "shp", full.names = T)
shp_files <- shapefile(shapefiles[[8]])
st_transform(shp_files, crs = 27700)

blue_tit_2019 <- raster(rast_files[[4]])
plot(blue_tit_2019)

plot(raster(rast_files[[22]]))

rast_read <- map(rast_files[4:28], raster)
rast_read <- rast_read[-c(2, 12, 17:18)]
rast_res <- map(rast_read, res)

rast_ext <- map(rast_read, extent)
rast_ext


## change extent of gsw raster
rast_read[[15]] <- setExtent(rast_read[[15]], extent(rast_read[[1]]), keepres = TRUE)
rast_read

plot(rast_read[[12]])


## keep land cover and species rasters
s <- stack(rast_read[c(1, 15, 16, 21)])
names(s)

plot(s, col = viridis::rocket(20, direction = -1))


