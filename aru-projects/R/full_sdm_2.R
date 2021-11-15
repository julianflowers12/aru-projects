### sdm 2 https://rspatial.org/raster/sdm/2_sdm_occdata.html

here::set_here("/Users/julianflowers/Dropbox/Mac (2)/Desktop/spatial_data")

path <- "/Users/julianflowers/Dropbox/Mac (2)/Desktop/spatial_data"
data_path <- "/Users/julianflowers/Dropbox/Mac (2)/Desktop/aru_work/data"

library(pacman)

p_load(sp, raster, maptools, dismo, rgdal, tidyverse, janitor, rgeos)

bioclim.data <- getData(name = "worldclim",
                        var = "bio",
                        res = 2.5,
                        path = path)


obs <- read_csv(glue::glue(data_path, "/Poecile montanus.csv"), show_col_types = FALSE)

obs <- obs %>%
  janitor::clean_names() %>%
  filter(start_date_year == 2019) %>%
  select(scientific_name, lat = latitude_wgs84, lon = longitude_wgs84)

max.lat <- ceiling(max(obs$lat))
min.lat <- floor(min(obs$lat))
max.lon <- ceiling(max(obs$lon))
min.lon <- floor(min(obs$lon))
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

bil.files <- list.files(path = paste0(path, "/", "wc2-5"), 
                        pattern = "*.bil$", 
                        full.names = TRUE)

mask <- raster(bil.files[1])

bg <- randomPoints(mask = mask,     # Provides resolution of sampling points
                           n = nrow(obs),      # Number of random points
                           ext = geographic.extent, # Spatially restricts sampling
                           extf = 1.25) 


par(mfrow=c(1,2))
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")
points(bg, cex=0.5)
# now we repeat the sampling, but limit
# the area of sampling using a spatial extent
bg2 <- randomPoints(mask, 50, ext=geographic.extent)
plot(e, add=TRUE, col='red')
points(bg2, cex=0.5)

coordinates(obs) <- ~lon+lat
projection(obs) <- CRS('+proj=longlat +datum=WGS84')

x <- circles(obs, d=10000, lonlat=TRUE)
x@polygons
pol <- polygons(x)

# sample randomly from all circles
samp1 <- spsample(pol, 250, type='random', iter=25)
## Warning in proj4string(obj): CRS object has comment, which is lost in output
# get unique cells
cells <- cellFromXY(mask, samp1)
length(cells)
## [1] 250
cells <- unique(cells)
length(cells)
## [1] 161
xy <- xyFromCell(mask, cells)

plot(pol, axes=TRUE)
points(xy, cex=0.75, pch=20, col='blue')

spxy <- SpatialPoints(xy, proj4string=CRS('+proj=longlat +datum=WGS84'))
o <- over(spxy, geometry(x))
xyInside <- xy[!is.na(o), ]

# # extract cell numbers for the circles
# v <- extract(mask, x@polygons, cellnumbers=T)
# # use rbind to combine the elements in list v
# v <- do.call(rbind, v)
# # get unique cell numbers from which you could sample
# v <- unique(v[,1])
# head(v)
# ## [1] 15531 15717 17581 17582 17765 17767
# # to display the results
# m <- mask
# m[] <- NA
# m[v] <- 1
# plot(m, ext=extent(x@polygons)+1)
# plot(x@polygons, add=T)


predictors <- stack(bil.files)
plot(predictors)

library(maptools)
data(wrld_simpl)
# file <- paste(system.file(package="dismo"), "/ex/bradypus.csv", sep="")
# bradypus <- read.table(file,  header=TRUE,  sep=',')
# # we do not need the first column
# bradypus  <- bradypus[,-1]

obs.data

plot(predictors, 1,
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE)

points(obs.data, cex = .2)


## extract values
presence <- raster::extract(predictors, obs.data)

set.seed(0)
backgr <- randomPoints(predictors, nrow(obs.data))
absence <- raster::extract(predictors, backgr)
pb <- c(rep(1, nrow(presence)), rep(0, nrow(absence)))
sdmdata <- data.frame(cbind(pb, rbind(presence, absence)))
sdmdata <- sdmdata %>% na.omit()
dim(sdmdata)

pairs(sdmdata[, 2:5])

### save data

saveRDS(sdmdata, "sdmdata.rds")
saveRDS(presence, "presence.rds")



library(dismo); library(broom)
sdmdata <- readRDS("sdmdata.Rds")
dim(sdmdata)
presvals <- readRDS("presence.Rds")

names(sdmdata)

sdm_mod <- glm(pb ~ ., data = sdmdata, family = "binomial")
tidy(sdm_mod)
glance(sdm_mod)


set.seed(0)
group <- kfold(obs.data, 5)
pres_train <- obs.data[group != 1, ]
pres_test <- obs.data[group == 1, ]

set.seed(10)
backg <- randomPoints(predictors, n= 6440, ext=geographic.extent, extf = 1.25)

colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]

r <- raster(predictors, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE, xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE)
plot(geographic.extent, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-',  cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')


bc <- bioclim(predictors, pres_train)
bc@max
plot(bc, a=1, b=2, p=0.85)

e <- evaluate(pres_test, backg_test, bc, predictors)
e
