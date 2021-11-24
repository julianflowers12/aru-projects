## sdm

### crop everything to GB outline


library(pacman)
p_load(adehabitatHR, sp, sf, caret, mgcViz, rgdal, gt, tidyverse, stars, sdm, usdm, geojsonio, janitor, raster, mapview, here, ggthemes,ggspatial)

installAll()

here()

#path <- "D:/Assignment data"

## files
rasters <- here("aru-projects/rasters/")
shp <- here("aru-projects/shp/")
data <- here("aru-projects/data/")
images <- here("aru-projects/images/")

## scripts

## gb boundaries

gb <- st_read("https://opendata.arcgis.com/datasets/f2c2211ff185418484566b2b7a5e1300_3.geojson")
class(gb)


## list files

rast_files <- list.files(rasters, pattern = "tif", full.names = T)
shape_files <- list.files(shp, pattern = "shp", full.names = T)
data_files <- list.files(data, pattern = "csv", full.names = T)

## import data

data <- map(data_files, data.table::fread)   ## data.table `fread` for speed

## import shape files and transform to epsg:27700
shps <- map(shape_files[c(1, 4, 5, 6, 11, 9, 10, 14, 17)], ~st_read(.x)) ## read into R (could use st_read)
shp_sf <- map(shps, st_as_sf)  ## turn into simple feature (see above)
shp_tx <- map(shp_sf, st_transform, 27700)  ## transform to epsg:27700

(map(shp_tx, st_crs))

## select wt shapefiles

shp_tx

shp_tx_2010 <- shp_tx[[9]]

plot(shp_tx_2010)



## Check raster files

rast_read <- map(rast_files[4:28], raster)  # # read in files
rast_read <- rast_read[-c(2, 12, 17:18)] # select
rast_res <- map(rast_read, res) ## check resolution - all equal

rast_ext <- map(rast_read[c(1, 16, 18, 21)], extent) ## check extent
rast_ext ## variable

rast_analysis <- rast_read[c(1, 16, 18, 21)]
names(rast_analysis)

rast_analysis[[2]] <- setExtent(rast_analysis[[2]], extent(rast_analysis[[1]]), keepres = T)
rast_analysis[[3]] <- setExtent(rast_analysis[[3]], extent(rast_analysis[[1]]), keepres = T)
rast_analysis[[4]] <- setExtent(rast_analysis[[4]], extent(rast_analysis[[1]]), keepres = T)

map(rast_analysis, extent)

# ## change extent of gsw raster
# rast_read[[16]] <- setExtent(rast_read[[16]], extent(rast_read[[1]]), keepres = TRUE)
# extent(rast_read[[16]]) = extent(rast_read[[1]])
# 
# plot(rast_read[[16]])


## keep land cover and species rasters and stack
s <- stack(rast_analysis)
names(s)  ## now have blue tit, great spotted woodpecker and landcover rasters to same extent and resolution

plot(s, col = viridis::rocket(20, direction = -1))  # plot to check

## crop to data extent

sc <- crop(s, extent(shp_tx_2010))
plot(sc)


### check colinearity


v <- usdm::vifstep(sc) ## retained variables.


### get values

values <- values(sc)
head(values)
values_df <- data.frame(values) %>%
  set_names(c("blue_tit", "gsw", "lc90", "lc15")) %>%
  mutate(id = row_number())

##### sdm 

species <- shp_tx_2010 %>% 
  dplyr::select(geometry) %>%
  mutate(species = 1) %>%
  as(., "Spatial")


plot(species)

d <- sdmData(species ~ b3 + gsw2 + lc1990 + lc2015, train = species, predictors = sc, bg = list(n = 4000, remove = T) )
saveRDS(d, "wt_sdm_data.rds")

df <- d@features %>%
  mutate(presence = ifelse(rID <= length(d@species$species@presence), 1, 0), 
         coords = d@info@coords)

names(df)

landcover <- df %>%
  group_by(presence) %>%
  count(lc1990) %>%
  filter(lc1990 !=0) %>%
  mutate(sum = sum(n), 
         prop= round(100 * n / sum, 2)) %>%
  gt::gt()

# df %>%
#   ggplot(aes(factor(presence), b3)) +
#   geom_boxplot()
# 
# df %>%
#   group_by(presence, lc2015) %>%
#   dplyr::select(-contains("coords")) %>%
#   group_by(presence) %>%
#   mutate(n = n(), 
#          tot_bt = sum(b3), 
#          tot_gsw = sum(gsw2), 
#          )
#   
#   %>%
#   prop.table()
#   ggplot(aes(factor(lc2015), factor(presence) , fill = n)) +
#   geom_tile()

### hectad counts

shp_decade <- map_df(shp_tx[4:8], data.frame) ### check this

wt_hectad <- shp_decade %>%
  janitor::clean_names() %>%
  mutate(decade = as.numeric(start_da_3) - as.numeric(start_da_3) %% 10) %>%
  count(decade, hectad = osgr_10km) %>%
  mutate(species = "Willow tit")

gsw <- data[1]

gsw_hectad <- gsw[[1]] %>%
  janitor::clean_names() %>%
  mutate(decade = as.numeric(start_date_year) - as.numeric(start_date_year) %% 10) %>%
  filter(between(decade, 1970, 2010)) %>%
  count(decade, hectad = osgr_10km) %>%
  mutate(species = "Great spotted woodpecker")

wt_hectad %>%
  bind_rows(gsw_hectad) %>%
  pivot_wider(names_from = "species", values_from = "n", values_fill = 0) %>%
  janitor::clean_names() %>%
  drop_na(decade) %>%
  ggplot(aes(great_spotted_woodpecker, willow_tit)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ decade, scales = "free")

### conventional modelling; glm, gam, ranger
library(broom)
df <- df %>%
  dplyr::select(-contains("ID")) %>%
  janitor::clean_names()
names(df)

glm <- glm(presence ~ ., data = df, family = binomial())

glance(glm)
tidy(glm)
bs <- "cr"


gam <- bam(presence ~ s(b3, bs = bs) + s(gsw2, bs = bs) + factor(lc1990) + factor(lc2015), 
           data = df, family = "binomial", discrete = TRUE, method="fREML")


coef(gam)
summary(gam)
gam.check(gam)
anova.gam(gam)
concurvity(gam)
vis.gam(gam,ticktype="detailed",color="heat",theta=-35)  
vis.gam(gam, view=c("b3","lc1990"),plot.type="contour",color="heat")

b <- getViz(gam)

print(plot(b, allTerms = T), pages = 1) 

plot.gam(gam, residuals = T, rug = T, pages = 1, shift = T)

qq(getViz(gam))

preds <- predict.gam(gam, df, type = "response") %>%
  bind_cols(df) %>%
  mutate(predicted = ifelse(`...1` > 0.5, 1, 0))



cf <- caret::confusionMatrix(table(preds$predicted, preds$presence))

cf$table

plot(gam, all.terms = T, pages = 1)




####
m <- sdm(species ~ b3 + gsw2 + lc1990 + lc2015, data = d, methods = c("ranger", "glm", "glmnet"), parallelSettings = list(ncore = 6, methods = "parallel"))

saveRDS(m, "m.rds")

m@models

predsrf <- predict(m, newdata = sc, w = 1, mean = T)
predsglmnet <- predict(m, newdata = sc, w = 3, mean = T)

plot(predsrf)

p1 <- ensemble(m, newdata=sc, w = 1 ,setting=list(method='weighted',stat='AUC'))

gui(m)


##### herons

egrets <- shp_tx[[2]] %>%
  janitor::clean_names() %>%
  filter(str_detect(identifica, "Accepted")) %>%
  dplyr::select(lon = longitude, lat = latitude, 
         year = start_da_3, osgr_10km, 
         xcoord, ycoord, geometry) 

egretssp <- st_transform(egrets, 27700)
# coordinates(egrets) <- ~lon+lat
# egretssp <- st_as_sf(egrets)
# egrets <- st_set_crs(egretssp, 27700)
# st_transform(egrets, "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs" )

egrets2019 <- egretssp %>%
  filter(year == 2019)

egrets2009 <- egretssp %>%
  filter(year == 2009)

egrets1999 <- egretssp %>%
  filter(year == 1999)

ggplot(egrets2019) +
  geom_sf()

ggplot(egrets2009) +
  geom_sf()

ggplot(egrets1999) +
  geom_sf()

egrets

lat_90 <- egrets %>%
  group_by(year) %>%
  filter(between(year, 1970, 2019)) %>%
  summarise(q = quantile(xcoord/1000, probs = 0.99), n= n(), 
            median = quantile(xcoord/1000, probs = 0.5)) 

lat_90 %>%
  ggplot(aes(year, q)) +
  geom_point(aes(size = n)) +
  geom_smooth(aes(colour = n), se = F, method = "gam") +
  scale_y_continuous(label = scales::comma)

mod <- gam(q ~ s(year), data = lat_90)
gam.check(mod)
summary(mod)
plot(mod, pages = 1, shift = T)
e <- getViz(mod)
plot(sm(e, 1)) +
  l_fitLine(colour = "red") +
  l_ciLine(mul = 5, colour = "blue", lty = 2) +
  l_points(shape = 16)
  
egrets %>%
  filter(between(year, 1990, 2019)) %>%
  ggplot(aes(factor(year), lat)) +
  geom_boxplot(outlier.shape = "")


             