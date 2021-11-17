### sdm https://jcoliver.github.io/learn-r/011-species-distribution-models.html

here::set_here("/Users/julianflowers/Dropbox/Mac (2)/Desktop/spatial_data")

path <- "/Users/julianflowers/Dropbox/Mac (2)/Desktop/spatial_data"
data_path <- "/Users/julianflowers/Dropbox/Mac (2)/Desktop/aru_work/data"

library(pacman)

p_load(sp, raster, maptools, dismo, rgdal, tidyverse, janitor)

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


data(wrld_simpl)

# Plot the base map
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")

# Add the points for individual observation
points(x = obs$lon, 
       y = obs$lat, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)
# And draw a little box around the graph
box()


obs.data <- obs[, c("lon", "lat")]

# Build species distribution model
bc.model <- bioclim(x = bioclim.data, p = obs.data)

# Predict presence from model
predict.presence <- dismo::predict(object = bc.model, 
                                   x = bioclim.data, 
                                   ext = geographic.extent)


plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")

# Add model probabilities
plot(predict.presence, add = TRUE)

# Redraw those country borders
plot(wrld_simpl, add = TRUE, border = "grey5")

# Add original observations
points(obs.data$lon, obs.data$lat, col = "olivedrab", pch = 20, cex = 0.75)
box()

## files

bil.files <- list.files(path = paste0(path, "/", "wc2-5"), 
                        pattern = "*.bil$", 
                        full.names = TRUE)

mask <- raster(bil.files[1])

set.seed(20210707)

# Randomly sample points (same number as our observed points)
background <- randomPoints(mask = mask,     # Provides resolution of sampling points
                           n = nrow(obs.data),      # Number of random points
                           ext = geographic.extent, # Spatially restricts sampling
                           extf = 1.25)  

plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95",
     main = "Presence and pseudo-absence points")

# Add the background points
points(background, col = "grey30", pch = 1, cex = 0.75)

# Add the observations
points(x = obs.data$lon, 
       y = obs.data$lat, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)

box()

## test train split 80:20 (5 kfold)
testing.group <- 1

# Create vector of group memberships
group.presence <- kfold(x = obs.data, k = 5) # kfold is in dismo package

table(group.presence)

presence.train <- obs.data[group.presence != testing.group, ]
presence.test <- obs.data[group.presence == testing.group, ]

# Repeat the process for pseudo-absence points
group.background <- kfold(x = background, k = 5)
background.train <- background[group.background != testing.group, ]
background.test <- background[group.background == testing.group, ]

# Build a model using training data
bc.model <- bioclim(x = bioclim.data, p = presence.train)

# Predict presence from model (same as previously, but with the update model)
predict.presence <- dismo::predict(object = bc.model, 
                                   x = bioclim.data, 
                                   ext = geographic.extent)

# Use testing data for model evaluation
bc.eval <- evaluate(p = presence.test,   # The presence testing data
                    a = background.test, # The absence testing data
                    model = bc.model,    # The model we are evaluating
                    x = bioclim.data)    # Climatic variables for use by model

# Determine minimum threshold for "presence"
bc.threshold <- threshold(x = bc.eval, stat = "spec_sens")

# Plot base map
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")

# Only plot areas where probability of occurrence is greater than the threshold
plot(predict.presence > bc.threshold, 
     add = TRUE, 
     legend = FALSE, 
     col = c(NA, "olivedrab"))

# And add those observations
points(x = obs.data$lon, 
       y = obs.data$lat, 
       col = "black",
       pch = "+", 
       cex = 0.75)

# Redraw those country borders
plot(wrld_simpl, add = TRUE, border = "grey5")
box()


######### forecast
forecast.data <- getData(name = "CMIP5", # forecast data
                         var = "bio",    # bioclim
                         res = 2.5,      # 2.5 minute resolution
                         path = path, # destination directory
                         model = "GD",   # GFDL-ESM2G
                         rcp = "45",     # CO2 increase 4.5
                         year = 70)

names(forecast.data) <- names(bioclim.data)

forecast.presence <- dismo::predict(object = bc.model, 
                                    x = forecast.data, 
                                    ext = geographic.extent)

plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")

# Add model probabilities
plot(forecast.presence, add = TRUE)

# Redraw those country borders
plot(wrld_simpl, add = TRUE, border = "grey5")

# Add original observations
points(obs.data$lon, obs.data$lat, col = "olivedrab", pch = 20, cex = 0.75)
box()


# Plot base map
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")

# Only plot areas where probability of occurrence is greater than the threshold
plot(forecast.presence > bc.threshold, 
     add = TRUE, 
     legend = FALSE, 
     col = c(NA, "olivedrab"))

# And add those observations
points(x = obs.data$lon, 
       y = obs.data$lat, 
       col = "black",
       pch = "+", 
       cex = 0.6)

# Redraw those country borders
plot(wrld_simpl, add = TRUE, border = "grey5")
box()
