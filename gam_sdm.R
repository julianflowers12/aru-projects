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

paridae <- data.table::fread("Paridae 2010-2020.csv") %>%
  janitor::clean_names() %>% 
  select(lat = latitude_wgs84, lon = longitude_wgs84, 
         name = scientific_name, contains("osgr"), identification_verification_status, 
         contains("start"))

dendocopus <- data.table::fread("Dendrocopos.csv") %>%
  janitor::clean_names() %>% 
  select(lat = latitude_wgs84, lon = longitude_wgs84, 
         name = scientific_name, contains("osgr"), identification_verification_status, 
         contains("start"))

dendrocopos <- dendocopus[!is.na(start_date_month) & str_detect(identification_verification_status , "Accepted") & name %in% c("Dendrocopos major") & str_length(osgr_100km) == 2, .N, by = .(month = start_date_month, osgr_100km, name)]

q <- dendrocopos %>%
  ggplot(aes(factor(month), fct_rev(osgr_100km), fill = N)) +
  geom_tile() +
  facet_wrap(~name, nrow = 1) +
  viridis::scale_fill_viridis(option = "viridis", direction = -1)

q

head(paridae)

p <- paridae[name %in% c("Parus major", "Cyanistes caeruleus", "Poecile montanus", "Poecile palustris", "Periparus ater") & !is.na(start_date_month) & str_length(osgr_100km) == 2, .N, by = .(month = start_date_month, osgr_100km, name)] %>%
  ggplot(aes(factor(month), fct_rev(osgr_100km), fill = N)) +
  geom_tile() +
  facet_wrap(~name, nrow = 1) +
  viridis::scale_fill_viridis(option = "mako", direction = -1)

p

p1 <- paridae[name %in% c("Parus major", "Cyanistes caeruleus", "Poecile montanus", "Poecile palustris", "Periparus ater") & !is.na(start_date_month) & str_length(osgr_100km) == 2 & !str_detect(osgr_100km, "^I"), .N, by = .(year = start_date_year, osgr_100km, name)] %>%
  ggplot(aes(factor(year), fct_rev(osgr_100km), fill = log10(N))) +
  geom_tile() +
  facet_wrap(~name, nrow = 1) +
  viridis::scale_fill_viridis(option = "rocket", direction = -1) +
  theme_minimal()

p1 +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


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

plot(sm(o, 16)) +
  l_fitLine() +
  l_points() +
  l_ciLine()
  



