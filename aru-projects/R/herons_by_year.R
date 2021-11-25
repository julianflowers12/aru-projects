## egrets
library(tidyverse); library(broom); library(modelr); library(raster); library(sf)

england <- st_read(glue::glue(here::here("data/Download_1841450/gb-outlines_4219078"), "/england.shp"))
england <- sf::st_transform(england, 4326)
path <- here::here("assignments/egrets")

df <- read_csv(glue::glue(path, "/multiple taxa.csv")) %>%
  janitor::clean_names()
species <- df %>%
  dplyr::select(common_name, latitude = latitude_wgs84, 
         longitude = longitude_wgs84, start_date_year) %>%
  mutate(decade = start_date_year - start_date_year%%10)

path1 <- path <- here::here("ukbioclim")
files <- paste0(path, "/",  list.files(path, "bil"))

n <- ceiling(max(species$latitude, na.rm = TRUE))
s <- floor(min(species$latitude, na.rm = TRUE))
e <- ceiling(max(species$longitude, na.rm = TRUE))
w <- floor(min(species$longitude, na.rm = TRUE))

extent <- extent(w, e, s, n)


bio1crop <- crop(raster(files[2]), england)

plot(bio1crop)


df <- df %>%
  janitor::clean_names()

df <- df %>%
  dplyr::select(scientific_name, common_name, year = start_date_year, lat = latitude_wgs84, lon = longitude_wgs84,
         error = coordinate_uncertainty_m)
options(digits = 3)

df_5 <- df %>%
  filter(year >= 1970, year < 2020,
         !is.na(common_name)) %>%
  mutate(year_5 = cut(year, breaks = 10))

## 95th centile of latitude by year
df_5 %>%
  group_by(year, common_name) %>%
  summarise(decile = quantile(lat, probs = c(0.1, 0.5, 0.95), na.rm = TRUE, names = TRUE)) %>%
  mutate(q = c("0.1", "0.5", "0.95")) %>%
  pivot_wider(names_from = "q", values_from = "decile") %>%
  ggplot() +
  geom_point(aes(year, `0.95`, colour = common_name)) + 
  geom_smooth(aes(year, `0.95`, colour = common_name), se = FALSE, method = "lm") +
  facet_wrap(~ common_name, ncol = 2)
 
## nest data
nest_df <- df %>%
  group_by(common_name) %>%
  nest()

## mode fn
model <- function(df){
  
  lm(year ~ lon, data = df)
  
}

nest_df$data[[2]] %>%
  filter(year >= 2000) %>%
  ggplot(aes(year, lat)) +
  geom_jitter() +
  geom_smooth(method = "gam")
  lm(data = ., year ~ lat) %>%
  glance()

## run model
nest_df1 <- nest_df %>%
  mutate(mod = map(data, model), 
         glance = map(mod, broom::glance), 
         tidy = map(mod, broom::tidy)) %>%
  unnest("glance") 

nest_df1
View(N)
nest_df %>%
  mutate(resids = map2(data, model, add_predictions))

nest_df$data
  
other <- df %>%
  filter(year >= 1970, year < 2020,
         !is.na(common_name), 
         common_name != "Little Egret") %>%
  count(common_name, year) 

other %>% 
  ggplot(aes(year, n, colour = common_name)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE, option = "mako", begin = .2, end = .8) +
  theme_minimal()

little_egret <- df %>%
  filter(year >= 1970, year < 2020,
         !is.na(common_name), 
         common_name == "Little Egret") %>%
  count(common_name, year) 

little_egret %>% 
  ggplot(aes(year, n, colour = common_name)) +
  geom_line() + 
  geom_smooth(se = FALSE, method = "gam", lty = "dotted") +
  scale_color_viridis(discrete = TRUE) 

mean_lon <- df %>%
  filter(year >= 1970, year < 2020,
         !is.na(common_name), 
         common_name != "Little Egret") %>%
  group_by(common_name, year) %>%
  summarise(mean_lon = median(lat, na.rm = TRUE), 
            n = n()) 
  
mean_lon %>%
  ggplot(aes(year, mean_lon, colour = common_name)) +
  geom_point(aes(size = n)) +
  #geom_line() +
  geom_smooth(se = FALSE) +
  facet_wrap(~ common_name)

df %>%
  filter(year >= 1970, year < 2020,
         !is.na(common_name)) %>%
  ggplot() +
  geom_violin(aes(common_name, lat, fill = factor(year)), show.legend = FALSE) +
  coord_flip()
           

