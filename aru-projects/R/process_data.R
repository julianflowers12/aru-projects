# import and process data

library(pacman)
p_load(tidyverse, janitor, data.table)


path <- "D:/Assignment data"

data_files <- list.files(path, ".csv")

## import

gis <- data_files[c(1, 3, 6)]
gis <- map(gis, ~paste0(path, "/", .x))

landscapes <- data_files[c(2)]
landscapes <- map(landscapes, ~paste0(path, "/", .x))


## read in
gis_data <- map(gis, fread)
gis_data <- map(gis_data, clean_names)

landscape_data <- map(landscapes, read_csv)
landscape_data <- map(landscape_data, clean_names)

create_decade <- function(df){

  df <- df %>%
    mutate(decade = start_date_year - start_date_year%%10)

  df

}

# add decade
gis_data <- map(gis_data[1:2], create_decade)
landscape_data <- map(landscape_data, create_decade)

## select fields
gis_data <- map(gis_data, ~select(.x, scientific_name, common_name,  start_date_year, decade, lon = longitude_wgs84,
                     lat = latitude_wgs84, contains("osgr"), state_province, coordinate_uncertainty_m))


landscape_data <- map(landscape_data, ~select(.x, scientific_name, common_name,  start_date_year, decade, lon = longitude_wgs84,
                                  lat = latitude_wgs84, contains("osgr"), state_province, coordinate_uncertainty_m))
## select fields
select_fields <- function(df){

  require(tidyverse)
  df <- create_decade(df) %>%
    select(scientific_name, common_name,  start_date_year, decade, lon = longitude_wgs84,
           lat = latitude_wgs84, contains("osgr"), state_province, error = coordinate_uncertainty_m)

  df

}


## woodpecker
woodpecker <- select_fields(gis_data[[1]])
paridae <- select_fields(gis_data[[2]])


trend_counts <- function(df){

  require(tidyverse)
  require(scales)


 df <- df %>%
    mutate(name = fct_lump(scientific_name, n = 1)) %>%
    group_by(decade, common_name, name) %>%
    filter(between(decade, 1960, 2010)) %>%
    na.omit() %>%
    count() %>%
    mutate(name = paste0(common_name, " ", "(", name, ")"))

 p <- df %>%
   filter(name != " (Other)") %>%
   ggplot() +
   geom_col(aes(decade, n)) +
   facet_wrap(~name, scales = "free") +
   scale_y_continuous(label = scales::comma) +
   theme_minimal() +
   theme(plot.title.position = "plot") +
   labs(title = paste("Decadal counts of", unique(df$name), "1960 - 2010"),
        y = "Observations",
        x = "Decade")


 out <- list(counts = df, plot = print(p))

}

x <- trend_counts(woodpecker)
unique(x$counts$name)

x$counts
x$plot

woodpecker %>%
  mutate(name = fct_lump(scientific_name, n = 1)) %>%
  group_by(decade, common_name, name) %>%
  filter(between(decade, 1960, 2010)) %>%
  na.omit() %>%
  count() %>%
  mutate(name = paste0(common_name, " ", "(", name, ")"))


## paridae

y <- trend_counts(paridae)

## herons

herons <- trend_counts(landscape_data[[1]])
