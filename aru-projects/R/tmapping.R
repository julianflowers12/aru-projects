## map

p_load(sf, ggmap, tmap)


path <- "D:/Data store/Download_1838024/open-greenspace_4212192"

data_files <- list.files(path, "shp")
data_files


greenspace <- st_read(paste0(path, "/", data_files[2]))

greenspace$function.%>%
  unique()


tm_shape(greenspace) +
  tm_fill("function.") +
  tmap_mode("view")
