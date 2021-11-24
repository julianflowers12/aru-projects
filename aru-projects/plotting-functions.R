## plotting script in ggplot 2


my_plots <- function(shp, crs, name, title, caption){

  require(ggplot2)
  require(ggspatial)
  require(sf)

p <- ggplot(shp) +
  geom_sf(aes(fill = as.numeric(Records)), show.legend = T) +
  viridis::scale_fill_viridis(discrete = F, direction = -1, name = name) +
  theme_void() +
  ggspatial::annotation_north_arrow(style = north_arrow_fancy_orienteering()) +
  annotation_scale(bar_cols = c("red", "white")) +
  coord_sf(crs = crs) +
  labs(title = title,
       caption = caption)

p

}


my_plots(shp_tx[[2]], crs = 4326, name = "Great spotted woodpecker",
         title = "Hectad observation counts of great spotted woodpecker, 2019",
         caption = "Source: BTO")
