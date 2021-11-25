## plotting script in ggplot 2


hectad_plots <- function(shp, crs = NULL, name, title = NULL, caption = NULL){

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

library(patchwork)
b <- hectad_plots(shp_tx[[1]], name = "Blue tit",
         title = "Blue tit, 2019",
         caption = "Source: BTO")

g <- hectad_plots(shp_tx[[2]], name = "Great spotted woodpecker",
                  title = "Great spotted woodpecker, 2019",
                  caption = "Source: BTO")



b <- b +
  geom_sf(data = shp_tx[[3]], alpha = 0.0, colour = "red", fill = )

g <- g +
  geom_sf(data = shp_tx[[3]], alpha = 0.0, colour = "red")

p <- b + g

q <- p + plot_annotation(
  title = "Willow tit home range compared with distribution of blue tit and great spotted woodpecker, 2019",
  subtitle = "Hectad counts"
)

ggsave("hectads.png", q)
