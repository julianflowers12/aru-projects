## select and plot nbn data


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
