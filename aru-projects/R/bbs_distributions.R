## bbs data https://www.bto.org/our-science/data/what-data-are-available https://www.bto.org/sites/default/files/atlas_open_data_files.zip

library(pacman)
p_load(tidyverse, janitor, data.table)


path <- "D:/Assignment data/atlas_open_data_files"

data_files <- list.files(path, ".csv")


bbs <- data_files
bbs <- map(bbs, ~paste0(path, "/", .x))
bbs_data <- map(bbs, fread)
bbs_data <- map(bbs_data, clean_names)


bbs_herons <- bbs_data[[2]] %>%
  left_join(bbs_data[[5]]) %>%
  filter(str_detect(english_name, c("Egret|Ibis|Spoonbill")))

glimpse(bbs_herons)

northerly_bbs <- bbs_herons %>%
  filter(str_detect(grid, "^H|^N")) %>%
  arrange(english_name, grid, period)


bbs_herons_change <- bbs_data[[1]] %>%
  left_join(bbs_data[[5]]) %>%
  filter(str_detect(english_name, c("Egret|Ibis|Spoonbill")))

northerly_bbs_change <- bbs_herons_change %>%
  filter(str_detect(grid, "^H|^N")) %>%
  arrange(english_name, grid, interval)


### willow tit

bbs_willow_tit <- bbs_data[[2]] %>%
  left_join(bbs_data[[5]]) %>%
  filter(str_detect(english_name, c("Willow Tit")))


bbs_willow_tit_change <- bbs_data[[1]] %>%
  left_join(bbs_data[[5]]) %>%
  filter(str_detect(english_name, c("Willow Tit")))

bbs_willow_tit_summary <- bbs_willow_tit_change %>%
  group_by(interval, season) %>%
  summarise(loss = sum(n_tenkms_loss),
            gain = sum(n_tenkms_gain),
            stable = sum(n_tenkms_stable)
  )


bbs_willow_tit_change %>%
  filter(n_tenkms_loss == 1) %>%
  select(interval, season, grid) %>%
  count(interval, season, grid) %>%
  pivot_wider(names_from = c("interval", "season"), values_from = "n") %>%
  arrange(desc(grid)) %>%
  mutate(grid100 = str_sub(grid, 1, 2)) %>%
  add_count(grid100) %>%
  View()
