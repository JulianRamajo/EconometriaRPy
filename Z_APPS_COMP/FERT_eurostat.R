library(eurostat)
library(tidyverse)
library(ggplot2)
fertility <- get_eurostat("tgs00100") 
fertility_2018 <-  fertility %>% filter(time == "2018-01-01") %>%  mutate(cat = cut_to_classes(values, n=7, decimals=1))
mapdata <- get_eurostat_geospatial(nuts_level = 2) %>% right_join(fertility_2018)
ggplot(mapdata, aes(fill=cat)) + scale_fill_brewer(palette = "RdYlBu") + 
  geom_sf(color=alpha("white",1/3), alpha = .6) +
  xlim(c(-12,44)) + ylim(c(33,70)) +
  labs(title="Fertility rate, by NUTS-2 regions, 2018", subtitle="Avg. number of live births per woman", fill="Total fertility rate(%)") +
  theme_light()