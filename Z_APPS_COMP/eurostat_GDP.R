library(eurostat)
library(tidyverse)
GDP_PPS <- get_eurostat("nama_10r_2gdp") 
GDP_PPS_2018 <- GDP_PPS %>% filter(time == "2018-01-01")
PPS_HAB_2018 <- GDP_PPS_2018 %>% filter(unit == "PPS_HAB_EU27_2020")
GISCO_map <- get_eurostat_geospatial(nuts_level = 2) 
GDP_PPS_2018.sf <- merge(GISCO_map, PPS_HAB_2018, by.x = "geo", by.y = "geo", all.x = TRUE) %>% 
  mutate(cat = cut_to_classes(values, n=10, decimals=0))
ggplot(GDP_PPS_2018.sf[GDP_PPS_2018.sf$cat != "No data",], aes(fill=cat)) + scale_fill_brewer(palette = "RdYlBu", na.value = "grey50") + geom_sf() +
  xlim(c(-12,44)) + ylim(c(33,70)) +
  labs(title="Regional GDP, by NUTS 2 regions, 2018", subtitle="[PPS per inhabitant in percentage of the EU27 (from 2020) average]", 
       fill="PPS_HAB\n(% EU27_2020)")