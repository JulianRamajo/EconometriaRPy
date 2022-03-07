library(WDI)
WDIsearch("gdp.*capita.*PPP")
WDIsearch("CO2.*capita")
WDI_data <- WDI(indicator = c("NY.GDP.PCAP.PP.KD","EN.ATM.CO2E.PC"),start = 2000, end = 2016, extra = TRUE)
View(WDI_data)
#
library(tidyverse)
WDI_data <- WDI_data %>% filter(region != "Aggregates")
WDI_data <- WDI_data %>% rename(GDPpc = NY.GDP.PCAP.PP.KD, CO2pc = EN.ATM.CO2E.PC)
Kuznets_data <- WDI_data[,2:11]
Kuznets_data <- Kuznets_data[,-7]
# write_csv(Kuznets_data,"WDI_CO2pc_GDPpc.csv")
#
Kuznets_2016_data <- Kuznets_data %>% filter(year == "2016")
ggplot(Kuznets_2016_data, aes(x = GDPpc)) + geom_histogram()
ggplot(Kuznets_2016_data, aes(y = GDPpc, x = region)) + geom_boxplot() + coord_flip() + scale_y_log10()
gg1 <- ggplot(Kuznets_2016_data,aes(x=GDPpc, y=CO2pc)) + geom_point()
gg1
gg2 <- gg1 + geom_smooth(se = FALSE) + scale_x_log10() + scale_y_log10()
gg2
#
Kuznets_2016_curve <- lm(log(CO2pc) ~ log(GDPpc) + I(log(GDPpc)^2) , data=Kuznets_2016_data)
summary(Kuznets_2016_curve)
#
library(maps)
map('world')
map_data <- map_data("world")
dim(map_data)
class(map_data)
head(map_data)
ggplot(map_data, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", colour = "black")
library(countrycode)
map_data$ccode <- countrycode(map_data$region, origin = "country.name", destination = "wb")
Kuznets_2016_data$ccode <- countrycode(Kuznets_2016_data$country, origin = "country.name", destination = "wb")
merged_data <- full_join(map_data, Kuznets_2016_data, by = "ccode")
ggplot(merged_data, aes(x = long, y = lat, group = group, fill = log10(GDPpc))) + geom_polygon()
ggplot(merged_data, aes(x = long, y = lat, group = group, fill = log10(CO2pc))) + geom_polygon()
ggplot(merged_data, aes(x = long, y = lat, group = group, fill = log10(GDPpc))) + geom_polygon() + scale_fill_gradient(low = "green", high = "red")
ggplot(merged_data, aes(x = long, y = lat, group = group, fill = log10(CO2pc))) + geom_polygon() + scale_fill_gradient(low = "green", high = "red")
#