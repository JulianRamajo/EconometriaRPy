#
library(tidyverse)
library(giscoR)
library(sf)
library(eurostat)
library(cartography)

# Regions
nuts2 <- gisco_get_nuts(year = "2016", epsg = "3035", resolution = "10", nuts_level = "2")

# Countries
countries <- gisco_get_countries(year = "2016", epsg = "3035", resolution = "10")

# Datos a representar
GDP <- get_eurostat("nama_10r_2gdp")
GDP_2018 <- GDP[GDP$time == "2018-01-01", ] 
PPS_HAB_2018 <- GDP_2018[GDP_2018$unit == "PPS_HAB_EU27_2020", ]

nuts2.sf <- merge(nuts2, PPS_HAB_2018, by.x = "NUTS_ID", by.y = "geo", all.x = TRUE)

# MAP

# Prepare mapping
br <- getBreaks(nuts2.sf$values, method = "quantile")
pal <-hcl.colors(n = (length(br) - 1), palette = "RdYlBu", alpha = 0.75)

# Plot
opar <- par(no.readonly = TRUE)
par(mar = c(2, 2, 2, 2))

plot(st_geometry(countries), col = "#E0E0E0", lwd = 0.1, bg = "#C6ECFF", 
     xlim = c(2300000, 7050000),ylim = c(1390000, 5400000))

choroLayer(nuts2.sf, var = "values", border = NA, breaks = br, col = pal, legend.pos = "n", colNA = "#E0E0E0", add = TRUE)

# Borders
plot(st_geometry(countries), lwd = 0.25, col = NA, add = TRUE)

# Legend
legendChoro( pos = "topright",
  title.txt = "PPS_HAB\n(% EU27_2020)",
  breaks = c("", format(round((br), 1), big.mark = ",")[-c(1, length(br))], ""),
  col = pal,
  nodata = T,
  nodata.txt = "n.d.",
  nodata.col = "#E0E0E0",
  frame = TRUE)
# Layers
layoutLayer(title = "Gross domestic product, Purchasing power standard (PPS) per inhabitant in percentage of the EU average, 2018",
            scale = 1000, col = pal[3], sources = gisco_attributions(), author = "")
#