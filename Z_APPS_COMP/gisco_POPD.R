#
library(giscoR)
library(sf)
library(eurostat)
library(cartography)

# Regions
nuts3 <- gisco_get_nuts(year = "2016", epsg = "3035", resolution = "10", nuts_level = "3")

# Countries
countries <- gisco_get_countries(year = "2016", epsg = "3035", resolution = "10")

# Datos a representar
popdens <- get_eurostat("demo_r_d3dens")
popdens <- popdens[popdens$time == "2018-01-01", ]

nuts3.sf <- merge(nuts3, popdens, by.x = "NUTS_ID", by.y = "geo", all.x = TRUE)

# Prepare mapping
br <- c(0, 25, 50, 100, 200, 500, 1000, 2500, 5000, 10000, 30000)
pal <- hcl.colors(n = (length(br) - 1), palette = "inferno", alpha = 0.7, rev = TRUE)

# Plot
opar <- par(no.readonly = TRUE)
par(mar = c(0, 0, 0, 0), bg = "#C6ECFF")

plot(st_geometry(countries), col = "#E0E0E0", lwd = 0.1, bg = "#C6ECFF", 
     xlim = c(2300000, 7050000),ylim = c(1390000, 5400000))

choroLayer(nuts3.sf, var = "values", border = NA, breaks = br, col = pal, legend.pos = "n", colNA = "#E0E0E0", add = TRUE)
# Borders
plot(st_geometry(countries), lwd = 0.25, col = NA, add = TRUE)

# Legend
legendChoro( pos = "topright",
  title.txt = "Population density (km2)\nNUTS3 (2018)",
  breaks = c("", format(br, big.mark = ",")[-c(1, length(br))], ""),
  col = pal,
  nodata = T,
  nodata.txt = "n.d.",
  nodata.col = "#E0E0E0",
  frame = TRUE)
# Layers
layoutLayer(scale = 1000, col = pal[3], sources = gisco_attributions(), author = "")
#