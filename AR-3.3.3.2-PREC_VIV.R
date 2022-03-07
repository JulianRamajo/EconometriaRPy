#
library(spData)
help(house, package="spData")
data(house, package="spData")
class(house)
str(house)
summary(house@data)
#
library(sf) 
library(leaflet) 
library(ggplot2)
library(maptools)
library(RColorBrewer) 
library(classInt)
library(rgdal)
library(viridis)
#
# Representación de datos espaciales
#
spplot(house, "price", col.regions = rev(magma(10)))
#
house_sf <- st_as_sf(house) # Convirtiendo objeto sp a sf
class(house_sf)
plot(house_sf[1])
ggplot(house_sf) + geom_sf(aes(fill=price))+ theme_bw()
#
# Indicadores de asociación espacial (global y local)
#
library(spdep) 
library(dplyr)
# Información sobre las coordenadas (longitud y latitud) de los datos
coords <- coordinates(house)
# Matrices de pesos espaciales
# Vecinos más próximos (k=6 mediana de vecinos en la base de datos)
house.6nn <- knearneigh(coords, k=6) 
house.6nn.nb <- knn2nb(house.6nn)
# Distancia Euclidiana máxima entre los centroides
# rn <- row.names(house_sf)
# house.1nn.nb <- knn2nb(knearneigh(coords, k=1))
# max.dist <- max(unlist(nbdists(house.1nn.nb, coords)))
# house.dist.nb <- dnearneigh(coords, d1=0, d2=max.dist, row.names=rn, longlat =T) 
# Datos para la matriz de distancias inversas al cuadrado posterior
# dlist <- nbdists(house.dist.nb,coords) 
# id2list <- lapply(dlist,function(x) 1/x^2)
#
# Gráficos de vecinos
#
plot(st_geometry(house_sf)) 
plot(house.6nn.nb, coords, add=TRUE, col="red")
#
# plot(st_geometry(house_sf)) 
# plot(house.dist.nb, coords, add=TRUE, col="red")
#
# Cálculo de las matrices de pesos W (estandarizadas por filas)
house.6nn.w <- nb2listw(neighbours=house.6nn.nb, style="W")
# house.id2.w <- nb2listw(neighbours=house.dist.nb,glist=id2list,style="W")
# house.d.w <- nb2listw(neighbours=house.dist.nb, style="W")
#
# Estadísticos de autocorrelación espacial
#
# Global
moran.test(house_sf$price, listw=house.6nn.w)
geary.test(house_sf$price, listw=house.6nn.w)
# Local
LocalI <- as.data.frame(localmoran(house_sf$price, listw=house.6nn.w))
str(LocalI)
moran.plot(house_sf$price, listw=house.6nn.w) 
# Clusters locales
house_LocalI_sf <- bind_cols(house_sf,LocalI) #
plot(house_LocalI_sf["Z.Ii"])
#
# Modelos econométricos espaciales
#
library(spatialreg)
form <- formula(log(price) ~ age + log(lotsize) + rooms)
#
# Modelo lineal (LM) estimado por MCO
#
model.LS <- lm(formula=form, data=house_sf)
summary(model.LS)
#
# Modelo con retardo espacial (SLM)
#
# Estimación S2SLS
model.SLM.STSLS <- stsls(formula=form, data=house_sf, listw=house.6nn.w)
summary(model.SLM.STSLS)
# Estimación ML
# model.SLM.ML <- lagsarlm (formula=form, data=house_sf, listw=house.6nn.w, method="eigen")
# summary(model.SLM.ML)
#
# Especificación MESS (Matrix Exponential Spatial Specification)
# model.SLM.MESS <- lagmess(formula=form, data=house_sf, listw=house.6nn.w) 
# summary(model.SLM.MESS)
#
# Modelo con errores espaciales (SEM)
#
# Estimación GMM
model.SEM.GMM <- GMerrorsar(formula=form, data=house_sf, listw=house.6nn.w)
summary(model.SEM.GMM)
# Estimación ML
# model.SEM.ML <- errorsarlm (formula=form, data=house_sf, listw=house.6nn.w, method="eigen")
# summary(model.SEM.ML)
#
# Modelo combinado (SAC -> SLM + SEM)
#
# Estimación GS2SLS
model.SAC.GSTSLS <- gstsls(formula=form, data=house_sf, listw=house.6nn.w)
summary(model.SAC.GSTSLS)
#