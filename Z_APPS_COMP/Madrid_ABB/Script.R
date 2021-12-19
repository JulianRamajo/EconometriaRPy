#Lectura de librerías
library(tidyverse)
library(car)
library(sf)
library(tufte)
library(knitr)
library(stringr)
library(rgdal)
library(ggmap)
library(GISTools)
library(gstat)
library(spdep)
library(spatialreg)
library(sp)
library(gridExtra)
library(tmap)
library(leaflet) 
library(maptools)
library(RColorBrewer) 
library(classInt)
library(viridis)
# Lectura de datos
madrid_abb <- st_read("Madrid_ABB.gpkg")
class(madrid_abb)
names(madrid_abb)
str(madrid_abb)
summary(madrid_abb)
#
# Análisis de la variable precio
#
summary(madrid_abb$precio)
# Histograma
hist <- qplot(data=madrid_abb,x=precio)
hist
#
hist <- qplot(data=madrid_abb,x=log_precio)
hist
#
base <- ggplot(madrid_abb, aes(x=log_precio))
hist <- base + geom_histogram(bins=50, aes(y=..density..))
kde1d <- hist + geom_density(fill="#FF6666", alpha=0.5, colour="#FF6666")
kde1d
# Distribución espacial de los precios
ggplot(madrid_abb) + geom_sf(aes(fill=precio))+ theme_bw()
# madrid_abb %>% ggplot(aes(color = precio)) + geom_sf() + scale_color_viridis_c() + theme_void()
# Recuento espacial de alojamientos
hexbin <- ggplot() + geom_hex(data=as.data.frame(st_coordinates(madrid_abb)), aes(x=X, y=Y)) + scale_fill_continuous(type = "viridis")
hexbin
# Densidad bidimensional
lon_lat <- st_transform(madrid_abb, crs = 4326) %>% st_coordinates() %>% as.data.frame()
qmplot(X, Y, data = lon_lat, geom="blank") +
stat_density2d_filled(data = lon_lat, aes(x = X, y = Y, alpha = ..level..), n = 15) +
  scale_color_viridis_c()
#
# Modelo básico
#
m1 <- lm(log_precio ~ capacidad + aseos + dormitorios + camas + factor(tipo_hab) + WiFi + cafetera + gimnasio + aparcamiento + km_el_retiro, data=madrid_abb)
summary(m1)
#
# Heterogeneidad espacial: modelo con variables ficticias por barrio (efectos fijos espaciales)
#
m2 <- lm(log_precio ~ barrio + capacidad + aseos + dormitorios + camas + factor(tipo_hab) + WiFi + cafetera + gimnasio + aparcamiento + km_el_retiro -1, data=madrid_abb)
summary(m2)
#
compareCoefs(m1,m2)
# Gráfico de los efectos fijos estimados
nei.names <- m2$coefficients %>% as.data.frame() %>% row.names() %>% str_replace("barrio", "")
nei.fes <- data.frame(coef = m2$coefficients, nei = nei.names, row.names = nei.names) %>% right_join(madrid_abb, by = c("nei" = "barrio"))
nei.fes %>% st_as_sf() %>% ggplot(aes(color = coef)) + geom_sf() + scale_color_viridis_c() + theme_void()
#
# Extensión adicional: regímenes espaciales (Modelo con variables ficticias por barrio, con interacción)
#
# m2plus <- lm(log_precio ~  0 + (capacidad + aseos + dormitorios + camas + factor(tipo_hab) + WiFi + cafetera + gimnasio + aparcamiento + km_el_retiro):(barrio), data=madrid_abb)
# summary(m2plus)
#
# Modelos espaciales
#
# Matrices de pesos espaciales
#
# Vecinos más próximos (knn)
coords <- st_coordinates(madrid_abb)
k25nn <- knearneigh(as.matrix(coords), k=25) 
k25nb <- knn2nb(k25nn)
w25nn <- nb2listw(neighbours=k25nb, style="W") # Matriz de pesos W estandarizada por filas
# Distancia Euclidiana máxima entre los centroides
# rn <- row.names(madrid_abb)
# k1nn <- knn2nb(knearneigh(coords, k=1))
# max.dist <- max(un list(nbdists(k1nn, coords)))
# k1nb <- dnearneigh(coords, d1=0, d2=max.dist, row.names=rn, longlat =T)
# dlist <- nbdists(k1nb,coords) 
# id2list <- lapply(dlist,function(x) 1/x^2)
# wid2 <- nb2listw(neighbours=k1nb, glist=id2list, style="W") # Matriz de pesos W estandarizada por filas
#
# Gráficos de vecinos
#
plot(st_geometry(madrid_abb)) 
plot(w25nn, coords, add=TRUE, col="red")
#
# Estadísticos de autocorrelación espacial
#
# Global
moran.test(madrid_abb$precio, listw=w25nn)
# moran.plot(madrid_abb$precio, listw=w50nn)
geary.test(madrid_abb$precio, listw=w25nn)
# Local (clústeres locales)
LocalI <- as.data.frame(localmoran(madrid_abb$precio, listw=w25nn))
str(LocalI)
LocalI_sf <- bind_cols(madrid_abb,LocalI)
plot(LocalI_sf["Z.Ii"])
#
# Modelo de Durbin: se añaden retardos en las variables exógenas
#
# Generando retardos espaciales de las variables explicativas
madrid_abb$w_capacidad <- lag.listw(w25nn, madrid_abb$capacidad)
madrid_abb$w_aseos <- lag.listw(w25nn, madrid_abb$aseos)
madrid_abb$w_dormitorios <- lag.listw(w25nn, madrid_abb$dormitorios)
madrid_abb$w_camas <- lag.listw(w25nn, madrid_abb$camas)
# Modelo con retardos de las explicativas incluidos
m2Durbin <- lm(log_precio ~ barrio + capacidad + aseos + dormitorios + camas + factor(tipo_hab) + WiFi + cafetera + gimnasio + aparcamiento + km_el_retiro + w_capacidad + w_aseos + w_dormitorios + w_camas -1, data=madrid_abb)
summary(m2Durbin)
#
# Especificaciones SLM y SEM (librería spatialreg)
#
form <- formula(log_precio ~ capacidad + aseos + dormitorios + camas + factor(tipo_hab) + WiFi + cafetera + gimnasio + aparcamiento + km_el_retiro)
#
# Modelo lineal (LM) estimado por MCO
#
model.LM <- lm(formula=form, data=madrid_abb)
summary(model.LM)
#
# Modelo con retardo espacial (SLM)
#
# Estimación S2SLS
model.SLM.STSLS <- stsls(formula=form, data=madrid_abb, listw=w25nn)
summary(model.SLM.STSLS)
# Estimación ML
# model.SLM.ML <- lagsarlm (formula=form, data=madrid_abb, listw=w25nn, method="eigen")
# summary(model.SLM.ML)
#
# Especificación MESS (Matrix Exponential Spatial Specification)
# model.SLM.MESS <- lagmess(formula=form, data=madrid_abb, listw=w50nn) 
# summary(model.SLM.MESS)
#
# Modelo con errores espaciales (SEM)
#
# Estimación GMM
model.SEM.GMM <- GMerrorsar(formula=form, data=madrid_abb, listw=w25nn)
summary(model.SEM.GMM)
# Estimación ML
# model.SEM.ML <- errorsarlm (formula=form, data=madrid_abb, listw=w50nn, method="eigen")
# summary(model.SEM.ML)
#
# Modelo combinado (SAC -> SLM + SEM)
#
# Estimación GS2SLS
model.SAC.GSTSLS <- gstsls(formula=form, data=madrid_abb, listw=w25nn)
summary(model.SAC.GSTSLS)
#
