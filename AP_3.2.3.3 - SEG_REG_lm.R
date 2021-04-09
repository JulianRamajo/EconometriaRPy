# Piecewise regression is a great method to model and capture changes in a series slope, by using knots to create different segments. 
# The plot below demonstrates the usage of piecewise regression to capture the structural changes in the US total construction spending 
# series (orange dotted line). Added a linear trend (red dashed line) for comparison.

library(readr)
library(plotly)
library(dplyr)

# Lectura de datos: y = Total Construction Spending (TTLCON) [ https://fred.stlouisfed.org/series/TTLCON) ]
df <- read_csv("TTLCON_USA.csv") %>% setNames(c("date","y"))

head(df)
tail(df)
class(df)
str(df)

# Gráfica de los datos
plot_ly(data = df) %>% add_lines(x = ~ date, y = ~ y)

# Construcción de tendencias
df <- df %>%
  mutate(seg1 = 1:nrow(df),
         seg2 = pmax(0, seg1 - min(seg1[seg1[which(date > as.Date("2007-08-01"))]])),
         seg3 = pmax(0, seg1 - min(seg1[seg1[which(date > as.Date("2011-01-01"))]])))

head(df)
tail(df)

# Modelo con tendencia simple
md1 <- lm(y ~ seg1, data = df)
summary(md1)
# Modelo con tendencias sementadas (con pivote conocido)
md2 <- lm(y ~ seg1 + seg2 + seg3, data = df)
summary(md2)

df$yhat1 <- predict(md1)
df$yhat2 <- predict(md2)

# Gráfica de los datos con las tendencias ajustadas por los dos modelos
plot_ly(data = df) %>%
  add_markers(x = ~ date,
              y = ~ y,
              marker = list(
                opacity = 0.6,
                color = "#90e0ef",
                size = 8),
              name = "Gasto en construcción") %>%
add_lines(x = ~ date,
          y = ~ yhat1,
          line = list(color = "red",
                      dash = "dash",
                      width = 4),
          name = "Tendencia simple") %>%
  add_lines(x = ~ date,
            y = ~ yhat2,
            line = list(color = "#fca311",
                        dash = "dot",
                        width = 6),
            name = "Tendencia segmentada") %>%
  layout(title = "Gasto total en construcción en USA",
         font = list(color = "black"),
         yaxis = list(title = "Millones de dólares"),
         xaxis = list(title = "Fuente: U.S. Census Bureau, Total Construction Spending, extraído de FRED (fred.stlouisfed.org)"),
         margin = list(t = 50, b = 80),
         legend = list(x = 0.05, y = 0.95))
#