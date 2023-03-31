library(gtrendsR)
library(reshape2)
library(ggplot2)
#
trend_1 <- gtrends(c("R statistics", "Python statistics"))
plot(trend_1)
#
trend_2 <- gtrends(c("R econometrics", "Python econometrics"))
plot(trend_2)
#
trend_3 <- gtrends(c("R data science", "Python data science"))
plot(trend_3)
#
trend_4 <- gtrends(c("R data analysis", "Python data analysis"))
plot(trend_4)
#
trend_5 <- gtrends(c("R machine learning", "Python machine learning"))
plot(trend_5)
#
# Para búsquedas directas en la página web de Google Trends usar el link:
# https://trends.google.com/trends/explore?date=all&q=r%20statistics,python%20statistics
# NOTA: Cambiar "statistics" por "econometrics", "data%20science" , ...