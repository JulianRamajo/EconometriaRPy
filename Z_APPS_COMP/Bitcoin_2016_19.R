BTCUSD <- readRDS("Bitcoin_2016_2019")
plot(BTCUSD)
X <- (diff(log(bitcoin))[-1]) * 100 # log-returns (en %)
plot(X, type = "h")
