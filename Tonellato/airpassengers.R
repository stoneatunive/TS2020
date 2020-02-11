rm(list=ls())
data("AirPassengers")
plot(AirPassengers)
plot(log(AirPassengers))
lair=log(AirPassengers)

#first order differences
dy = diff(lair)
plot(dy)
acf(dy,lag.max=24)

#second order differences
dDy = diff(dy, lag=12)
plot(dDy)
acf(dDy, lag.max=24)
pacf(dDy, lag.max=24)

fit1 = arima(lair, order=c(0,1,1), method="ML", seasonal=list(order=c(0,1,1), period=12))

require(forecast)
fit0 = auto.arima(lair, 
                  max.p = 5, 
                  max.q = 5,
                  max.P = 2,
                  max.Q = 2,
                  test = "adf",
                  seasonal.test="hegy",
                  ic="bic")


