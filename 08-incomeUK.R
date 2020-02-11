# Remove all objects stored in the workspace
rm(list=ls())

# Load the package Ecdat
require(Ecdat)
data(IncomeUK)

lcons = log(IncomeUK[,2])
# (a) plot the three time series
plot(lcons)

# (b) plot the estimated ACF's and PACF's

acf(lcons, 
    ylab="ACF",
    main="Consumption")
  pacf(lcons, 
       ylab="PACF",
       main="consumption")

# (d)
require(forecast)
fit0=auto.arima(lcons, 
                test="adf",
                seasonal.test = "hegy",
                ic="aic", 
                stepwise = FALSE)
print(fit0)

# (e)
fit1=auto.arima(lcons, 
                test="adf",
                seasonal.test = "hegy",
                ic="aic", 
                stepwise = FALSE)
print(fit1)

# (f)
tsdiag(fit1)

# (h)
pred.r=predict(fit1, n.ahead=8)
pred.wd=1.96*pred.r$se
ts.all=ts.union(lcons,
                pred.r$pred,
                pred.r$pred+pred.wd,
                pred.r$pred-pred.wd)
plot(ts.all,
     plot.type='single',
     col=c('black', 'blue', 'red', 'red'))
legend('topleft', lty=c(1,1,1), 
       col=c('black', 'blue', 'red', 'red'),
       legend=c('observed','predicted','95% upper','95% lower'))
