# Remove all objects stored in the workspace
#rm(list=ls())

# Load the package Ecdat
# require(Ecdat)
# data(Tbrate)

Tbrate=ts(Tbrate, start=1950.00, end=1996.75, frequency=4)
# (a) plot the three time series
plot(Tbrate)

# (b) plot the estimated ACF's and PACF's
opar=par(mfrow=c(3,2))
for(j in 1:3){
  acf(Tbrate[,j], 
      lag.max=20,
      ylab="ACF",
      main=colnames(Tbrate)[j])
  pacf(Tbrate[,j], 
       lag.max=20,
       ylab="PACF",
       main=colnames(Tbrate)[j])
}
par(opar)

# (c)
require(urca)
df.r = ur.df(Tbrate[,"r"], type="none")
df.y = ur.df(Tbrate[,"y"], type="none")
df.pi = ur.df(Tbrate[,"pi"], type="none")
print(summary(df.r))
print(summary(df.y))
print(summary(df.pi))

# (d)
require(forecast)
tbrate=Tbrate[,"r"]
fit0=auto.arima(tbrate, 
                test="adf", 
                ic="aic", 
                stepwise = FALSE)
print(fit0)

# (e)
fit1=auto.arima(tbrate, 
                test="adf", 
                ic="aic", 
                stepwise = FALSE)
print(fit1)

# (f)
tsdiag(fit1)

# (h)
pred.r=predict(fit1, n.ahead=8)
pred.wd=1.96*pred.r$se
ts.all=ts.union(tbrate,
                pred.r$pred,
                pred.r$pred+pred.wd,
                pred.r$pred-pred.wd)
plot(ts.all,
     plot.type='single',
     col=c('black', 'blue', 'red', 'red'))
legend('topleft', lty=c(1,1,1), 
       col=c('black', 'blue', 'red', 'red'),
       legend=c('observed','predicted','95% upper','95% lower'))