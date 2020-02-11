# Remove all objects stored in the workspace
rm(list=ls())

# Generate nobs values from a MA(2)
set.seed(0)
nobs=500
mod.list=list(ma=c(.1,-0.6))
sd.eps=sqrt(400)
mu=100
ma.sim=arima.sim(model=mod.list, n=nobs, sd=sd.eps) + mu

##### Answers
### (a)
plot(ma.sim)

### (b)
ma.sim.acf=ARMAacf(ma=c(.1,-0.6), lag.max=10)
print(ma.sim.acf)
plot(ma.sim.acf, type='h', ylim=c(-1,1))
abline(h=0)

### (c)
opar=par(mfrow=c(2,1))
acf(ma.sim, lag.max=10)
pacf(ma.sim, lag.max=10)
par(opar)

### (d)
x=window(ma.sim, start=1, end=nobs-5) # select the first nobs-5 observations
xf=window(ma.sim, start=nobs-4, end=nobs)  # select the last 5 observations
fit0=arima(x, order=c(0,0,2), method="ML")
print(fit0)

### (e)
tsdiag(fit0)

### (f)
fit0.pred=predict(object=fit0, n.ahead = 5)
require(forecast)
acc.fc.fit0=accuracy(fit0.pred$pred, x=xf)
pred.int.wd=1.96*fit0.pred$se
ts.all=ts.union(xf,
                fit0.pred$pred,
                fit0.pred$pred+pred.int.wd,
                fit0.pred$pred-pred.int.wd)
plot(ts.all,
     plot.type='single',
     col=c('black', 'blue', 'red', 'red'))
legend('topleft', lty=c(1,1,1), 
       col=c('black', 'blue', 'red', 'red'),
       legend=c('observed','predicted','95% upper','95% lower'))