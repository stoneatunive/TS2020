
getwd()
setwd("D:/sharefile/Personal Folders/teaching2018/prog-exercises")

library(tseries)
library(forecast)
#simulatd process ma(1)
x <- rnorm(100)
wn <- ts(x, start=2000,frequency=12)
tsdisplay(wn)
mean(wn)
var(wn)

head(wn,10)
tail(wn)
aa<-lag(wn,1)
head(aa)
ma1 <- wn+(0.9*lag(wn,1))
head(ma1,10)
plot(ma1)
tsdisplay(ma1)
lag.plot(ma1,12)
mean(ma1)
var(ma1)



#use of arima.sim to simulate MA(q)

ts.sim <- arima.sim(list(order = c(0,0,1), ma= c(0.9)), n = 100)
tsdisplay(ts.sim)
mean(ts.sim)
var(ts.sim)
ts.sim2 <- arima.sim(list(order = c(0,0,1), ma= c(0.8)), n = 200)
mean(ts.sim2)
var(ts.sim2)

tsdisplay(ts.sim2)
ggmonthplot(ts.sim2)
ggseasonplot(ts.sim2)
lag.plot(ts.sim2,12)

ts.sim2 <- arima.sim(list(order = c(0,1,0)), n = 100)



#ex 1
ts.sim1 <- arima.sim(list(order = c(0,0,2), ma= c(0.3,0.8)), n = 200)
tsdisplay(ts.sim1)
ts.sim4 <- arima.sim(list(order = c(0,0,3), ma= c(0.5,0.8,0.6)), n = 100)
tsdisplay(ts.sim4)

#ex 2
ts.sim4a <- arima.sim(list(order = c(1,0,0), ar=c(-0.7)), n = 200)
tsdisplay(ts.sim4a)

ts.sim4a1 <- arima.sim(list(order = c(1,0,0), ar=c(0.9)), n = 100)
tsdisplay(ts.sim4a1)
lag.plot(ts.sim4a1,12)

ts.sim4b <- arima.sim(list(order = c(2,0,0), ar=c(0.6,0.3)), n = 100)
tsdisplay(ts.sim4b)

ts.sim4c <- arima.sim(list(order = c(3,0,0), ar=c(0.2,0.4,0.3)), n = 100)
tsdisplay(ts.sim4c)


#ex 3
ts.sim5 <- arima.sim(list(order = c(1,0,1), ar=c(0.8), ma= c(0.3)), n = 1000)
tsdisplay(ts.sim5)


#ex 4
ts.sim6 <- arima.sim(list(order = c(1,0,1), ar=c(0.3), ma= c(0.8)), n = 1000)
tsdisplay(ts.sim6)

ts.sim7 <- arima.sim(list(order = c(1,0,1), ar=c(0.8), ma= c(0.3)), n = 1000)
tsdisplay(ts.sim7)
estimate<-arima(ts.sim7,order=c(2,0,0), include.mean = FALSE)
estimate$aic
BIC(estimate)
summary(estimate)
tsdisplay(estimate$residuals)

ts.sim8 <- arima.sim(list(order = c(1,0,0), ar=c(0.6)), n = 100)
tsdisplay(ts.sim8)



#estimate 
estimate<-arima(ts.sim8,order=c(3,0,0), include.mean = FALSE)

estimate1<-arima(ts.sim8,order=c(3,0,0),fixed=c(0,0,NA,0))
names(estimate)
summary(estimate)
estimate$coef
estimate$var.coef
tsdisplay(estimate$residuals)
plot(ARMAacf(ts.sim8))
#tsdiag(estimate)
#forecast
#accuracy
#ts.union


#Ljung-Box test
x <- rnorm(100)
wn <- ts(x, start=2000,frequency=12)
t<-Box.test(wn,lag=12,type='Ljung')
names(t)
t$p.value
Box.test(estimate$residuals,lag=12,type='Ljung')



#how to save aic bic
aaic<-matrix(data=0, ncol=4, nrow=4);
aaic
for (i in 0:3){for (j in 0:3) {
  estimate<-arima(ts.sim8,order=c(i,0,j), include.mean = FALSE)
  aaic[i+1,j+1]<-estimate$aic
}} 
aaic

abic<-matrix(data=0, ncol=4, nrow=4);
for (i in 0:3){for (j in 0:3) {
  estimate<-arima(ts.sim8,order=c(i,0,j), include.mean = FALSE)
  abic[i+1,j+1]<-BIC(estimate)}} 
abic

estimate<-arima(diff(it_s,1),order=c(0,0,2), include.mean = FALSE)
names(estimate)

ts.plot(diff(it_s,1),estimate$series, main="IPI_Italy",xlab="year", ylab="IPI",lty=c(1,2),col=1:2)
legend("bottomleft", legend = c("NSA","SA"), col=1:2, lty = 1:2)

tsdisplay(estimate$residuals)
Box.test(estimate$residuals,lag=12,type='Ljung')
#adftest

x <- rnorm(1000)  # no unit-root
adf.test(x,,4)


