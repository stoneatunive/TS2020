##SimulateVAR(2)-data
library(tseries)
library(dse)
library(vars)

set.seed(123456) # Reset random number generator for reasons of reproducability

# Generate sample
t <- 200 # Number of time series observations
k <- 2 # Number of endogenous variables
p <- 2 # Number of lags



# Generate coefficient matrices -  example 1.a for comparison
#A.0 <-c(5,10)
A.1 <- matrix(c(.7, .2, .2, .7), k) # Coefficient matrix of lag 1
A.2 <- matrix(c(0, 0, 0, 0), k) # Coefficient matrix of lag 2
A <- cbind(A.1, A.2) # Companion form of the coefficient matrices

# Generate coefficient matrices -  example 1.b for comparison
#A.0 <-c(5,10)
A.1 <- matrix(c(.5, -.2, -.2, .5), k) # Coefficient matrix of lag 1
A.2 <- matrix(c(0, 0, 0, 0), k) # Coefficient matrix of lag 2
A <- cbind(A.1, A.2) # Companion form of the coefficient matrices

# Generate coefficient matrices -  example 1.c for comparison
#A.0 <-c(5,10)
A.1 <- matrix(c(.5, .5, .5, .5), k) # Coefficient matrix of lag 1
A.2 <- matrix(c(0, 0, 0, 0), k) # Coefficient matrix of lag 2
A <- cbind(A.1, A.2) # Companion form of the coefficient matrices

# Generate coefficient matrices -  example 1.d for comparison
A.0 <-c(.5,0)
A.1 <- matrix(c(.5, .5, .5, .5), k) # Coefficient matrix of lag 1
A.2 <- matrix(c(0, 0, 0, 0), k) # Coefficient matrix of lag 2
A <- cbind(A.1, A.2) # Companion form of the coefficient matrices

# Generate coefficient matrices - example 2
A.0 <-c(5,10)
A.1 <- matrix(c(.5, -.2, .2, -.5), k) # Coefficient matrix of lag 1
A.2 <- matrix(c(-.3, -.1, -.7, .3), k) # Coefficient matrix of lag 2
A <- cbind(A.1, A.2) # Companion form of the coefficient matrices




# Generate series
series <- matrix(0, k, t + 2*p) # Raw series with zeros
for (i in (p + 1):(t + 2*p)){ # Generate series with e ~ N(0,0.5)
  a<-rnorm(k, 0, .5)
  series[, i] <- A.1%*%series[, i-1] + A.2%*%series[, i-2] + a 
}
series<-A.0+series

vardat <- ts(t(series[, -(1:p)])) # Convert to time series format
colnames(vardat) <- c("y1", "y2") # Rename variables



dev.new()
plot.ts(vardat) # Plot the series
ts.plot(vardat, main="example",xlab="year", ylab="sim",lty=c(1,2),col=1:2)
legend("bottomleft", legend = c("y1","y2"), col=1:2, lty = 1:2)



##Determininganappropriatelag-order
infocrit<-VARselect(vardat,lag.max=4,
                    type="const")
infocrit


##Estimatingthemodel 
varsimest<-VAR(vardat,p=3,type="const",
               season=NULL,exogen=NULL)
varsimest$varresult
##Alternatively,selectionaccordingtoBIC
varsimest1<-VAR(vardat,type="const",
               lag.max=3,ic="SC")
varsimest$varresult$y1
varsimest$varresult$y2
##Checkingtheroots
roots<-roots(varsimest)
roots

#################################################
##testingserialcorrelation
#args(serial.test)
##Portmanteau-Test
var2c.serial<-serial.test(varsimest,lags.pt=16,
                          type="PT.asymptotic")
var2c.serial
plot(var2c.serial,names="y1")
plot(var2c.serial,names="y2")
##testingheteroscedasticity
#args(arch.test)
var2c.arch<-arch.test(varsimest,lags.multi=5,
                      multivariate.only=TRUE)
var2c.arch
##testingfornormality
#args(normality.test)
var2c.norm<-normality.test(varsimest,
                           multivariate.only=TRUE)
var2c.norm
##classandmethodsfordiganostictests
#class(var2c.serial)
#class(var2c.arch)
#class(var2c.norm)
#methods(class="varcheck")
##Plotofobjects-varcheck"
#args(vars:::plot.varcheck)
#plot(var2c.serial,names="y1")


###############CUSUM

reccusum<-stability(varsimest,
                    type="OLS-CUSUM")
fluctuation<-stability(varsimest,
                       type="fluctuation")

plot(fluctuation)


##Causalitytests1
##Grangerandinstantaneouscausality
var.causal<-causality(varsimest,cause="y2")
var.causal

##Forecasting objects of class varest
#args(vars:::predict.varest)
predictions<-predict(varsimest, n.ahead=25,
                     ci=0.95)
class(predictions)
args(vars:::plot.varprd)
#Plot of predictions for y1
plot(predictions, names="y1")
plot(predictions, names="y2")
##Fanchart for y2
args(fanchart)
fanchart(predictions, names="y1")
fanchart(predictions, names="y2")

##Impulse response analysis
irf.y1<-irf(varsimest, impulse="y1",
            response="y2", n.ahead=10,
            ortho=FALSE, cumulative=FALSE,
            boot=FALSE, seed=12345)
args(vars:::plot.varirf)
plot(irf.y1)

irf.y2<-irf(varsimest, impulse="y2",
            response="y1", n.ahead=10,
            ortho=TRUE, cumulative=FALSE,
            boot=FALSE, seed=12345)
args(vars:::plot.varirf)
plot(irf.y2)

##Forecast error variance decomposition
dev.new()
fevd.var2<-fevd(varsimest, n.ahead=10)
args(vars:::plot.varfevd)
plot(fevd.var2, addbars=2)


# Generate coefficient matrices - one more example
A.1 <- matrix(c(-.3, .6, -.4, .5), k) # Coefficient matrix of lag 1
A.2 <- matrix(c(-.1, -.2, .1, .05), k) # Coefficient matrix of lag 2
A <- cbind(A.1, A.2) # Companion form of the coefficient matrices

#white noise example
#series1<-ts(rnorm(200)) 
#series2<-ts(rnorm(200))
#vardat <-ts.union(series1,series2)

#production example
#y1<-diff(it_s,1)
#y2<-diff(de_s,1)
#vardat<-ts.union(y1,y2)
