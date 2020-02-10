rm(list=ls())

### (a)
# set time series length
set.seed(0)
nobs=500
# generate 500 realisations from independent N(0,0.1)
epsilon=as.ts(rnorm(nobs, mean=0, sd=sqrt(0.1)))
# define two time series of length 501 with observations identically equal to 0
y=w=ts(rep(0,nobs+1))
for(i in 2:(nobs+1)){
  y[i] = y[i-1] + epsilon[i]
  w[i] = w[i-1] + y[i]
}

### (b)
opar=par(mfrow=c(3,1))
plot(epsilon)
plot(diff(y))
plot(diff(y,differences=2))

### (b)
plot(ts.union(epsilon,y,w))