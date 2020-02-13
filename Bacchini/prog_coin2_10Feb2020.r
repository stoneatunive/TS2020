#---------------------------------------------------------------------
#
# Time Series Econometrics
#
# VAR Forecasting, Causality, Cointegration  Example
#
# April 2019
#
#---------------------------------------------------------------------
#
#
#
#---------------------------------------------------------------------
# Load packages
# The Canadian data set which is included in the package vars is brought into memory.
library("vars")
library("urca")
library("forecast")
library("VAR.etp")
data("Canada")
#--------------------------------------------------------------
View(Canada)



#---------------------------------------------------------------------
summary(Canada)
plot(Canada, nc = 2, xlab = "")
#--------------------------------------------------------------

#other ADF test - example WN
help("ur.df")
x=rnorm(1000,1)
wn <- ts(x, start=2000,frequency=12)
summary(ur.df(wn, type = "trend", lags = 2))


#---------------------------------------------------------------------
adf1prod <- summary(ur.df(Canada[, "prod"], type = "trend", lags = 2))
adf1prod
adf2prod <- summary(ur.df(diff(Canada[, "prod"]), type = "drift", lags =1))
adf2prod
#---------------------------------------------------------------------


#---------------------------------------------------------------------
adf1e <- summary(ur.df(Canada[, "e"], type = "trend", lags = 2))
adf1e
adf2e <- summary(ur.df(diff(Canada[, "e"]), type = "drift", lags =1))
adf2e
#---------------------------------------------------------------------



#---------------------------------------------------------------------
adf1rw <- summary(ur.df(Canada[, "rw"], type = "trend", lags = 2))
adf1rw
adf2rw <- summary(ur.df(diff(Canada[, "rw"]), type = "drift", lags =1))
adf2rw
#---------------------------------------------------------------------
summary(ur.df(x, type = "trend", lags = 2))

#---------------------------------------------------------------------
adf1U<- summary(ur.df(Canada[, "U"], type = "trend", lags = 2))
adf1U
adf2U <- summary(ur.df(diff(Canada[, "U"]), type = "drift", lags =1))
adf2U
#---------------------------------------------------------------------


#VAR selection
help("VARselect")

VARselect(Canada, lag.max = 5, type = "const")



#---------------------------------------------------------------------
var.2c <- VAR(Canada, p = 2, type = "const")
summary(var.2c)
roots(var.2c)
#---------------------------------------------------------------------
#var.3c <- VAR(diff(Canada,1), p = 2, type = "const")
#summary(var.3c)

plot(var.2c, names = "e")

#---------------------------------------------------------------------
help("serial.test")

var2c.pt.adj <- serial.test(var.2c, lags.pt = 16, type = "PT.adjusted")
var2c.pt.adj

var2c.BG <- serial.test(var.2c, lags.pt = 16, type = "BG")
var2c.BG
#---------------------------------------------------------------------



#---------------------------------------------------------------------
#Forecasting.
fcst <- forecast(var.2c, h=4)
plot(fcst, xlab="Year")
fcst
#---------------------------------------------------------------------



#---------------------------------------------------------------------
#Granger causality. (Toda-Yamamoto)
restrict = rbind( c(1,1,4),c(2,1,4))
VAR.Wald(Canada[, c("e", "U", "rw", "prod")], p=3, restrict, type ="const")
#---------------------------------------------------------------------


#---------------------------------------------------------------------
#Granger causality. (Toda-Yamamoto)
restrict = rbind( c(1,1,4),c(2,1,4))
VAR.Wald(Canada[, c("e", "U", "rw", "prod" )], p=3, restrict, type ="const")
#---------------------------------------------------------------------


#---------------------------------------------------------------------
#Granger causality. (Toda-Yamamoto)
restrict = rbind( c(1,1,4),c(2,1,4))
VAR.Wald(Canada[, c("e", "rw", "prod", "U")], p=3, restrict, type ="const")
#---------------------------------------------------------------------




#---------------------------------------------------------------------
#Cointegration. Trace Test
test1 <- ca.jo(Canada,ecdet="trend",type="trace",K=2,spec="transitory")
test1
summary(test1)
plot(test1)
#---------------------------------------------------------------------


#---------------------------------------------------------------------
#VECM
vecm <- ca.jo(Canada[, c("rw", "prod", "e", "U")], type = "trace",ecdet = "trend", K = 2, spec = "transitory")
vecm.r1 <- cajorls(vecm, r = 1)
vecm.r1

fcst <- forecast(var.2c, h=4)


