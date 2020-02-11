
library(tseries)
library(forecast)
#connect directly to eurostat database
library(RJSDMX)
library("TSsdmx")
getProviders()

eurostat <- TSconnect("sdmx", dbname="EUROSTAT")
a=getFlows('EUROSTAT')

ecb <- TSconnect("sdmx", dbname="ECB")

a2=getFlows('ECB','YC')

a3=getFlows('EUROSTAT','*sts_inpr_m*')
getDimensions('EUROSTAT', 'sts_inpr_m')
getCodes('EUROSTAT','sts_inpr_m', 'S_ADJ')
getCodes('EUROSTAT','sts_inpr_m', 'FREQ')
getCodes('EUROSTAT','sts_inpr_m', 'INDIC_BT')
getCodes('EUROSTAT','sts_inpr_m', 'UNIT')
names(getCodes('EUROSTAT','sts_inpr_m', 'S_ADJ'))

#print(verifyQuery('EUROSTAT','sts_inpr_m.M.PROD.B-D.NSA.I15.IT'))
it_n <- TSget('sts_inpr_m.M.PROD.B-D.NSA.I15.IT',eurostat,start=2000)
it_s <- TSget('sts_inpr_m.M.PROD.B-D.SCA.I15.IT',eurostat,start=2000)
de_n <- TSget('sts_inpr_m.M.PROD.B-D.NSA.I15.DE',eurostat,start=2000)
de_s <- TSget('sts_inpr_m.M.PROD.B-D.SCA.I15.DE',eurostat,start=2000)
fr_n <- TSget('sts_inpr_m.M.PROD.B-D.NSA.I15.FR',eurostat,start=2000)
fr_s <- TSget('sts_inpr_m.M.PROD.B-D.SCA.I15.FR',eurostat,start=2000)
plot(it_n)

#-------------national accounts selection
getDimensions('EUROSTAT', 'nasa_10_nf_tr')
getCodes('EUROSTAT','nasa_10_nf_tr', 'SECTOR')
na_nf_tr <- TSget('nasa_10_nf_tr.A.CP_MEUR.RECV|PAID.D11.S14_S15.IT',eurostat,start=2000)
plot(na_nf_tr[,1])
#-------------



ts.plot(it_n,it_s, main="IPI_Italy",xlab="year", ylab="IPI",lty=c(1,2),col=1:2)
legend("bottomleft", legend = c("NSA","SA"), col=1:2, lty = 1:2)

tsdisplay(it_s)
tsdisplay(it_n)
ggmonthplot(it_n)
ggmonthplot(it_s)
ggseasonplot(it_n)
lag.plot(it_s[1:224],12)


