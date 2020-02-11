x <- rnorm(90)
y <- rnorm(100, 2, .1)
layout(1:2)
plot(x)
plot(y)
class(x)
class(y)

###################################################

x1 <- ts(data=x, start=c(1949,2), frequency=4)
y1 <- ts(y, start=c(1948,1), frequency=4)
class(x1)

###################################################

attributes(x1)

# tsp provides start date, end date, and frequency
tsp(x1)

###################################################

window(x1, start=c(1953,3), # extract a subset of the time
       end=c(1955,4))       # series 
layout(1:2)
plot(x1)  # actually use the method plot.ts()
plot(y1)

###################################################

xyjoin <- ts.union(x1,y1)
window(xyjoin, start=1948.00, end=1950.75)

###################################################

class(xyjoin)
str(xyjoin)
tsp(xyjoin)

###################################################

plot(xyjoin, yax.flip=T)

###################################################

xyint <- ts.intersect(x1,y1)
tsp(xyint)
window(xyint, start=1949.25, end=1951.25)

###################################################

class(xyint)
str(xyint)
tsp(xyint)

###################################################

plot(xyint, yax.flip=T)

###################################################

plot(xyint, plot.type='single',
     col=c('red','blue'), lty=1:2)
legend(x='topleft', legend=c('x1', 'y1'),
       col=c('red', 'blue'), lty=1:2)

###################################################

plot(xyint, plot.type='single',
     col=c('red','blue'), lty=1:2)
legend(x='bottomright', legend=c('x1', 'y1'),
       col=c('red', 'blue'), lty=1:2)
