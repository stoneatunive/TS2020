3+2
sin(3+2)
7/sin(3+2)

###################################################

x <- 3
y <- 2
z <- sin(x+y)
z
w <- 7/sin(3+2)
w

###################################################

x <- c(1,3,9,49049,23)
x
y <- 1:10
z <- seq(12, 20, by=2)
z
w <- rep(10,5)
w

###################################################

z + w	# sum element by element
z / w	# divide element by element
z * w	# multiply element by element

###################################################

t(z) %*% w	# compute z'*w
z %*% t(w)	# compute z*w'

###################################################

X <- matrix(1:12, nrow=4)
X
Y <- matrix(1:12, nrow=4, byrow=T)
Y

###################################################

Z <- X * Y	# element by element matrix product
Z
W <- t(X) %*% Y	# matrix product
W

###################################################

W[c(1,3),2:1]

###################################################

mydata <- data.frame(weight=rnorm(5, mean=70, sd=2),
		     height=rnorm(5, 180, 5))
mydata

###################################################

str(mydata)
mydata$height
attributes(mydata)

###################################################

mystring <- c('hello', 'world')
mylist <- list(list1=mystring, list2=x, list3=W, list4=mydata)
mylist

###################################################

str(mylist)
attributes(mylist)
names(mylist)

###################################################

mylist[4]
mylist[[4]]

###################################################

mylist$list4
