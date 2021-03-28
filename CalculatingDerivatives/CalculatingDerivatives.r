install.packages('mosaic')
install.packages('Deriv')

library(mosaic)
library(Deriv)

options(repr.plot.width = 8, repr.plot.height = 12)

f0 <- function(x) 4
f0

d0 <- Deriv(f0)
d0

f1 <- function(x) 4*x
f1

plot(f1,xlim=c(-1,6), ylim =c(0,25),lwd = 4, col='blue')

d1 <- Deriv(f1)
d1

f2 <- function(x) x^2
f2

plot(f2,xlim=c(-5,5), ylim =c(0,25),lwd = 4, col='blue')

d2 <- Deriv(f2)
d2

plot(d2,xlim=c(-2,2), ylim =c(-5,5),lwd = 3, col='blue')

makeFun(X)

v_t <- function(t) 5 + t*(3*t -12)
v_t

acc <- Deriv(v_t)
acc

acc(5)

plot(acc,xlim= range(-2,8), ylim =range(-18,25),lwd = 3, col='red' , ylab = "Acceleration" , xlab = "time",
    main="6*t -12")


