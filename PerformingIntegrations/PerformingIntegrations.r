install.packages('mosaic')
install.packages('Deriv')

library(mosaic)
library(Deriv)

options(repr.plot.width = 8, repr.plot.height = 12)

f0 <- function(x) {
    m =0; b=3;
    m*x +b;
}
f0

plot(f0,xlim=c(0,5), ylim =c(0,6),lwd = 3, col='blue', main='f(x) = m * x +b')

I0 <- function(x) {integrate(f0,lower=0,upper=x)$value}
vect_fun <- Vectorize(I0,vectorize.args='x')

t <- seq(0,5,.5)
plot(t,vect_fun(t), xlim=c(0,5), ylim =c(0,20), lwd = 3, col='green', 
     ylab='f(x)', xlab='x', main='f(x) = m/2 * x^2 +b*x + c')

f1 <- function(x){
    2*x +2
}
f1

t <- seq(0,5,.5)
plot(f1, xlim=c(0,2), ylim =c(0,10), lwd = 3, col='blue', 
     ylab='f(x)', xlab='x', main='f(x) = 2 * x + 2 ')

I1 <- function(x) {integrate(f1,lower =0,upper=x)$value}
vect_fun <- Vectorize(I1,vectorize.args='x')

t <- seq(-5,5,.5)
plot(t,vect_fun(t), xlim=c(-5,5), ylim =c(-2,15), lwd = 3, col='green', 
     ylab='f(x)', xlab='x', main='f(x) =x^2 +2*x + c')

f2 <- function(x) {
    m =1; b=0;
    m*x +b;
}
f2

plot(f2,xlim=c(0,5), ylim =c(0,5),lwd = 3, col='blue', main='f(x) = x')

I2 <- function(x) {integrate(f2,lower =0,upper=x)$value}
vect_fun <- Vectorize(I2,vectorize.args='x')

t <- seq(-5,5,.5)
plot(t,vect_fun(t), xlim=c(-5,5), ylim =c(0,15), lwd = 3, col='green', 
     ylab='f(x)', xlab='x', main='f(x) =x^2/2 + c')

area <- integrate(f2, lower =-5,upper=5)$value
area

area <- integrate(f2, lower =0,upper=5)$value
area

t <- seq(0,5,.5)
plot(t,vect_fun(t), xlim=c(-5,5), ylim =c(0,15), lwd = 3, col='green', 
     ylab='f(x)', xlab='x', main='f(x) =x^2/2 + c')


