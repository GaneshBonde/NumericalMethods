install.packages("ggplot2")
install.packages('polynom')

library(ggplot2)
library(polynom)

options(repr.plot.width =12, repr.plot.height= 8)

x <- c(2,6)
y <- c(2,12)

two_points <- data.frame(x,y)
two_points

plot(x, y, pch = 19 , cex = 3 , main = 'Two points')

interp_pts_linear <- approx (x , y , method = 'linear', n =20)
interp_pts_linear$x

interp_pts_linear$y

interp_data_points <- data.frame(cbind(interp_pts_linear$x, interp_pts_linear$y))
colnames(interp_data_points) <- c('x_i', 'y_i')

interp_data_points

plot(x ,y, pch = 19, cex = 3 , main = 'Linear interpolation')
points(interp_pts_linear, pch = 16 , cex = 1.5 , col ='red')

interp_pts_constant  <- approx(x,y, method ='constant', n = 20)
interp_pts_constant$x

interp_pts_constant$y

interp_data_points <- data.frame(cbind(interp_pts_constant$x, interp_pts_constant$y))
colnames(interp_data_points) <- c('x_i','y_i')
interp_data_points

plot(x,y, pch=13, cex = 3 , main ='Constant interpolation')
points(interp_pts_constant, col = 'green2', pch = 22 , cex = 2)

plot(x,y, pch =19, cex =3)
points(interp_pts_linear , col = 'red' , pch = 16 , cex = 1.5)
points(interp_pts_constant , col = 'green2', pch = 22 , cex = 2)
legend(x = 'topleft', c('Data', 'linear' , 'constant'), pch = c(19,16,22),
      col= c("black", "red","green"), bg = "white" , cex = 1.25)

estimate_value <- approx(x , y, xout = 3.8)
estimate_value

plot(x,y, pch = 19, cex =3)
points(interp_pts_linear, col = 'red' , pch = 16 , cex = 1.5 )
points(approx(x,y,xout = estimate_value$x) , col='blue' , cex = 2 , pch =16)

legend(x = 'topleft', c('Data', 'linear' , 'specific point'), pch = c(19,16,22),
      col= c("black", "red","green"), bg = "white" , cex = 1.5)

# With rule 1
plot(x,y, pch = 19, cex =3)
points(interp_pts_linear, col = 'red' , pch = 16 , cex = 1.5 )
plot(approxfun(x,y,rule = 1), 0 , 7, , pch =22, col = 'orange' , add = TRUE , lwd = 3)


# With rule 2
plot(x,y, pch = 19, cex =3)
points(interp_pts_linear, col = 'red' , pch = 16 , cex = 1.5 )
plot(approxfun(x,y,rule = 2), 0 , 7, , pch =22, col = 'orange' , add = TRUE , lwd = 3)

# With rule 2:1
plot(x,y, pch = 19, cex =3)
points(interp_pts_linear, col = 'red' , pch = 16 , cex = 1.5 )
plot(approxfun(x,y,rule = 2:1), 0 , 7, , pch =22, col = 'orange' , add = TRUE , lwd = 3)

# With rule 1:2
plot(x,y, pch = 19, cex =3)
points(interp_pts_linear, col = 'red' , pch = 16 , cex = 1.5 )
plot(approxfun(x,y,rule = 1:2), 0 , 7, , pch =22, col = 'orange' , add = TRUE , lwd = 3)

# With rule 2:2
plot(x,y, pch = 19, cex =3)
points(interp_pts_linear, col = 'red' , pch = 16 , cex = 1.5 )
plot(approxfun(x,y,rule = 2:2), 0 , 7, , pch =22, col = 'orange' , add = TRUE , lwd = 3)

x <- c(-3, -2, -1, 0, 1,2,3)
y <- c(-33, -9 , -1.5 , 2, 5, 12, 36)
data_points <- data.frame(cbind(x,y))

head(data_points , 10)


ggplot(data_points, aes(x =x, y=y)) + geom_point(size = 5 , col ='red')

poly_values <- poly.calc(x,y)
poly_values

poly_fun <- function(x){
    return (2 + 2.875*x - 0.3055556*x^2 + 0.3020833*x^3 + 0.05902778*x^4 + 0.07291667*x^5 -  
0.003472222*x^6)
}

ggplot(data_points, aes(x=x,y=y)) + geom_point(size=4,col ='red')+ 
stat_function(fun = poly_fun, col = 'green' , size = 1 , alpha =1)

expected_data <- data.frame(x = 3:6)
expected_data

expected_data$y <- predict(lm(y ~ poly_fun(x), data = data_points), newdata = expected_data)

expected_data

ggplot(expected_data, aes  (x=x,y=y))+ 
geom_line(col='blue', data = expected_data, size=2) +
geom_point(col ='red', size = 4)

num <- 1e3
sine_data <- data.frame(
    x = 1: num,
    y = sin(seq(0, 7*pi, length.out = num)) + rnorm(n = num , mean = 0, sd = 0.2)
)

ggplot(sine_data , aes(x = x , y=y)) + geom_point(size = 4 , col = 'red')

approx_data <- data.frame(
    with(sine_data, 
         approx(x , y, xout = seq(1 , num, by =2), method ='linear')
         ), 
         method = 'approx()'
)

ggplot(approx_data, aes(x=x, y=y))+
geom_point(dat = sine_data, aes(x,y) , alpha = 0.2 , col = 'red')+
geom_line(size = 1, col = 'blue')

spline_data <- data.frame(
    with(sine_data, 
         spline(x , y, xout = seq(1 , num, by =2))
         ), 
         method = 'spline()'
)

ggplot(spline_data, aes(x=x, y=y))+
geom_point(dat = sine_data, aes(x,y) , alpha = 0.2 , col = 'red')+
geom_line(size = 1, col = 'blue')

smooth_data <- data.frame(
   x = 1:num,
   y = as.vector(smooth(sine_data$y)),
   method = "smooth()"
)

ggplot(smooth_data, aes(x=x, y=y))+
geom_point(dat = sine_data, aes(x,y) , alpha = 0.2 , col = 'red')+
geom_line(size = 1, col = 'blue')


