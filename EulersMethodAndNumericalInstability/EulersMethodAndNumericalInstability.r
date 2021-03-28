y_dash <- function(t_i, y){
    y
}

y_dash

# Euler's Method
t_0 <- 0
t_n <- 6
h <- 1
y_0 <- 1

euler_method <- function(t_0, h, t_n, y_0, y_dash){
    t_seq <- seq(t_0, t_n, by = h)

    data_frame <- data.frame(t_i = t_seq)
    
    data_frame$h <- h
    data_frame$y_i <- y_0
    
    data_frame$f_dash[1] <- y_dash(data_frame$t_i[1], y_0)
    
    for (i in 2:nrow(data_frame)) {

        data_frame$y_i[i] <- data_frame$y_i[i - 1] +  h * data_frame$f_dash[i - 1]
        
        data_frame$f_dash[i] <- y_dash(data_frame$t_i[i], data_frame$y_i[i])
        
    }
    return (data_frame)
}

table <- euler_method(t_0, h, t_n, y_0, y_dash)
table

colnames(table) <- c('t.i', 'h', 'y.i', 'Dy/dt')
table

table$y <- exp(table$t.i)
table

plot(table$t.i, table$y.i, col = 'red', type = 'l', lwd = 3)
points(table$t.i, table$y.i, col ='black', lwd = 3)

lines(table$t.i, table$y, col ='green', lwd = 3)
points(table$t.i, table$y, col ='black', lwd = 3)

grid(col = 'black')
legend('topleft', legend = c('Eular', 'Exact'), lwd = 2, col = c('red', 'green'))

table$error <- table$y.i - table$y
table

t_0 <- 0
t_n <- 6
h <- 0.03
y_0 <- 1

table <- euler_method(t_0, h, t_n, y_0, y_dash)
head(table)

colnames(table) <- c('t.i', 'h', 'y.i', 'Dy/dt')

table$y <- exp(table$t.i)

head(table)

plot(table$t.i, table$y.i, col = 'red', type = 'l', lwd = 3)

lines(table$t.i, table$y, col ='green', lwd = 3)

grid(col = 'black')
legend('topleft', legend = c('Eular', 'Exact'), lwd = 2, col = c('red', 'green'))

table$error <- table$y.i - table$y
head(table)

y_dash <- function(t_i, y){
    -2.3*y
}

y_dash

t_0 <- 0
t_n <- 6
h <- 1
y_0 <- 1

table <- euler_method(t_0, h, t_n, y_0, y_dash)
head(table)

colnames(table) <- c('t.i', 'h', 'y.i', 'Dy/dt')

table$y <- exp(-2.3 *table$t.i)

head(table)

plot(table$t.i, table$y.i, col = 'red', type = 'l', lwd = 3)
points(table$t.i, table$y.i, col ='black', lwd = 3)

lines(table$t.i, table$y, col ='green', lwd = 3)
points(table$t.i, table$y, col ='black', lwd = 3)

grid(col = 'black')
legend('topleft', legend = c('Eular', 'Exact'), lwd = 2, col = c('red', 'green'))

t_0 <- 0
t_n <- 6
h <- 0.6
y_0 <- 1

table <- euler_method(t_0, h, t_n, y_0, y_dash)

colnames(table) <- c('t.i', 'h', 'y.i', 'Dy/dt')

table$y <- exp(-2.3 *table$t.i)

head(table)

plot(table$t.i, table$y.i, col = 'darkblue', type = 'l',lty =2, lwd = 3)
points(table$t.i, table$y.i, col ='red', lwd = 3)

lines(table$t.i, table$y, col ='green', lwd = 3)

grid(col = 'black')
legend('topleft', legend = c('Eular', 'Exact'), lwd = 2, col = c('blue', 'green'))


