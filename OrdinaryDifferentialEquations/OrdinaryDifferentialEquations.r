install.packages('deSolve')
install.packages('rmarkdown')

library(ggplot2)
library(deSolve)
library(rmarkdown)

options(repr.plot.width = 8, repr.plot.height = 12)

time_interval <- seq(0,9,by = 1)
time_interval

state <- c(x =0)

parameters <- c()

model_fn <- function(t,state,parameters){
    with(as.list(c(state,parameters)), {
        dx <- 3
        
        return (list(dx))
    })
}

result <- ode(y = state, times = time_interval, func = model_fn, parms = parameters)
head(result,10)

plot <- ggplot(data = as.data.frame(result),aes(time,x))+
        geom_line(color='#2E86C1', size=1.5)
plot

time_interval <- seq(0,9,by = 1)

state <- c(x =3)

parameters <- c()

model_fn <- function(t,state,parameters){
    with(as.list(c(state,parameters)), {
        dx <- 3*t
        
        return (list(dx))
    })
}

result <- ode(y = state, times = time_interval, func = model_fn, parms = parameters)
head(result,1000)

plot <- ggplot(data = as.data.frame(result),aes(time,x))+
        geom_line(color='#2E86C1', size=1.5)
plot

time_interval <- seq(0,9,by = 1)

state <- c(x =4)

parameters <- c(k = 100)

model_fn <- function(t,state,parameters){
    with(as.list(c(state,parameters)), {
        dx <- t*t +k
        
        return (list(dx))
    })
}

result <- ode(y = state, times = time_interval, func = model_fn, parms = parameters)
head(result,1000)

plot <- ggplot(data = as.data.frame(result),aes(time,x))+
        geom_line(color='#2E86C1', size=1.5)
plot

time_interval <- seq(0,10,by = 0.5)
time_interval

diff_func <- function(x,k){
    x * (1 - k* x)
}

state <- c(x =0.1)

k <- 0.2
result_list <- c()

result_list[1] <- state

for (i in 1:(length(time_interval) -1)){
    result_list[i+1] = result_list[i] +
                       0.5 * diff_func(result_list[i],k)
}

result_list

approximation <- ggplot(data = data.frame(p= result_list,t= time_interval), aes(t,result_list))+
        geom_point(color='#2E86C1', size=3)
approximation

parameters <- c(k = 0.2)

model_fn <- function(t,state,parameters){
    with(as.list(c(state,parameters)), {
        dx <- x*(1-k*x)
        
        return (list(dx))
    })
}

result <- ode(y = state, times = time_interval, func = model_fn, parms = parameters)
head(result,10)

plot <- ggplot(data = as.data.frame(result),aes(time,x))+
        geom_line(color='#2E86C1', size=1.5)
plot

ggplot() + geom_point(data = data.frame(p = result_list, t= time_interval ), aes(t,result_list),size = 3)+
geom_line(data=as.data.frame(result), aes(time,x), color='#2E86C1',size = 1.5)

time_interval <- seq(from =0,to = 12,by = 0.05)


parameters <- c(r =1.3, k=12)

state <- c(x=0.1)

model_fn <- function(t,state,parameters){
    with(as.list(c(state,parameters)), {
        dx <- r*x*(1-x/k)
        
        return (list(dx))
    })
}

result <- ode(y = state, times = time_interval, func = model_fn, parms = parameters)
head(result,10)

plot <- ggplot(data = as.data.frame(result),aes(time,x))+
        geom_line(color='#2E86C1', size=1.5)
plot


