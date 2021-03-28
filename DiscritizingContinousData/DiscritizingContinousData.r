install.packages('lambda.tools')

library(lambda.tools)
library(ggplot2)

options(repr.plot.width = 12, repr.plot.height = 8)

con_value <- seq(-20,20, by=0.5)
con_value

qnt_01 <- quantize(con_value)
qnt_01

qnt_df_01 <- data.frame(con_value, qnt_01)
head(qnt_df_01, 5)

ggplot(qnt_df_01, aes(x=con_value, y=qnt_01))+
geom_point(size= 3 , aes(color = factor(qnt_01)))

qnt_02 <- quantize(con_value, bins = c (-4,-3,-2,-1,0,1,2,3,4))
qnt_02

qnt_df_02 <- data.frame(con_value, qnt_02)
head(qnt_df_02, 5)

ggplot(qnt_df_02, aes(x=con_value, y=qnt_02))+
geom_point(size= 3 , aes(color = factor(qnt_02)))

x <-  seq(0, 2*pi , by = 0.1)
x

y <- sin(x)
y

sine_wave <- qplot(x,y, geom="path", xlab="time", ylab = "Sine wave")+
             geom_point(col='red', size =3)+
             geom_hline(aes(yintercept =0), size = 1 , col = 'black')
sine_wave

qnt_03 <- quantize(sin(x), bins = c (-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1))
qnt_03

qnt_df_03 <- data.frame(x, qnt_03)
head(qnt_df_03, 5)

ggplot(qnt_df_03, aes(x=x, y=qnt_03))+
geom_point(size= 3 , aes(color = factor(qnt_03)))

sine_wave + geom_step(aes(x = x , y = qnt_03), col = 'blue' , size =0.75)


