library("dplyr")
library("ggplot2")

z <- c(5,6,NA,7)
z^2
mean(z)
mean(z,na.rm = TRUE)

w <- runif(100,min = 0, max=1)
w
9/0
1/Inf

t <- data.frame(x=c(1,2,3,4),y=c(5,6,7,8),
                z=runif(4,min=0,max=1))
t 

t[2,3]
t[2,]
t[,2]
t
t$y

help(prcomp)

h <- cars
help(cars)
cars

qplot(data=h, speed, dist)
qplot(data=h, speed, dist,
      xlab="Скорость машины, миль в час",
      ylab="Длина тормозного пути, футов",
      main="Данные по машинам 1920х годов")

imena <- c("Маша","Оля","Айса")
imena
imena[3]

chulan <- list(table=t,imena=imena,r=9)
chulan$imena
chulan$r

model_fatIma <- lm(data=h, dist~speed)
model_fatIma$coefficients
model_fatIma$residuals
model_fatIma$fitted.values

summary(model_fatIma)


