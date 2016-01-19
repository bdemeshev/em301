# if you can't read cyrillic letters then:
# File - Reopen with encoding - utf8 (set as default)

# базовые арифметические операции

a <- 7
b <- 9
a * b
42 / 0
5 / (42 / 0)
atan(Inf)
pi / 2
pi
7 + Inf

# вектора
v <- c(5, 6, 2.5)
v + 7
v * 2
sum(v)
mean(v)
w <- c(7, 2, -5)
v + w
v[2]
v[2] <- 1000
v

# число 5 повторенное 100 раз
q <- rep(5, 100)
q

# числа от 20 до 400
r <- 20:400
r

# встроенный набор данных
cars
help(cars)
str(cars)

# выбираем нужный элемент (или часть) таблицы
cars[5, 2]
cars[5, ]
cars[ , 2]
cars$speed
cars$speed[2]

mean(cars$speed)
min(cars$speed)
cars[7,2]

r <- list(a = 8, b = c(2, 6), h = cars)
r$a
r$b
r$h
r$c <- c(5, 6, 7)
r$c


model_1 <- lm(data = cars, dist ~ 0 + speed)
model_1
model_2 <- lm(data = cars, dist ~ speed)
model_2
model_3 <- lm(data = cars, dist ~ speed + I(speed ^ 2))
model_3

library("dplyr")
library("ggplot2")

# Привет, Андрей!

swiss
help(swiss)
model_4 <- lm(data = swiss,
              Fertility ~ Agriculture + Catholic)

model_4

model_2 <- lm(data = cars, dist ~ speed)
model_2
summary(model_2)

str(cars)

# P(t_48 < 1.5)
pt(1.5, df = 48)
pt(1.5, df = 48) - pt(1, df = 48)
qt(0.975, df = 48)

# RSS
deviance(model_2)
# оценка ковариационной матрицы
vcov(model_2)

summary(model_4)
