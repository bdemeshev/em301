# встроенный набор данных cars
head(cars)
help(cars)

library(dplyr) # пакет для операций с данными
# переводим мили в км, а футы в метры
cars <- cars %>% mutate(kmh=speed*1.609,
                        torm=dist*0.304) 
# добавляем квадрат скорости
cars <- cars %>% mutate(kmh2 = kmh^2)
head(cars)

library(ggplot2) # пакет для графиков
qplot(data=cars,kmh,torm)

# оцениваем три модели
m1 <- lm(data=cars,torm~kmh)
summary(m1)
m2 <- lm(data=cars,torm~0+kmh)
summary(m2)
m3 <- lm(data=cars,torm~kmh2)
summary(m3)
