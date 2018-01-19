a <- 7
b <- 8
a + b

# Приветсвуем вновь прибывших!

library(tidyverse)
?qchisq


d <- diamonds
?diamonds

glimpse(d) # краткая информация
head(d) # начало таблички
tail(d) # хвост таблички

d$price # из двумерной таблички извлек столбец price

# отберу из таблички d в табличку d2
# бриллианты с ценой выше среднего
d2 <- filter(d, price > mean(price))
glimpse(d2)

# регрессия
# 5 -> x
model_0 <- lm(data = d2, 
      price ~ carat + table)
summary(model_0)
confint(model_0, level = 0.9)


model_nconst <- lm(data = d2, 
              price ~ 0 + carat + table)
summary(model_nconst)