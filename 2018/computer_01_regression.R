library(tidyverse)
library(rio)
library(skimr) 
library(car) # linearHypothesis
library(lmtest) # тесты для линейных моделей

# как бы я загрузил данные...
data = import("... откуда взять данные ...")

# diamonds — встроенный набор данных
head(diamonds) # начало набора данных
skim(diamonds) # описательные статистики

model_0 = lm(data = diamonds, price ~ x + y + z + carat + cut)
summary(model_0)

# добавили плохой, линейно зависимый регрессор
d2 = mutate(diamonds, sum_xy = x + y)
head(d2)

model_bad = lm(data = d2, price ~ x + y + sum_xy)
summary(model_bad)

# доверительный интервал
confint(model_0)

H0 = c("x=y", "carat=0")
linearHypothesis(model_0, H0)

# альтернативный способ с явной оценкой двух моделей
model_r = lm(data = diamonds, price ~ cut + carat)
# model_r — частный случай model_0
waldtest(model_r, model_0)

summary(model_r)



model_b = lm(data = diamonds, price ~ x + y + z + carat + factor(cut, ordered = FALSE))  
summary(model_b)


