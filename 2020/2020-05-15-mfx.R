library(tidyverse) # манипуляции с данными и графики
library(mfx) # предельные эффекты в логит-пробит моделях
library(rio) # если читать внешний набор данных (excel/csv)
library(lmtest) # для тестов LR, LM, Wald, ...
library(texreg) # представление результатов в табличках в тех (или в ворд)
library(ivpack) # IV и 2SLS
library(skimr) # описательные статистики


# если говорит "нет пакета"
# либо опечатка в названии либо Tools - Install packages...

# загружаете данные
data = import("мой любимый файлик.xlsx")

# 1. табличка со сравнением моделей
# 2. предельные эффекты для логит модели
# 3. 2SLS

# цель 1 на примере мнк :)
head(cars)
qplot(data=cars, x=speed, y=dist) + ggtitle("Машинки") +
  xlab("Скорость, миль в час") + ylab("Длина тормозного пути, футов")

# модель 1
# y_i = b_1 + b_2 x_i + u_i
# модель 2
# y_i = b_1 + b_2 x_i + b_3 x_i^2 + u_i

m1 = lm(data=cars, dist ~ 1 + speed)
summary(m1)

cars2 = mutate(cars, sp2 = speed ^ 2)
glimpse(cars2)

m2 = lm(data=cars2, dist ~ 1 + speed + sp2)
summary(m2)

texreg(list(m1, m2)) # по дефолту для теха
htmlreg(list(m1, m2), file = "~/Downloads/junk.html") 
screenreg(list(m1, m2))

# F-test
waldtest(m1, m2)
# p-value > 0.05, H0 is not rejected
# H0: обе модели верны
# H1: ограниченная не верная, неограниченная верна
# выбираю m1

# сюжет 2. предельные эффекты для логит-модели
data(Guns)
?Guns
data(DoctorVisits)
?DoctorVisits

glimpse(DoctorVisits)
qplot(data = DoctorVisits, x = visits)

dv = mutate(DoctorVisits, y = (visits > 0))
glimpse(dv)
skim(dv)


# P(y_i = 1) = Logit(b_1 + b_2 gender_i + b_3 age_i + b_4 income_i)
# Logit(t) = 1 / (1 + exp(-t))

mef = logitmfx(data = dv, y ~ 1 + gender + age + income)
# предельные эффекты
mef
# если взять среднестатистического персонажа

# 5.8e-06 = 5.8 * 10^{-6} = 0.0000058


# сама логит-модель и коэффициенты бета с крышкой
summary(mef$fit)


mef2 = logitmfx(data = dv, y ~ 1 + gender + age)
# предельные эффекты
screenreg(list(mef, mef2))
?DoctorVisits
