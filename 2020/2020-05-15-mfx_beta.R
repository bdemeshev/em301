# [v] 1. сравнение регрессий табличкой 
# [v] 2. предельные эффекты в логит моделях
# 3. 2SLS (IV)

library(tidyverse) # манипуляции
library(rio) # импорт данных
library(lmtest) # тесты для линейных моделей
library(skimr) # описательные статистики
library(mfx) # предельные эффекты
library(ivpack) # IV
library(texreg) # таблички сравнения моделей

# если "no package"
# либо опечатка
# либо Tools - Install packages

# 1. табличка сравнения

cars
qplot(data=cars, x=speed, y=dist) +
  xlab("Скорость машины, миль в час") +
  ylab("Длина тормозного пут, футы") +
  ggtitle("Данные по машинкам :)")

# модель 1
# dist_i = b_1 + b_2 speed_i + u_i
# модель 2
# dist_i = b_1 + b_2 speed_i + b_3 speed^2_i + u_i

m1 = lm(data = cars, dist ~ 1 + speed)
summary(m1)

cars2 = mutate(cars, sp2 = speed ^ 2)
glimpse(cars2)

m2 = lm(data = cars2, dist ~ 1 + speed + sp2)
summary(m2)

screenreg(list(m1, m2)) # на экран
screenreg(list(m1, m2), custom.model.names = c("TURLIG", "HANDRUM"))
?screenreg

texreg(list(m1, m2)) # в тех
htmlreg(list(m1, m2), file = "~/Downloads/junk.html") # hmtl -> ворд

# тест Вальда (F-тест)
waldtest(m1, m2)
# H0: обе верны (restr, unrestr)
# Ha: restr не верна, unrestr верна
# на 5% ур значимости H0 не отвергается
# выбираем m1

# 2. предельные эффекты

# загружаете свой набор данных
my_lovely_data = import("мой любимый файлик.xslx")

# встроенные наборы данных
data()

data("DoctorVisits") # AER
# если не срабатывает, то Tools - Install packages - AER
glimpse(DoctorVisits)
?DoctorVisits


qplot(data = DoctorVisits, x = visits)

dv = mutate(DoctorVisits, y = case_when(visits > 0 ~ 1,
                                        visits == 0 ~ 0))
glimpse(dv)
skim(dv)

# модель 1
# P(y_i = 1) = Logit(b_1 + b_2 gender_i + b_3 age_i + b_4 income_i)
# Logit(t) = 1 / (1 + exp(-t))

mef1 = logitmfx(data = dv, y ~ 1 + gender + age +  income)
mef1 # предельные эффекты
# по умолчанию предельные эффекты считаются для среднестатистического персонажа

# вариант 2: среднее от предельных эффектов по каждому индивиду
logitmfx(data = dv, y ~ 1 + gender + age +  income, atmean = FALSE)


summary(mef1$fit) # коэффициенты бета с крышкой

# как оценивается коэффициенты: ML
# se(dF/dx) с помощью дельта-метода

library(margins) # другой пакет для предельных эффектов :)

dv2 = mutate(dv, dumf = case_when(gender == "female" ~ 1,
                                gender == "male" ~ 0))
mod1 = glm(data = dv2, y ~ age + income + dumf, 
           family = binomial(link = "logit"))
summary(mod1)

margins(mod1, at = list(age = 0.2, income = 0.6, dumf = c(0, 1)))
?margins
