library(tidyverse) # манипуляции с данными и графики
library(lmtest) # тесты для линейных моделей
library(sandwich) # оценки Var(beta hat) при гетероскедастичности

glimpse(diamonds)
help("diamonds")

qplot(data = diamonds, x = carat, y = price)
qplot(data = diamonds, x = carat, y = price, alpha = 0.002)

d_subset <- sample_n(diamonds, 5000)
qplot(data = d_subset, x = carat, y = price)

qplot(data = diamonds, x = carat, 
      y = price, 
      geom = "hex")


qplot(data = d_subset, 
      x = carat, y = price) + 
  xlab("Масса (карат)") +
  ylab("Цена (долларов)") +
  ggtitle("Бриллианты :)")

model_0 <- lm(data = diamonds, 
              price ~ carat + x + y + z)
summary(model_0) # отчёт со словами
coeftest(model_0) # только табличка про коэффициенты

vcov(model_0) # оценка Var(beta hat) при гомоскедастичности

# оценка Var(beta hat) при гетероскедастичности
vcovHC(model_0) # по умолчанию HC3
vcovHC(model_0, type = "HC1") # как в стате

# проверка значимости с использованием устойчивых 
# гетероскедастичности se_HC(beta hat)
coeftest(model_0, vcov. = vcovHC)
help(coeftest)

# оценим ограниченную модель
model_r <- lm(data = diamonds, price ~ carat)

# F-тест:
# F = ( (RSS_R - RSS_UR) / q ) / (RSS_UR / (n - k_UR) )
# при гомоскедастичности!
waldtest(model_r, model_0, test = "F")

# тест Вальда
# при гомоскедастичности!
waldtest(model_r, model_0, test = "Chisq")
# H0: beta_x = beta_y = beta_z = 0 отвергается!

# тест Вальда
# при гетероскедастичности!
waldtest(model_r, model_0, test = "Chisq", 
         vcov = vcovHC)
# H0: beta_x = beta_y = beta_z = 0 отвергается!

# Тест Уайта (классический, без корректировки Коэнкера)
# Шаг 1
# Построили регрессию y = beta_1 + beta_2 x + u
# раздобыли остатки u_hat
# Шаг 2
# подозреваем, что есть факторы влияющие на Var(u|X)
# Построили регрессию
# u_hat^2 = gamma_1 + gamma_2 z + gamma_3 z^2 + v
# H_0: гомоскедастичность, Var(u|X) = const
# gamma_2 = 0, gamma_3 = 0
# LM = n R^2_{step 2}

bptest(data = diamonds, 
       price ~ carat + x + y + z, 
       varformula = ~ carat, studentize = FALSE)

# корректировка Коэнкера
# между шагом 1 и шагом 2 добавляется шаг 1.5
# u_hat^2 <- u_hat^2 / (1 - H_ii)
bptest(data = diamonds, 
       price ~ carat + x + y + z, 
       varformula = ~ carat)
# H0 о гомоскедастичности отвергается, 
# гетероскедастичность есть

# тест Голдфельда-Квандта
# подозреваем какую-ту переменную во влиянии на Var(u|X)
# сортируем наблюдения по этой переменной
# выкидываем 20% наблюдений посередине
# оцениваем исходную модель y = beta_1 + beta_2 x + u
# По первой части наблюдений. Получаем RSS_1
# По второй части наблюдений. Получаем RSS_2
# H0: гомоскедастичность, sigma^2_2 = sigma^2_1
# GQ = (RSS_1 / (n_1 - k) ) / (RSS_2 / (n_2 - k))
# GQ ~ F_{n_1 - k, n_2 - k}
# где k — число коэффициентов
gqtest(data = diamonds, 
       order.by = ~ carat,
       fraction = 0.2,
       price ~ carat + x + y + z)
# H0 о гомоскедастичности отвергается, 
# гетероскедастичность есть
