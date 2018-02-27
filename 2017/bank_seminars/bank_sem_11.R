library(tidyverse) # манипуляции с данными и грамматика графиков
library(lmtest) # тесты для линейных моделей
library(sandwich) # скорректированные оценки для гетероскедастичности
library(broom) # приведение всех моделей в таблички

# предполагаем гомоскедастичные ошибки u_i
# Var(u_i|X) = const

glimpse(swiss)
?swiss

model <- lm(data = swiss, 
    Infant.Mortality ~ Agriculture + Catholic + Education)
summary(model) # общий отчёт по модели


coeftest(model) # отдельно табличка с коэффициентами
tidy(model) # отдельно табличка с коэффициентами

# скоррекируем se(b_hat) на возможную гетероскедастичность
coeftest(model, vcov. = vcovHC)

model_ur <- lm(data = swiss, 
  Infant.Mortality ~ Agriculture + Catholic + Education + Fertility)

# сравнение двух моделей через F тест в предположении гомоскедастичности
waldtest(model, model_ur)
# H_0 верна короткая модель
# Р-значение = 0.007 < alpha = 0.05
# H_0 отвергается

waldtest(model, model_ur, vcov = vcovHC)
# H_0 верна короткая модель
# Р-значение = 0.02 < alpha = 0.05
# H_0 отвергается


# визуальный тест на гетероскедастичность
qplot(data = diamonds, 
      y = price, x = carat)
qplot(data = diamonds, 
      y = price, x = carat, geom = "hex")



qplot(data = swiss, 
      y = Infant.Mortality, x = Education)
