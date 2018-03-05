library(tidyverse)
library(lmtest)
library(sandwich)

# Tools - Install packages ...
glimpse(diamonds)
qplot(data = diamonds, 
      x = carat,
      y = price)

qplot(data = diamonds, 
      x = log(carat),
      y = log(price))

model_a <- lm(data = diamonds,
      price ~ carat + x + y + z)
coeftest(model_a) # тесты на незначимость коэффициентов
# в предположении гомоскедастичности

coeftest(model_a, vcov. = vcovHC) # тесты на незначимость коэффициентов
# в предположении гетероскедастичности
# по умолчанию HC3

?vcovHC
# хочу как в стате
coeftest(model_a, 
  vcov. = vcovHC(model_a, type = "HC1"))


model_b <- lm(data = diamonds, price ~ carat)

# гомоскедастичный F-тест (для малых выборок)
waldtest(model_b, model_a, test = "F")
# гомескедастичный тест Вальда: (n -> oo)
waldtest(model_b, model_a, test = "Chisq")

# гетероскедастичный тест Вальда: (n -> oo)
# корректировка HC3
waldtest(model_b, model_a, vcov = vcovHC,  
         test = "Chisq")

# спецификация в логарифмах
model_ln_a <- lm(data = diamonds,
  log(price) ~ log(carat) + 
    log(1 + x) + log(1 + y) + log(1 + z))

coeftest(model_ln_a, vcov. = vcovHC) # тесты на незначимость коэффициентов
# в предположении гетероскедастичности, HC3
# хочу HC1 как в стате
coeftest(model_ln_a, 
         vcov. = vcovHC(model_ln_a, type = "HC1"))

# тест почти White :) (с корректировкой Koenker)
bptest(data = diamonds, 
       price ~ carat + x + y + z, 
       varformula = ~ carat * x * y * z)
# H0: гомоскедастичность 

# без корректировки Koenker
bptest(data = diamonds, studentize = FALSE, 
       price ~ carat + x + y + z, 
       varformula = ~ carat * x * y * z)

# тест Goldfeld-Quandt
# H0: гомоскедастичность
# Ha: Var(u_i|X) = h(carat_i), dh/dcarat > 0
gqtest(data = diamonds, 
       fraction = 0.2, 
       price ~ carat + x + y + z, 
       order.by = ~ carat)
# p_value < 5%, следовательно, H0 отвергается



# лирическое отступление про человекочитабельный вывод 
# результатов оценивания/тестирования
library(CausalImpact)

?CausalImpact
set.seed(1)
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 52)
y <- 1.2 * x1 + rnorm(52)
y[41:52] <- y[41:52] + 10
data <- cbind(y, x1)
pre.period <- c(1, 40)
post.period <- c(41, 52)
impact <- CausalImpact(data, pre.period, post.period)
summary(impact, "report")
# конец лирического отступлений :)


