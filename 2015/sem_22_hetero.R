library("dplyr")
library("ggplot2")
library("sandwich")
library("lmtest")

flats <- read.table(
  "~/Downloads/flats_moscow.txt",
  header = TRUE, sep = "\t", dec = ".")
glimpse(flats)

model_0 <- lm(price ~ totsp + livesp,
              data = flats)
summary(model_0)

### тест Бройша-Пагана

# те же регрессоры в уравнении для
# условной дисперсии, что и в исходном уравнении
bptest(model_0)

# произвольные регрессоры в уравнении
# условной дисперсии
bptest(model_0,
       varformula = ~ brick + totsp,
       data = flats)

# тест Уайта: в уравнение для дисперсии
# включаются те же переменные, что и в исходное
# плюс их квадраты
bptest(model_0,
       varformula = ~ totsp + I(totsp^2) +
                      livesp + I(livesp^2),
       data = flats)


# тест Голдфельда-Квандта
help(gqtest)

# упорядочиваем данные по totsp
# выкидываем 20% наблюдений посередине
gqtest(model_0, order.by = ~ totsp,
       fraction = 0.2, data = flats)
