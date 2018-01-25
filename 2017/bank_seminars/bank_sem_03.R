library(tidyverse)
library(rio)
library(lmtest)

# если знаю, где лежит файл
flats <- import("C:/Users/student/Downloads/flats_moscow.txt")
flats <- import("D:\\flats_moscow.txt")

# если толком не знаю, где лежит файл
file.choose()
# полученную от file.choose() строку копируем внутрь команды import()

glimpse(flats)

qplot(data = flats, x = totsp, y = price)
qplot(data = flats, x = log(totsp), y = log(price))

model_a <- lm(data = flats, price ~ livesp + kitsp)
summary(model_a)

mean(flats$livesp)

new <- tibble(livesp = c(40, 50), kitsp = c(10, 10))
new


predict(model_a, newdata = new, interval = "confidence")
?predict

predict(model_a, newdata = new, interval = "prediction")

predict(model_a, newdata = new)
new

predict(model_a, newdata = new, interval = "prediction", level = 0.8)

args(predict.lm)

# переопределение существующих объектов
lm <- 7
lm + 6
lm <- cos
lm(0)

# добавление новой переменной
flats2 <- mutate(flats, ln_price = log(price), 
                 ln_totsp = log(totsp))