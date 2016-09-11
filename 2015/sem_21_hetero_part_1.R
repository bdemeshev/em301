library("sandwich")
library("lmtest")
library("dplyr")
library("ggplot2")

# данные сохраняем со ссылки:
# goo.gl/zxwS7u

# setwd(".....")
# read.table("flats_moscow.txt")

name <- "~/Downloads/flats_moscow.txt"
# name <- file.choose()

flats <- read.table(name,
  header = TRUE, sep = "\t", dec = ".")

glimpse(flats)

model_0 <- lm(data = flats,
          price ~ totsp + livesp)
model_1 <- lm(data = flats,
  price ~ totsp + livesp +
    walk + metrdist)

qplot(data = flats, x = totsp, y = price)
vcov(model_0) # оценка ковариационной матрицы
vcovHC(model_0) # оценка устойчивая к ГСК

waldtest(model_0, model_1) # неправильный тест Вальда! мы забыли про гетероскедастичность
waldtest(model_0, model_1,
         vcov = vcovHC) # формула устойчивая к ГСК

coeftest(model_0) # неправильные стандартные ошибки
coeftest(model_0, vcov. = vcovHC)

confint(model_0) # неправильный довер. интервал

test_table <- coeftest(model_0,
                       vcov. = vcovHC)
test_table
z_cr <- qnorm(0.975)
confint_table <- data_frame(
  beta_hat = test_table[, 1],
  se_HC = test_table[, 2],
  left = beta_hat - z_cr * se_HC,
  right = beta_hat + z_cr * se_HC
)

confint_table # довер интервал устойчивый к ГСК
confint(model_0)

