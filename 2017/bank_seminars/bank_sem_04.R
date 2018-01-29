library(tidyverse)
library(rio)
library(lmtest)

# данные goo.gl/4JQ9kM

# если я не знаю, где лежит файл
file.choose()

# если примерно знаю, где файл, то интуиция + таб
flats <- import("~/Downloads/flats_moscow.txt")

# посмотреть, загрузились ли данные
glimpse(flats)

# оцениваем ограниченную короткую модель
model_r <- lm(data = flats, price ~ livesp + kitsp)
summary(model_r)

# оцениваем неограниченную длинную модель
model_ur <- lm(data = flats, price ~ livesp + kitsp +
                 walk + dist + metrdist)
summary(model_ur)

rss_r <- deviance(model_r)
rss_r
n <- nrow(flats)
n
rss_ur <- deviance(model_ur)
k_ur <- 6
q <- 3
F_obs <- (rss_r - rss_ur) / q / (rss_ur / (n - k_ur))
F_obs

# готовая функция
waldtest(model_r, model_ur)
waldtest(model_ur, model_r)

# хочу сравнить одинаковы ли коэффициенты 
# в модели price ~ livesp + kitsp на
# двух подвыборках brick = 0 и brick = 1

flats2 <- mutate(flats, bls = brick * livesp,
                        bks = brick * kitsp)
glimpse(flats2)

model_2subset <- lm(data = flats2, 
      price ~ livesp + kitsp + brick + bls + bks)
waldtest(model_r, model_2subset)

model_2subset <- lm(data = flats, 
  price ~ livesp + kitsp + brick + brick:livesp + brick:kitsp)

model_junk <- lm(data = flats, price ~ livesp * kitsp)
summary(model_junk)


model_2subset <- lm(data = flats2, 
       price ~  brick * (livesp + kitsp))

model_4subset <- lm(data = flats2, 
      price ~ floor * brick * (livesp + kitsp))
summary(model_4subset)
waldtest(model_2subset, model_4subset)
