library(tidyverse)

results = tibble(
  n_obs = seq(from = 10000, to = 50000, by = 10000))

results

results

results = mutate(results, ln_n = log(n_obs))
results = mutate(results, ln_n = map(n_obs, ~ log(.)))

res2 = mutate(results, data = map(n_obs, ~ head(diamonds, .)))
res2

res3 = mutate(res2, 
              model = map(data, ~ lm(data = ., price ~ carat)))
res3

# строим одну модель 
model = lm(data = diamonds, price ~ carat)

# общий отчет
summary(model)
# второй коэффициент
coef(model)[2]
# остатки
resid(model)

res4 = mutate(res3, beta2 = map(model, ~ coef(.)[2]))
res4
res4 = mutate(res3, beta2 = map_dbl(model, ~ coef(.)[2]))
res4

# упражнение: добавим R^2 каждой модели

## сначала получим R^2 для конкретной модели:
report = summary(model)
report$r.squared

summary(model)$r.squared

## а теперь пачка R^2 в столбик:
res5 = mutate(res4, R2 = map_dbl(model, ~ summary(.)$r.squared))
res5

# упражнение:
# n_obs: 10000, 20000, ..., 50000
# formula: price ~ carat, price ~ carat + x + y + z
# R^2 = ?


exer = crossing(
      n_obs = seq(from = 10000, to = 50000, by = 10000),
      formula = c("price ~ carat", "price ~ carat + x + y + z"))
exer

# тренируемся на отдельно взятой модели

model = lm(data = head(diamonds, 10000), formula = "price ~ carat")
r2 = summary(model)$r.squared
r2


# то же самое только с помощью map для столбцов
exer2 = mutate(exer, 
        model = map2(n_obs, formula, 
          ~ lm(data = head(diamonds, .x), formula = .y)))
exer3 = mutate(exer2, 
        r2 = map_dbl(model, ~ summary(.)$r.squared))
exer3

# вариант с написанием функции
myfunc = function(n_obs, form) {
  model = lm(data = head(diamonds, n_obs), formula = form)
  r2 = summary(model)$r.squared
  return(r2)
}

myfunc(10000, "price ~ carat + x + z")

exer
exer_3bis = mutate(exer, 
          r2 = map2_dbl(n_obs, formula, myfunc))
exer_3bis


# временные ряды

# куча разных форматов
# ts, tsibble

library(tsibble) # работа с форматом tsibble
library(forecast) # самые распространённые одномерные модели
library(sophisthse) # импорт данных с sophist.hse.ru

all = series_info

unemp = sophisthse("UNEMPL_M")

str(unemp)

frequency(unemp)
start(unemp)

myts = ts(c(3, 4, 5, 6), start = c(2010, 3), frequency = 4)
myts

ggtsdisplay(unemp)

# оценим модель ARIMA(1, 0, 1) - (1, 0, 1)[12]
model_101_101 = Arima(unemp, order = c(1, 0, 1), seasonal = c(1, 0, 1))
model_101_101

prediction_101_101 = forecast(model_101_101, h = 24)
prediction_101_101
autoplot(prediction_101_101)

# модель ARIMA с автоматическим выбором количества параметров
model_arima = auto.arima(unemp)
model_arima
prediction_arima = forecast(model_arima, h = 24)
autoplot(prediction_arima)

# отбор наблюдений
unemp_2010 = window(unemp, start = c(2010, 1))
ggtsdisplay(unemp_2010)

# ETS - модель

forecast(unemp, h = 7) %>% autoplot()
model_ets = ets(unemp)
model_AAN = ets(unemp, model = "AAN")

forecast(model_arima, h = 3) %>% autoplot() 
forecast(model_ets, h = 3) %>% autoplot() 

# работа с датами

library(lubridate)


ymd("2012-9-17") %>% str()
mdy("9-17-2003")

df = tibble(x = runif(100), date = ymd("2010-01-12") + days(0:99))
df

df2 = as_tsibble(df, index = "date")
df2

df3 = filter(df2, date >= "2010-02-03")


