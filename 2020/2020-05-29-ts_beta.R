library(tidyverse) # манипуляции с данными
library(fable) # ETS, ARIMA, ...
library(feasts) # графики рядов и характеристики
library(rio) # импортировать данные из csv, xlsx, dta, eviews, ...
library(tsibble) # формат хранения рядов
library(lubridate) # работа с датами

# tools - install packages ...

# часто возникает возня с преобразованием дат
# R
# древний формат ts: регулярные ряды (годовые, квартальные, месячные)
# зоопарк форматов: zoo, xts, ...
# сейчас формат tsibble
# индекс (дата) | ключи (регион, область) | ряд x | ряд y | ряд z

AirPassengers
?AirPassengers

air = as_tsibble(AirPassengers)
air

air2 = mutate(air, value2 = value^2, lag_value = lag(value))
# Ly_t = y_{t-1}
air2

air_train = filter(air, index < ymd("1958-01-01"))
air_test = filter(air, index >= ymd("1958-01-01"))
air_test

# графики:
gg_tsdisplay(air, value)
# ACF? = autocorrelation function
# 1. tACF (теоретическая)
# ACF_k = Corr(y_t, y_{t-k})
# ACF_1 = Corr(y_t, y_{t-1}) = Corr(y_5, y_4) = Corr(y_100, y_99) =...
# tACF можно посчитать только у стационарного процесса
# не по столбцу цифр, а по уравнениям

# 2. sACF (выборочная)
# считается по столбику цифр
# считается для любого столбца цифр
# sACF_1 - коэффициент бета в регрессии y_t на константу и y_{t-1}
# sACF_2 - коэффициент бета в регрессии y_t на константу и y_{t-2}

# sACF_12 = 0.75
# \hat y_t = const +  0.75  y_{t-12}
# \hat beta = sCov(y, x) / sVar(x) - не всегда корреляция (!)
# но! если у нас временной ряд:
# \hat beta = sCov(y_t, y_{t-1}) / sVar(y_{t-1}) = ... \approx sCorr(y_t, y_{t-1})
# если наш столбец цифр порожден стац рядом, то sCorr состоятельно оценивает настоящую Corr


# cran task view time series
# https://cran.r-project.org/web/views/TimeSeries.html
# все области
# https://cran.r-project.org/web/views/


# модели:
# ETS(ANN), ETS(auto), MA(1), ARMA(auto)
models = model(air_train, 
               ann = ETS(value ~ error('A') + trend('N') + season('N')),
               ets_a = ETS(value),
               ma1 = ARIMA(value ~ pdq(0, 0, 1) + PDQ(0, 0, 0)),
               arima_a = ARIMA(value))

# ETS: https://otexts.com/fpp3/ets.html

# ETS(ANN)
models$ann[[1]] %>% report()
# alpha, l_0, sigma^2

# ETS(auto) = ETS(MAdM)
models$ets_a[[1]] %>% report()

# ARIMA: https://otexts.com/fpp3/arima.html

# MA(1)
models$ma1[[1]] %>% report()
# y_t = 230.98 + u_t + 0.9767 u_{t-1}
# u_t ~ N(0; 2179)

models$arima_a[[1]] %>% report()
# Model: ARIMA(1,1,0)(0,1,0)[12] 
# ARIMA(несезонная)(сезонная)[частота]
# ARIMA(лево, лево, право) (лево, лево, право)
# ARima(1**)(***)
# (1 + 0.2411L)
# arIma(*1*)(***)
# (1 - L)
# sarIma(***)(*1*)
# (1 - L^12)
# итого:
# (1 + 0.2411L) z_t = u_t
# z_t = (1 - L)(1 - L^12) y_t
# z_t - стационарный

fcst = forecast(models, h = "3 years")
fcst_ann = filter(fcst, .model == "ann")
autoplot(fcst_ann, air)

fcst_ets_a = filter(fcst, .model == "ets_a")
autoplot(fcst_ets_a, air)

fcst_arima_a = filter(fcst, .model == "arima_a")
autoplot(fcst_arima_a, air)

accuracy(models, air_test) # ???

