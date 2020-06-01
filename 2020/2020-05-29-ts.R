# прогнозирование одномерных временных рядов :)
library(fable) # ETS, ARIMA
library(tidyverse) # манипуляции с данными
library(feasts) # визуализации временных рядов
library(lubridate) # работа с датами
library(tsibble) # удобный формат хранения рядов
library(rio) # чтение данных из xlsx, csv, dta, eviews...

AirPassengers
?AirPassengers

# R:
# первый: ts (для регулярных рядов: месячные, квартальные, годовые)
# зоопарк: zoo, xts...
# текущий: tsibble (дата | ключ | ряд 1 | ряд 2)

air = as_tsibble(AirPassengers) 
air

# график
gg_tsdisplay(air, value)

# 1. теоретическая AutoCorrelation Function
# определена только стационарных рядов
# ACF_k = Corr(y_t, y_{t-k})
# ACF_1 = Corr(y_5, y_4) = Corr(y_100, y_99) = ...
# ACF_2 = Corr(y_10, y_8) = Corr(y_20, y_18) = ...
# можно найти, если дан процесс 

# 2. выборочная (эмпирическая) ACF 
# можно посчитать для любого столбика чисел
# если столбик чисел порожден стац. процессом,
# то sample ACF_k - состоятельная оценка для ACF_k
# оценка коэффициента бета в регрессии y_t на константу и y_{t-k}
# sACF_12 = 0.75
# регрессии: \hat y_t = c + 0.75 y_{t-12}
# понятен ли смысл левого нижнего?

gg_season(air, value)
gg_subseries(air) 
gg_lag(air, value)

# оценим автоматические модели 
models = model(air, 
               ets_ann = ETS(value ~ error("A") + trend("N") + season("N")),
               ets_auto = ETS(value),
               ma_1 = ARIMA(value ~ pdq(0, 0, 1) + PDQ(0, 0, 0)),
               arima_auto = ARIMA(value))
# ETS(ANN)
models$ets_ann[[1]] %>% report()

# ETS(auto) = ETS(MAdM)
models$ets_auto[[1]] %>% report()

# MA(1)
models$ma_1[[1]] %>% report()
# y_t = 280.64 + u_t + 0.96 u_{t-1}

# ARIMA(auto) = ARIMA(2,1,1)(0,1,0)[12]
models$arima_auto[[1]] %>% report()
# Ly_t = y_{t-1}
# L^2 y_t = y_{t-2}
# каждая цифра: 2, 1, 1, 0, 1, 0 - множитель с лагами в уравнении
# 0 - нет
# 1, 2... - степень лагового полинома
# ARIMA(несезонные множители)(сезонные множители)
# arIma(*, 1, *)(*, *, *)
# (1 - L) в LHS
# arIma(*, *, *)(*, 1, *)
# (1 - L^12) в LHS
# ARima(2, *, *)(*, *, *)
# (1 - 0.596L - 0.2143L^2) в LHS
# ariMA(*, *, 1)(*, *, *)
# (1 - 0.9819L) в RHS
# выписываю уравнение:
# (1 - 0.596L - 0.2143L^2)z_t = (1 - 0.9819L) u_t
# z_t = (1 - L)(1 - L^12) y_t
# z_t - стационарный процесс

fcst = forecast(models, h = "2 years")
fcst %>% autoplot()
fcst

fcst_ets = filter(fcst, .model == "ets_auto")
fcst_ets %>% autoplot(air)

fcst_arima = filter(fcst, .model == "arima_auto")
fcst_arima %>% autoplot(air)

fcst_ann = filter(fcst, .model == "ets_ann")
fcst_ann %>% autoplot(air)



