library(forecast)
library(prophet)
library(imputeTS)
library(tidyverse)
library(forecastHybrid)
library(purrr)
library(lubridate)

cpi0 <- read_csv("~/Downloads/cpi_weekly.csv")

glimpse(cpi0)
class(cpi0)

colnames(cpi0) <- c("date", "actual")
glimpse(cpi0)

ymd("2008-08-06") # 2008-08-06 как дата
ymd("2008-08-06") + weeks(7) 
ymd("2008-08-06") + weeks(0:7) 

clean_cpi <- tibble(date = 
          ymd("2008-08-06") + weeks(0:(nrow(cpi0) - 1)))
glimpse(cpi0)
glimpse(clean_cpi)

# первая попытка склеить таблички
clean_cpi <- left_join(clean_cpi, cpi0, by = "date")
# облом так как дата в табличках в разных форматах
# формат dttm (дата + время)
# формат date (дата)

# пробуем перевести 
a <- cpi0$date[1]
class(a)
as.Date(a) %>% class() 

# переводим один формат дат в другой
cpi0 <- mutate(cpi0, date2 = as.Date(date))
glimpse(cpi0)

# попытка склейки 2
clean_cpi <- left_join(clean_cpi, cpi0, 
                       by = c(`date` = "date2"))
glimpse(clean_cpi)


# единица измерения = 1 год
# циклов внутри года = 365.25/7
yday(min(clean_cpi$date)) # день от начала года

cpi_ts <- ts(data = clean_cpi$actual, 
             frequency = 365.25/7,
             start = 2008 + yday(min(clean_cpi$date))/365)
cpi_ts


# работаем с рядом :)
statsNA(cpi_ts)
plotNA.distribution(cpi_ts)

# заполнили пропуски
cpi_ts_filled <- na.interpolation(cpi_ts)
plotNA.distribution(cpi_ts_filled)

# визуализация
ggtsdisplay(cpi_ts_filled)

# наивные прогнозы

# для конкретной подвыборки
cpi_pre2017 <- window(cpi_ts_filled, end = 2016.999)

naive(cpi_pre2017, h = 1)
snaive(cpi_pre2017, h = 1)
# сезонный наивный прогноз не сработал так как
# частота ряда была дробная и 
# взять дробное количество недель назад функция не смогла

week(min(clean_cpi$date))

cpi_ts_filled2 <- ts(cpi_ts_filled, 
              freq = 52,
              start = c(2008, week(min(clean_cpi$date))))

# для конкретной подвыборки
cpi2_pre2017 <- window(cpi_ts_filled2, end = c(2016, 52))

naive(cpi2_pre2017, h = 1)
snaive(cpi2_pre2017, h = 1)

autoplot(f_naive0)

# смотрим, что там внутри списка с прогнозами
f_naive0 <- naive(cpi2_pre2017, h = 10)
str(f_naive0)
f_naive0[["mean"]]



# ошибки прогнозов растущим окном
e_naive <- tsCV(cpi_ts_filled2, naive, h = 1)
e_snaive <- tsCV(cpi_ts_filled2, snaive, h = 1)

ggtsdisplay(e_naive)
ggtsdisplay(e_snaive)

# пробуем ARIMA ручную
# ARMA(1, 1)-SARMA(1,1)

model_11_11 <- Arima(cpi2_pre2017, 
                     order = c(1, 0, 1),
                      seasonal = c(1, 0, 1))
forecast(model_11_11, h = 10)

# упакуем нашу модель в одну функцию
farima_11_11 <- function(y, h = 1) {
  model <- Arima(y, 
                 order = c(1, 0, 1),
                 seasonal = c(1, 0, 1))
  future_y <- forecast(model, h = h)
  return(future_y)
}

farima_11_11(cpi2_pre2017, h = 2)

# здесь можно принять ванну и выпить чашечку кофе!
e_farima_11_11 <- tsCV(cpi_ts_filled2, 
                       farima_11_11, h = 1)

xfourier <- fourier(cpi_ts_filled, K = 2)
ggtsdisplay(xfourier[, 1])
ggtsdisplay(xfourier[, 2])
ggtsdisplay(xfourier[, 3])
ggtsdisplay(xfourier[, 4])
yf <- apply(xfourier, MARGIN = 1, sum)
ggtsdisplay(yf)
ggtsdisplay(cpi_ts_filled)

# прогноз с помощью arima
# с регрессорами в виде рядов Фурье
farima_fourier <- function(y, h = 1) {
  model <- Arima(y, 
                 order = c(1, 0, 0),
                 xreg = fourier(y, K = 10),
                 seasonal = c(0, 0, 0))
  future_y <- forecast(model, 
              xreg = fourier(y, K = 10, h = h), 
              h = h)
  return(future_y)
}

farima_fourier(cpi_pre2017, h = 1)
e_farima_fourier <- tsCV(cpi_ts_filled, 
                       farima_fourier, h = 1)

ets_auto <- function(y, h = 1) {
  model <- ets(y)
  future_y <- forecast(model, h = h)
  return(future_y)
}

ets_auto(cpi2_pre2017, h = 1)

e_ets_auto <- tsCV(cpi_ts_filled2, 
                   ets_auto, h = 1)

tbats_auto <- function(y, h = 1) {
  model <- tbats(y)
  future_y <- forecast(model, h = h)
  return(future_y)
}

e_tbats_auto <- tsCV(cpi_ts_filled, 
                   tbats_auto, h = 1)


# руками усреднили две модели
e_naive_snaive <- (e_naive + e_snaive) / 2

# автоматическое усреднение 6 или меньше моделей
?hybridModel()

all_errors <- tibble(naive = coredata(e_naive),
                     snaive = coredata(e_snaive),
                     arima_fourier = coredata(e_farima_fourier),
                     date = clean_cpi$date)
MSE <- function(u) {
  return(mean(u^2, na.rm = TRUE))
} 

all_errors <- mutate(all_errors, 
        snaive_fourier = (arima_fourier + snaive) / 2)

errors_subset <- all_errors %>% filter(date > ymd("2017-01-01")) 

MSE(errors_subset$naive)
MSE(errors_subset$snaive)
MSE(errors_subset$arima_fourier)
MSE(errors_subset$snaive_fourier)



