library(forecast)
library(sophisthse)
library(tidyverse)

wages_y <- sophisthse("WAG_Y")
wages_m <- sophisthse("WAG_M")
# коды рядов — sophist.hse.ru
# View(series_info)
View(series_info)

wages_y
y1 <- wages_y[, 2]
ggtsdisplay(y1)

model <- ets(y1)
summary(model)

future <- forecast(model, h = 5)
future

autoplot(future)

wages_m
y2 <- wages_m[, 1]
ggtsdisplay(y2)

log_y <- log(y2)
ggtsdisplay(log_y)

y_final <- window(log_y, start = c(1999, 1))
ggtsdisplay(y_final)

model_a <- ets(y_final)
summary(model_a)

# таксономия ETS моделей
# https://otexts.org/fpp2/ets.html

future <- forecast(model_a, h = 120)
autoplot(future)

# за кадром автоматически получилось разложение ряда
autoplot(model_a)
states <- model_a$states
View(states)

# ручной заказ модели по требованию
model_manual <- ets(y_final, model = "AAZ")
summary(model_manual)

model_manual <- ets(y_final, 
                    model = "AAZ", 
                    damped = TRUE)
summary(model_manual)
future_manual <- forecast(model_manual, h = 120)
autoplot(future_manual)

# встроены наивные варианты
future_naive <- snaive(y_final, h = 120)
autoplot(future_naive)

# усреднить несколько моделей
library(forecastHybrid)

model_hybrid <- hybridModel(y_final, 
          models = "ae",
          a.args = list(approximation = FALSE),
          e.args = list(model = "AAZ"))
summary(model_hybrid$auto.arima)
summary(model_hybrid$ets)

future_hybrid <- forecast(model_hybrid, h = 120)
autoplot(future_hybrid)
