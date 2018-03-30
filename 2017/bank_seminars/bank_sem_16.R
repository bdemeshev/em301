library(forecast) # ARIMA, ETS, TBATS
library(sophisthse) # загрузка рос макро рядов
library(urca) # тестирование стационарности (есть ли единичный корень)
library(readxl)
library(tidyverse) # графики + манипуляции с данными

y_ar1 <- arima.sim(n = 200,
  model = list(ar = 0.8))

ggtsdisplay(y_ar1)

# смотрим на сами коэффициенты:
Pacf(y_ar1, plot = FALSE)

# оценим AR(1)
model_ar1 <- Arima(y_ar1, order = c(1, 0, 0))
summary(model_ar1)
model_ar1_wrong <- Arima(y_ar1, order = c(3, 0, 1))
summary(model_ar1_wrong)

# построение прогнозов
# горизонт прогнозирования = 8 (на 1, 2, 3 и 4 шага)
future <- forecast(model_ar1, h = 8)
future
autoplot(future)

# теперь AR2
y_ar2 <- arima.sim(n = 200,
    model = list(ar = c(0.6, -0.08)))
y_ar2
ggtsdisplay(y_ar2)

# теперь MA1
y_ma1 <- arima.sim(n = 200,
                   model = list(ma = 0.9))

ggtsdisplay(y_ma1)


