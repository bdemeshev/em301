library(forecast) # прогнозирование ARIMA, TBATS, тета
library(sophisthse) # скачивание рядов
library(imputeTS) # восстановление пропусков
library(rio) # импорт данных
library(tidyverse) # графики + манипуляции с табличками

# короткий ряд — можно попробовать тета-метод
wages <- sophisthse("WAG_Y") 
wages
y <- wages[, 2]
ggtsdisplay(y)

# наивное "такой же как вчера"
future_naive <- naive(y, h = 5)
autoplot(future_naive)

# https://robjhyndman.com/papers/Theta.pdf
# тета-метод
future_theta <- thetaf(y, h = 5)
autoplot(future_theta)

# авто-арима
model_a <- auto.arima(y)
future <- forecast(model_a, h = 5)
autoplot(future)
model_a

# длинный ряд со сложной сезонностью
# - варьируется длительность периода 
# например, число понедельников в году разное
# - накладываются волны разной частоты

gas <- import("https://robjhyndman.com/data/gasoline.csv")
gas # недельные данные (!)
ggtsdisplay(gas)

z <- ts(gas, frequency = 365/7)
model_z <- auto.arima(z) # дробная сезонность — auto.arima сработала

# https://robjhyndman.com/papers/ComplexSeasonality.pdf
# tbats
model_tbats <- tbats(z)
model_tbats

future_tbats <- forecast(model_tbats, h = 100)
autoplot(future_tbats)


# искусственно создадим ряд с дырками
wages <- sophisthse("WAG_M")
head(wages)
y <- wages[, 1]
ggtsdisplay(y)

# искусственные дыры
y_gap <- y
y_gap[150:170] <- NA
y_gap[220:230] <- NA
ggtsdisplay(y_gap)

# графики и описания для рядов с пропусками
statsNA(y_gap)
plotNA.distribution(y_gap)
plotNA.distributionBar(y_gap)
plotNA.gapsize(y_gap)

# методы заполнения пропусков: примитивные vs модели
### примитивные
# линейная интерполяция:
y_lin <- na.interpolation(y_gap)
plotNA.imputations(y_gap, y_lin)
plotNA.imputations(y_gap, y_lin, y)

# повторить последнее значение:
y_locf <- na.locf(y_gap)
plotNA.imputations(y_gap, y_locf)

### методы с моделированием ряда
# фильтр Калмана для модели класса ETS
y_kalman <- na.kalman(y_gap)
plotNA.imputations(y_gap, y_kalman)
plotNA.imputations(y_gap, y_kalman, y)

# фильтр Калмана для модели ARIMA
y_kalman_arima <- na.kalman(y_gap, 
          model = "auto.arima")
plotNA.imputations(y_gap, y_kalman_arima)

# удали сезонность — линейно аппроксимируй — верни сезонность
y_deseas_seas <- na.seadec(y_gap)
plotNA.imputations(y_gap, y_deseas_seas)

### (упр) удалить момент структурного сдвига
#
y_gap2 <- y
y_gap2[60:80] <- NA
plotNA.distribution(y_gap2)

# линейная аппроксимация
y_lin2 <- na.interpolation(y_gap2)
plotNA.imputations(y_gap2, y_lin2)

# фильтр Калмана
y_kalm2 <- na.kalman(y_gap2)
plotNA.imputations(y_gap2, y_kalm2)
plotNA.imputations(y_gap2, y_kalm2, y)

# желания:
# вопрос про график
# перебор моделей и табличка с aic

