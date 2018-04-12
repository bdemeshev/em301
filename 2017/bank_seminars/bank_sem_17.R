library(forecast) # прогнозирование рядов
library(sophisthse) # автоматическая загрузка
library(tidyverse) # манипуляции с данными


# откуда ряды?
# sophist.hse.ru

# полный список доступных рядов
View(series_info)

wages <- sophisthse("WAG_M")
wages # три ряда

y <- wages[, 1]
ggtsdisplay(y)

# судя по графику логарифм может быть полезен
log_y <- log(y)
ggtsdisplay(log_y)

# отрежем слишком древние данные
w <- window(log_y, start = c(1999, 1))
ggtsdisplay(w)

# есть сезонность
# есть единичный корень 
# (слишком медленно убывает ACF)
model_auto <- auto.arima(w)
model_auto

# не оценились стандартные ошибки
# откажемся от примерного метода макс. правдоподобия

model_auto <- auto.arima(w, approximation = FALSE)
model_auto

future <- forecast(model_auto, h = 24)
future

autoplot(future)

# пример ряда, где auto.arima проваливается
constr <- sophisthse("CONSTR_M_NAT")
y <- constr[, 1]
ggtsdisplay(y, lag.max = 50)

model_2 <- auto.arima(y)
model_2

future_2 <- forecast(model_2, h = 24)
autoplot(future_2)

# переходим на ручное управление:
# обычного единичного корня нет
# сезонный единичный корень есть
# довешиваем ARMA(1, 1) + SARMA(1, 1)
model_manual <- Arima(y, 
  order = c(1, 0, 1),
  seasonal = list(order = c(1, 1, 1)))
model_manual

future_3 <- forecast(model_manual, h = 24)
autoplot(future_3)
plot(future_3)

# сезонная декомпозиция, STL
w_stl <- stl(w, s.window = "periodic")
plot(w_stl)
autoplot(w_stl)
w_stl
# что за столбики справа от компонент?

View(rusmaps.dataframe)

info <- rus_fd@data

rus_fd_table <- fortify(rus_fd, region = "name")
glimpse(rus_fd_table)


rus_final <- left_join(rus_fd_table, info, 
                       by = c(id = "name"))
glimpse(rus_final)
skim(rus_final)


base <- ggplot(rus_final) + 
  geom_polygon(aes(x = long, y = lat, 
              fill = pop_2016, group = group), color = "white")

base

base + coord_quickmap()

base + coord_quickmap() + 
  labs(x = "", y = "", fill = "Население 2016") +
  ggtitle("Население 2016")

base + coord_quickmap() + 
  theme(legend.position = "none") + 
  labs(x = "", y = "") +
  ggtitle("Население 2016")

