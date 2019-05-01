# if you see krakozyabry on windows then File - Reopen with encoding - UTF8 :)

library(rio) # импорт данных
library(tidyverse) # обработка данных
library(stringr) # работа с текстовыми переменными
library(forecast) # прогнозирование временных рядов

# windows. Хак 1 для превентивного удара по кракозябрам на винде
Sys.setenv(language = "russian")


m0 = import("~/Downloads/marriage_data_raw.xls")

# Хак 2 для случая кракозябр на винде
# library(readxl)
# m0 = read_excel("....")


tail(m0)
head(m0)
glimpse(m0)
View(m0) # потом не забудьте закрыть табличку :)

# берём хвостик таблицы без первых двух строк
m1 = tail(m0, -2)

# задаём имена
colnames(m1) = c("region", "unit", "period", 2006:2018)
glimpse(m1)

# по гистограмме смотрим осмесленность переменной unit
qplot(data = m1, x = unit)

# убираем переменную unit
m2 = select(m1, -unit)
glimpse(m2)

# отбираем строки где period не содержит -
m3 = filter(m2, !str_detect(period, "-"))
glimpse(m3)

# выделяем имена месяцев с шифровками инопланетян в отдельный вектор
strange_names = unique(m3$period)
strange_names

# создаём таблицу перекодировщик странных имен в номера месяцев
coder = tibble(period = strange_names, month = 1:12)
coder

# левый джойн: к исходной таблице приклеиваем таблицу-кодировщик
m4 = left_join(m3, coder, by = "period")
glimpse(m4)

# удаляем переменную period
m5 = select(m4, -period)
glimpse(m5)

# собираем столбцы 2006:2018 в столбец marriage
# это перевод широкой таблицы в длинную
m6 = gather(m5, key = "year", value = "marriage", -region, -month)
glimpse(m6)

# задаём верный тип переменных
m7 = mutate(m6, year = as.numeric(year),
                marriage = as.numeric(marriage))
glimpse(m7)

# какие у нас бывают регионы?
unique(m7$region)

# отбираем Архангельскую область.
m8 = filter(m7, region == "11000000000 Архангельская область")

# сортируем по году и месяцу
m9 = arrange(m8, year, month)
m9

# отбираем столбец с данными в отдельный вектор
# и объявляем, что это регулярный временной ряд (класс ts)
# со стартом в 2006 году, 1-м месяце и периодичностью 12
y = ts(m9$marriage, start = c(2006, 1), frequency = 12)

ggseasonplot(y)
# В мае — маяться?

ggtsdisplay(y)
# внизу слева: парные регрессии y_t на конст и y_{t-k}
# внизу справа: множественные регрессии y_t на конст, y_{t-1}, y_{t-2}, ..., y_{t-k}
# в обоих случаях на графике коэффициент при y_{t-k}

# оцениваем ets(AAA)
model_aaa = ets(y, model = "AAA", damped = FALSE)
# полный список вариантов ETS
# глава 7.5 в Forecasting Principles and Practice
# https://otexts.com/fpp2/
# легко и быстро гуглится по fpp2

# графики долгосрочного уровня, наклона долгосрочного уровня и сезонности
autoplot(model_aaa)
# l_t, b_t, s_t
model_aaa$states %>% head()

# прогноз на 24 шага вперёд
prognoz = forecast(model_aaa, h = 24)
prognoz
autoplot(prognoz)


# можно глянуть на внутривыборочные ошибки прогнозов
checkresiduals(model_aaa)
# вывод: ошибки прогнозов ещё связаны между собой, значит можно усовершенствовать модель!


