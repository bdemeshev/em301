library(rio) # загрузка данных
library(tidyverse) # обработка данных
library(forecast) # прогнозирование
library(stringr) # работа со строками

# при кракозябрах в консоли на винде
Sys.setenv(language = "russian")

m0 = import("~/Downloads/marriage_data_raw.xls")
glimpse(m0) #
head(m0)
View(m0)

colnames(m0) = c("region", "unit", "period", 2006:2018)
m1 = tail(m0, -2) # долой первые две строки
tail(m1)

m2 = filter(m1, !str_detect(period, "-"))

qplot(data = m2, x = unit)
m3 = select(m2, -unit)

month_names = unique(m3$period)
month_names

coder = tibble(period = month_names, month = 1:12)
coder

m4 = left_join(m3, coder, by = "period")
glimpse(m4)

m5 = select(m4, -period)
glimpse(m5)

m6 = gather(m5, key = "year", value = "marriage", -region, -month)
glimpse(m6)

m7 = filter(m6, str_detect(region, "Россия"))
m8 = arrange(m7, year, month)
m8

m9 = mutate(m8, marriage = as.numeric(marriage))

y = ts(m9$marriage, start = c(2006, 1), frequency = 12)
y

ggseasonplot(y)
ggtsdisplay(y)

model_aaa = ets(y, model = "AAA", damped = FALSE)
autoplot(model_aaa)

y_fcst = forecast(model_aaa, h = 24)
autoplot(y_fcst)
y_fcst




library(rio)
library(tidyverse)

rosstat_to_tibble = function(rosstat_data, var_name = "marriage") {
  
  m0 = rosstat_data
  
  # Хак 2 для случая кракозябр на винде
  # library(readxl)
  # m0 = read_excel("....")
  
  
  #tail(m0)
  #head(m0)
  #glimpse(m0)
  #View(m0) # потом не забудьте закрыть табличку :)
  
  # берём хвостик таблицы без первых двух строк
  m1 = tail(m0, -2)
  
  # задаём имена
  colnames(m1) = c("region", "unit", "period", 2006:(ncol(m1) + 2002))
  # glimpse(m1)
  
  # по гистограмме смотрим осмесленность переменной unit
  # qplot(data = m1, x = unit)
  
  # убираем переменную unit
  m2 = dplyr::select(m1, -unit)
  # glimpse(m2)
  
  # отбираем строки где period не содержит -
  m3 = filter(m2, !str_detect(period, "-"))
  # glimpse(m3)
  
  # выделяем имена месяцев с шифровками инопланетян в отдельный вектор
  strange_names = unique(m3$period)
  # strange_names
  
  # создаём таблицу перекодировщик странных имен в номера месяцев
  coder = tibble(period = strange_names, month = 1:12)
  # coder
  
  # левый джойн: к исходной таблице приклеиваем таблицу-кодировщик
  m4 = left_join(m3, coder, by = "period")
  # glimpse(m4)
  
  # удаляем переменную period
  m5 = dplyr::select(m4, -period)
  #glimpse(m5)
  
  # собираем столбцы 2006:2018 в столбец marriage
  # это перевод широкой таблицы в длинную
  m6 = gather(m5, key = "year", value = "marriage", -region, -month)
  # glimpse(m6)
  
  # задаём верный тип переменных
  m7 = mutate(m6, year = as.numeric(year),
              marriage = as.numeric(marriage))
  colnames(m7)[4] = var_name
  return(m7)
}


marr = import("~/Downloads/marriage_data_raw.xls")
marr_tbl = rosstat_to_tibble(marr)

birth = import("~/Downloads/data_birth_raw.xls")
birth_tbl = rosstat_to_tibble(birth, "birth")

marr_tbl %>% glimpse()
birth_tbl %>% glimpse()


all = full_join(marr_tbl, birth_tbl, by = c("region", "month", "year"))
all %>% glimpse()