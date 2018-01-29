library(tidyverse)
library(rio)
library(lmtest)

# данные goo.gl/4JQ9kM

# если я не знаю, где лежит файл
file.choose()
# будьте осторожны! 
# эта команда открывает окошко для выбора файла 
# и окошко нужно закрыть, чтобы работать дальше

# если примерно знаю, где файл, то интуиция + таб
flats <- import("~/Downloads/flats_moscow.txt")

# два порядка выполнения действий
log(cos(1))
1 %>% cos %>% log

# верхние три наблюдения из таблицы
flats %>% head(3)
head(flats, 3)

# три самые дорогие квартиры
flats %>% arrange(-price) %>% head(3)
head(arrange(flats, -price), 3)

# у каждого округа москвы считаем 
# среднюю цену квартир и разброс цен
okrug <- flats %>% group_by(code) %>% 
  summarise(av_price = mean(price), razbros = var(price)) 

# посмотрим на табличку okrug
okrug

# сортируем табличку с округами и смотрим верхние три наблюдения
okrug %>% arrange(-razbros) %>% head(3)

# предыдущие действия без создания промежуточной таблицы okrug
flats %>% group_by(code) %>% 
  summarise(av_price = mean(price), 
            razbros = var(price)) %>% 
  arrange(-razbros) %>% head(3)

# пачка регрессий!
regs <- flats %>% group_by(code) %>%
       do(model = lm(data = ., price ~ livesp + kitsp))
# точка означает подтабличку;
# поскольку мы нарезали большую табличку на подтаблички 

# табличка со всеми регрессиями
regs

# из таблички regs извлекаем столбец model и из него модель номер 5
regs$model[[5]]

# отчёт по модели 5
summary(regs$model[[5]])


# сохраним табличку с округами в формате экселя
export(okrug, "~/Downloads/okrug.xlsx")

# встроенный набор данных по бриллиантам
d <- diamonds
glimpse(d)

# домашнее задание:
# 1. сгруппируйте данные по бриллиантам по интесивности цвета (переменная color)
# 2. для каждой интенсивности цвета:
# посчитайте среднюю цену одного карата и назовите её av_carat_price
# посчитайте максимальную цену одного карата и назовите её max_carat_price
# 3. результирующую табличку назовите color_summary
# 4. отсортируйте табличку color_summary по max_carat_price
# 5. назовите интенсивность цвета бриллианта, 
# у которого самая высокая цена одного карата


