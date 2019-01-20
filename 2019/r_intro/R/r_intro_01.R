factorial(10)

library(tidyverse)
library(rio)
library(estimatr)

stats::filter() # полный синтаксис с именем пакета

# абсолютный путь
# если не знаешь, то file.choose()
my_data = import("/home/boris/Documents/em301/2019/r_intro/data/demo.xlsx")

# session - set working directory - to source file location
# относительный путь
my_data = import("../data/demo.xlsx")

# вместо = можно писать <- 
# my_data <- ...

export(my_data, "../data/demo_2.xlsx")

# вектора, таблички и произвольные списки

hi = c(5, 6, 7, NA, -3, 8)
hi
hi + 12

hi[1]
hi[2:4]

foo = tibble(x = 1:10, y = runif(10, min = 0, max = 4), z = x + y)
foo

# отбор строк по критерию
foo2 = filter(foo, x > 3, y > mean(y))

# быстрый взгляд на табличку
glimpse(foo)

# описательные статистики
library(skimr)
skim(foo)

# создание новых новых переменных / модификация старых
foo3 = mutate(foo, w = log(x), w2 = w^2)
skim(foo3)

foo4 = mutate(foo, `the age` = x)
foo4

# переименование столбцов
foo4_good = rename(foo4, the_age = `the age`)
foo4_good

# сортировка
foo_sorted = arrange(foo, -y)
foo_sorted

# голова и хвост таблички
head(foo, 4)
tail(foo, 3)

# комбинирование нескольких действий
sqrt(cos(pi/3))
(pi/3) %>% cos %>% sqrt

# комбо: 
# создадим переменную co = cos(x)
# отсортируем по co
# оставим верхние 4 наблюдения

# синтаксис Эйлера-Клеро
foo_x = head(arrange(mutate(foo, co = cos(x)), -co), 4)
# синтаксис Магритта
foo_x = foo %>% mutate(co = cos(x)) %>% arrange(-co) %>% head(4)

glimpse(diamonds)
# R: split - apply - combine strategy
# excel: pivot table

report = diamonds %>% group_by(cut) %>%
     summarise(mean_price = mean(price), mean_weight = mean(carat)) 
report

# что делает эта команда?
diam2 = diamonds %>% group_by(cut) %>% mutate(x = price - mean(price))

# верхние три строки в каждой нарезке:
diamonds %>% group_by(cut) %>% top_n(3)


sol = diamonds %>% mutate(ppc = price / carat) %>%
           group_by(clarity) %>% arrange(-ppc) %>% top_n(1)
sol = diamonds %>% mutate(ppc = price / carat) %>%
  group_by(clarity) %>% top_n(1, ppc) %>% ungroup()


# произвольные списки
bar = list(x = 7, y = diamonds, z = c(5, 6, 7))
bar[[3]]
bar[["z"]]
bar$z

# множественная регрессия
model_0 = lm(data = diamonds, price ~ carat + x + y + z)
summary(model_0)

# регрессия с робастными к гетероскедастичности ст. ош.
model_hc = lm_robust(data = diamonds, price ~ carat + x + y + z)
summary(model_hc)

# немного про графики
qplot(data = diamonds, x = carat, y = price, color = cut)


base = qplot(data = diamonds, x = carat, y = price, color = cut)

base + labs(x = "Масса бриллианта в каратах", 
            y = "Цена в долларах", title = "Симпатяшка")

# домашка:
# зарегаться на datacamp.com
# внести datacamp-мейл на tiny.cc/7hpb2y
# пройти курсы
# Intro to tidyverse
# Communicating with data in tidyverse
# дедлайн: 10 февраля 2019, 20:35


# циклы (чтобы понять чужой код!!!)

for (i in 1:20) {
  print(i^2)
}

# как правильно :)

# функцию сделаем :)

foo_fun = function(x) {
  y = x^2
  z = y + x
  return(z)
}

foo_fun(11) 

# посчитайте коэффициент beta_2 c крышкой в регресии цены 
# бриллианта на массу по 10000, 20000, 30000, 40000, 50000
# наблюдениям

results = tibble(n_obs = seq(from = 10000, to = 50000, by = 10000))
results


head(diamonds, 4)

res2 = mutate(results, data = map(n_obs, ~ head(diamonds, .)))
res2

res3 = mutate(res2, model = map(data, ~ lm(data = ., price ~ carat)))

models = res3 %>% pull(model)
models[[3]]

# дз: извлеките бета 2 с крышкой