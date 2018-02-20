library(tidyverse) # манипуляции с данными + универсальные графики
library(corrplot) # графики для корреляционных матриц
library(skimr) # описательные статистики
library(vcd) # графики для качественных переменных
library(HSAUR) # набор данных по многоборью 
library(forecast) # прогнозирование одномерных врем рядов
library(ggthemes) # разные темы для графиков

data("heptathlon")
skim(heptathlon)
skim(diamonds)

new_data <- tibble(x = c(5, 6.7, 8), 
                   y = c("Да", "Нет", "Да"))
skim(new_data)
new_data2 <- tibble(x = c(5, 6.7, 8), 
                   y = factor(c("Да", "Нет", "Да")))
skim(new_data2)

# корреляционная матрица
cor(heptathlon)
A <- cor(heptathlon)

class(A)
class(new_data)

# пример: графики для корреляционных матриц
corrplot(A)
corrplot(A, order = "hclust")
corrplot(A, order = "hclust", 
         addrect = 3)
A
corrplot.mixed(A, order = "hclust",
               upper = "ellipse")

# пример: график для временных рядов
x_vector <- rnorm(200, mean = 0, sd = 4)
x_vector
x <- ts(x_vector, frequency = 12, 
        start = c(2000, 3))
class(x)
class(x_vector)
x
ggtsdisplay(x)

# график для категориальной переменной
qplot(data = diamonds, x = cut)
glimpse(diamonds)

mosaic(data = diamonds, 
  ~ cut + color, shade = TRUE)

mosaic(data = diamonds, 
       ~ color + cut, shade = TRUE)

# улитку можно закручивать дальше
# mosaic(data = diamonds, 
#       ~ color + cut + clarity, 
#       shade = TRUE)

# универсальные 
# диаграмма рассеяния по-быстрому
qplot(data = diamonds, 
      x = carat, y = price)

# тот же график в стиле грамматики графиков
ggplot(data = diamonds) +
  geom_point(aes(x = carat, 
                 y = price, 
                 color = color))

gr_0 <- ggplot(data = diamonds) +
  geom_point(aes(x = carat, 
                 y = price, 
                 color = color))

gr_0 + xlab("Масса бриллианта (карат)")


gr_1 <- gr_0 + xlab("Масса бриллианта (карат)") +
  ylab("Цена (долларов)") + 
  ggtitle("Встроенный набор данных")
gr_1

# разные прибавки к графику 1
gr_1 + facet_wrap(~ cut)
gr_1 + ggtitle("Верное название!")
gr_1 + stat_smooth(aes(y = price, x = carat))
gr_1 + geom_point(aes(x = carat, 
                      y = price)) + 
  theme_economist()


ggplot(data = diamonds) + 
  geom_hex(aes(x = carat, y = price))

d2 <- mutate(diamonds, 
             ln_price = log(price), 
             ln_carat = log(carat))

ggplot(data = d2, aes(x = ln_carat, 
                      y = ln_price)) + 
  geom_point(alpha = 0.02) + 
  stat_smooth(method = "lm", se = TRUE) + 
  ylim(lims = c(6, 10)) + xlab("Ось х")
 
  
  