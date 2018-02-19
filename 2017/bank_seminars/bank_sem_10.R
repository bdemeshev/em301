library(tidyverse) # манипуляции с данными + универсальные графики
library(corrplot) # графики для корреляционных матриц
library(skimr) # описательные статистики
library(vcd) # графики для качественных переменных
library(HSAUR) # набор данных по многоборью 
library(forecast) # прогнозирование одномерных врем рядов

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
