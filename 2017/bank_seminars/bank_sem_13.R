library(tidyverse) # манипуляции с данными
library(rpart) # построение деревьев
library(rpart.plot) # визуализация деревьев
library(ranger) # случайный лес

# набор данных про бриллианты
glimpse(diamonds)

# дерево для количественной переменной
tree_price <- rpart(data = diamonds, 
      price ~ cut + carat + x + y + z)
tree_price
rpart.plot(tree_price)

# дерево для категориальной переменной
# гистограмма для качества огранки:
qplot(data = diamonds, x = cut)
tree_cut <- rpart(data = diamonds, 
          cut ~ carat + price)
tree_cut
rpart.plot(tree_cut, type = 4)

new_diamonds <- tibble(
  price = c(1000, 2000),
  carat = c(0.5, 3))
new_diamonds
predict(tree_cut, newdata = new_diamonds)

# активируем встроенный набор данных 
# по пассажирам Титаника
data("ptitanic")
str(ptitanic)

tree_survived <- rpart(survived ~ ., 
                       data = ptitanic)
rpart.plot(tree_survived, type = 0)

# случайный лес
forest_price <- ranger(data = diamonds,  
    price ~ cut + carat + x + y + z)
forest_price

# сравним по прогнозной силе 
# случайный лес и дерево
glimpse(diamonds)

# генерируем случайно номера строк, 
# которые войдут в обучающую выборку
set.seed(777)
train_index <- sample(nrow(diamonds), 40000)
train_index
# делим выборку на две части
diam_train <- diamonds[train_index, ]
diam_test <- diamonds[-train_index, ]
# оцениваем две модели
tree_model <- rpart(data = diam_train, 
      price ~ cut + carat + x + y + z)
forest_model <- ranger(data = diam_train, 
      price ~ cut + carat + x + y + z)
# получаем прогнозы от дерева и леса
diam_test2 <- diam_test %>% mutate(
  tree_fcst = predict(tree_model, newdata = diam_test),
  forest_fcst = predictions(predict(forest_model, diam_test)))

# отбираем переменные и смотрим корреляционную матрицу
cor_matrix <- dplyr::select(diam_test2, 
              price, tree_fcst, forest_fcst) %>% cor()
cor_matrix


dplyr::select(diam_test2, 
              price, tree_fcst, forest_fcst)
