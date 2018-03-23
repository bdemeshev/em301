library(recipes) # типичные рецепты для обработки данных
library(tidyverse) # манипуляции с данными


# добавим три главные компоненты
# по старинке
glimpse(diamonds)
# руками отобрал количественные переменные
d_num <- select(diamonds, carat, depth, table, x, y, z)
glimpse(d_num)

# оцениваем все главные компоненты
all_pca <- prcomp(d_num, scale. = TRUE)
# извлекаем первые три главные компоненты
pca123 <- all_pca$x[, 1:3]
str(pca123)

# подклеиваем три главных компоненты 
# к набору данных diamonds
d2 <- mutate(diamonds, 
  pca1 = pca123[, 1], pca2 = pca123[, 2],
  pca3 = pca123[, 3])

# добавим три главные компоненты
# с помощью рецептов:

# 1. корректность типов переменных
glimpse(diamonds)

# пример ситуации, которая требует правки:
diamonds_bad <- diamonds %>% 
  mutate(cut = as.numeric(cut))
glimpse(diamonds_bad)
# как исправить?
diamonds_restored <- diamonds_bad %>%
  mutate(cut = factor(cut))
glimpse(diamonds_restored)

# 2. Начинаем рецепт с указания роли переменных
blin_1 <- recipe(price ~ ., data = diamonds)
blin_1

# 3. Добавим к рецепту шаг с добавлением трех главных компонент
blin_1 <- blin_1 %>% 
  step_pca(all_numeric(), num = 3)
blin_1

# 4. Подготовка рецепта 
# оценка параметров жарки
blin_1_prep <- prep(blin_1, 
                    training = diamonds)
blin_1_prep

# 5. Жарим исходные данные согласно 
# подготовленному рецепту
d3 <- bake(blin_1_prep, 
           newdata = diamonds,
           all_predictors())
glimpse(d3)
d3

# Забыли в рецепте масштабировать 
# главные компоненты!
blin_2 <- recipe(price ~ ., data = diamonds)
blin_2 <- blin_2 %>% 
  step_pca(all_numeric(), -all_outcomes(), num = 3,
    options = list(scale. = TRUE, center = TRUE))
# добавка options = list(...) 
blin_2_prep <- prep(blin_2, data = diamonds)
d4 <- bake(blin_2_prep, diamonds)
glimpse(d4)
glimpse(d2)

# мини-упр.
# рецепт, который от объясняющих переменных берёт логарифм
# от зависимой переменной - корень
r3 <- recipe(price ~ ., data = diamonds)
r3 <- r3 %>% 
        step_log(all_numeric(), -all_outcomes()) %>%
        step_sqrt(all_outcomes())



# разделим набор данных на две части:
train_index <- createDataPartition(
  diamonds$price, p = 0.8, list = FALSE)
train_index

d_train <- diamonds[train_index, ]
d_test <- diamonds[-train_index, ]

# фаза по подготовке рецепта здесь не обязательна
# так как нет параметров жарки набора данных
r3_prep <- prep(r3, d_train)
r3_prep

d_train_baked <- bake(r3_prep, d_train)
d_tеst_baked <- bake(r3_prep, d_test)
glimpse(d_train_baked)


