library(tidyverse)
library(glmnet)

glimpse(diamonds)

new_data <- tibble(x = 4, y = 4, z = 2.5, carat = 0.3, table = 60)
new_data

# создаём столбцы регрессоров в явном виде
X <- model.matrix(data = diamonds, ~ 0 + x * y * z * carat * table)
X_new <- model.matrix(data = new_data, ~ 0 + x * y * z * carat * table)
X_new

y <- diamonds$price

# мнк прогнозы
model <- lm(data = diamonds, price ~ x * y * z * carat * table)
summary(model)
predict(model, newdata = new_data) # точечный прогноз

# предиктивный интервал
predict(model, newdata = new_data, interval = "prediction")

# lasso прогнозы
model_lasso <- glmnet(X, y)
model_lasso
ncol(X)
coef(model_lasso, s = 130)
predict(model_lasso, newx = X_new, s = 130)

# делим выборку на две части!
# случайное число
sample(1:100, size = 5)
# задаем зерно генератора случайных чисел
set.seed(7)
sample(1:100, size = 5)

# создадим столбец с уникальным идентификатором
d2 <- mutate(diamonds, id = row_number())
glimpse(d2)

set.seed(42)
train <- sample_frac(d2, size = 0.9)
glimpse(train)

test <- anti_join(d2, train, by = "id")
glimpse(test)

# мнк
ols_train <- lm(data = train, price ~ x * y * z * carat * table)
rss_train <- deviance(ols_train)
rss_train / nrow(train)

# прогнозы по МНК
test <- mutate(test, price_ols = predict(ols_train, newdata = test))
glimpse(test)
sum((test$price - test$price_ols)^2) / nrow(test)

# lasso
X_train <- model.matrix(data = train, ~ 0 + x * y * z * carat * table)
X_test <- model.matrix(data = test, ~ 0 + x * y * z * carat * table)
y_train <- train$price

lasso_train <- glmnet(X_train, y_train)
lasso_train

# прогнозы по lasso
test <- mutate(test, 
               price_lasso = predict(lasso_train, newx = X_test, s = 130))
glimpse(test)
sum((test$price - test$price_lasso)^2) / nrow(test)

