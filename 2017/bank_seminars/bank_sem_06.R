library(tidyverse)
library(rio)
library(glmnet)

# маленькое упражнение
# мнк = lasso при lambda = 0
model <- lm(data = diamonds, 
        price ~ x * y * z * table * carat)
summary(model)


# lasso
# первый шаг технический
# отдельно выделим зависимую переменную
# и матрицу всех предикторов
y <- diamonds$price
X <- model.matrix(data = diamonds, 
        ~ 0 + x * y * z * table * carat)

# число столбцов в X
ncol(X)

lasso <- glmnet(X, y)
lasso

# на глаз:
# при 3х коэффициентах получаем долю
# объяснённной дисперсии 0.85
# при максимальной 0.87
# lambda = 130
coef(lasso, s = 130)

plot(lasso, 
    label = TRUE, 
    xvar = "lambda")
plot(lasso, 
     label = TRUE, 
     xvar = "dev")

lasso_cv <- cv.glmnet(X, y)
lasso_cv

# лямбда найденное с помощью кросс-валидации
lasso_cv$lambda.min

# лассо регрессия с этим лямбда
coef(lasso_cv, s = lasso_cv$lambda.min)

lasso_cv$lambda.1se
