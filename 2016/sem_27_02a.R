library(glmnet) # LASSO and Ridge regression

# LASSO: big k, small n

library(tidyverse)
glimpse(mtcars)


model_0 <- lm(data = mtcars, 
              mpg ~ (cyl + disp + hp + wt + am)^3 )
model_1 <- lm(data = mtcars, 
              mpg ~ I((cyl + disp + hp)^2) )
summary(model_1)
summary(model_0)

# handmade y and X :)
y <- mtcars$mpg
X <- model.matrix(data = mtcars, 
                  mpg ~ 0 + (cyl + disp + hp + wt + am)^3)

# LASSO:
lasso_model <- glmnet(X, y)
lasso_model

glimpse(mtcars)
coef(lasso_model, s = c(0.7, 3.3))

str(X)
colnames(X)

new_car = matrix(c(rep(0, 7), 15, rep(0, 17)), nrow = 1)

predict(lasso_model, s = 3.3, newx = new_car)

# ridge
ridge_model <- glmnet(X, y, alpha = 0)
ridge_model
coef(ridge_model, s = 5000)

plot(lasso_model, label = TRUE)
plot(lasso_model, label = TRUE, 
     xvar = "lambda")
plot(lasso_model, label = TRUE, 
     xvar = "dev")

set.seed(322223322)
lasso_cv <- cv.glmnet(X, y)
lasso_cv

lasso_cv$lambda.min
lasso_cv$lambda.1se
