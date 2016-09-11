library("glmnet")
library("car")
library("dplyr")
library("ggplot2")

cars
cars2 <- mutate(cars, speed2 = speed^2, speed3 = speed^3)
glimpse(cars2)

cor(cars2)
model_0 <- lm(data = cars2, dist ~ speed + speed2 + speed3)
summary(model_0)


X <- model.matrix(data = cars2, ~ speed + speed2 + speed3)

head(X)
XX <- crossprod(X) # X'X
det(XX)

# собственные числа матриц X'X
lambda <- eigen(XX)$values 

max(lambda)
min(lambda)

CI <- sqrt(max(lambda) / min(lambda))
CI

CI

model_1 <- lm(data = cars2[1:49, ], dist ~ speed + speed2 + speed3)
summary(model_1)


library("glmnet")

# создаём X без константы (или с) без разницы :)
X <- model.matrix(data = cars2, 
                  ~ speed + speed2 + speed3)
head(X)
y <- cars2$dist

model_lasso <- glmnet(X, y)
model_lasso

coef(model_lasso, s = 10)
plot(model_lasso, xvar = "lambda")
plot(model_lasso, xvar = "dev")

coef(model_lasso, s = 0.1, exact = TRUE)

cv_lasso <- cv.glmnet(X, y)
plot(cv_lasso)
cv_lasso$lambda.min
coef(cv_lasso, s = "lambda.min")

m <- matrix(c(10, 100, 1000), nrow = 1)
m
predict(cv_lasso, newx = m, 
        s = "lambda.min")


# ridge
model_ridge <- glmnet(X, y, alpha = 0)
model_ridge
