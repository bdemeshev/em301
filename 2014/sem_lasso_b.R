library("glmnet")
library("dplyr")

d <- cars 
plot(cars)

d <- mutate(d, speed2=speed^2,
            speed3=speed^3)
glimpse(d)
cor(d)

model <- lm(data=d, 
  dist~speed+speed2+speed3)
summary(model)

# (!) from biggest to lowest
lambdas <- seq(from=100,
               to=0.1,
               by=-0.05)
head(lambdas)
tail(lambdas)

y <- d$dist
X0 <- model.matrix(data=d, 
    dist~0+speed+speed2+speed3)
head(X0)

help("glmnet")
model_lasso <- glmnet(x=X0,
      y=y,
      alpha=1,
      lambda=lambdas)
coef(model_lasso, s=c(100,10,1,0.1))
summary(model)

plot(model_lasso, 
     xvar="lambda",
     label=TRUE)

plot(model_lasso, 
     xvar="dev",
     label=TRUE)

plot(model_lasso, 
     xvar="norm",
     label=TRUE)

cv_result <- cv.glmnet(x=X0,
                       y=y,
                       alpha=1)
plot(cv_result)
# оценка лямба методом кросс-валидации
cv_result$lambda.min

# консервативная оценка лямбда 
cv_result$lambda.1se


coef(cv_result,
     s="lambda.1se")
coef(cv_result,
     s="lambda.min")


