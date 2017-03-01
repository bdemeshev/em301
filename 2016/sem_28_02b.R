library(glmnet) # LASSO + ridge
library(tidyverse) # графики и манипуляции с данными

glimpse(mtcars)

model_0 <- lm(data = mtcars, 
      mpg ~ (cyl + disp + hp + wt)^3)
model_1 <- lm(data = mtcars, 
      mpg ~ I((cyl + disp + hp + wt)^2) )
summary(model_0)
summary(model_1)

# lasso
# we need explicit y and X
y <- mtcars$mpg
X <- model.matrix(data = mtcars, 
       mpg ~ 0 + (cyl + disp + hp + wt)^3)

# digression
# Don't use $$...$$. Use \[ ... \]
# One formula -> one start, one end.
# trash: $y$=$x+4$.
# trash:
# \[
# p = P(\text{ привет $y=1$ })
# \]
# Bad style: $ y=1 $. Good style: $y = 1$.

lasso <- glmnet(X, y)
lasso

ridge <- glmnet(X, y, alpha = 0)
ridge
# q(b) = \sum (y_i - \hat y_i)^2 + 0.5 \lambda \sum |\beta_j| + 

coef(lasso, s = c(0.0198, 4))

str(X)
Xnew <- matrix(nrow = 2, byrow = TRUE,
      c( rep(0, 6), 12, rep(0, 7),
         rep(0, 6), 20, rep(0, 7)) )
Xnew
# predictions
predict(lasso, newx = Xnew, s = 4)

# some plots
plot(lasso, xvar = "lambda", label = TRUE)
plot(lasso, xvar = "norm", label = TRUE)
plot(lasso, xvar = "dev", label = TRUE)

# cross-validation
set.seed(12)
lasso_cv <- cv.glmnet(X, y)
hat_lambda_cv <- lasso_cv$lambda.min
hat_lambda_1se <- lasso_cv$lambda.1se
hat_lambda_1se

# coefs for conservative lambda estimate:
coef(lasso_cv, s = "lambda.1se")


