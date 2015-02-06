# bult-in solution
library("sandwich")
df <- data.frame(y=c(1,3,3), x=c(0,0,1))
df

model <- lm(data=df, y~x)
coef(model)
# residuals
resid(model)


vcov(model)
vcovHC(model) # should fail
vcovHC(model, type ="HC0" )
help(vcovHC)



# by hand
y <- c(1,3,3)
X <- cbind(rep(1,3), c(0,0,1))
X
y

hat_beta <- solve(t(X) %*% X) %*% t(X) %*% y
hat_beta


# by hand Var(hat_beta)
y_hat <- X %*% hat_beta
e_hat <- y-y_hat
RSS <- sum((y-y_hat)^2)
vcov_ols <- RSS/(3-2)*solve(t(X) %*% X)
vcov_ols

# crossprod(X) is just synonym fort(X) %*% X

H <- X %*% solve(crossprod(X)) %*% t(X)
H

diag(H)

S_hat_white <- diag(as.vector(e_hat^2))
S_hat_HC3 <- diag(as.vector(e_hat^2)/(1-diag(H))^2)
S_hat_HC3 # look at the problem

# vcov White
solve(crossprod(X)) %*% t(X)  %*% S_hat_white %*% X %*% solve(crossprod(X))
# vcov HC3
solve(crossprod(X)) %*% t(X)  %*% S_hat_HC3 %*% X %*% solve(crossprod(X))
