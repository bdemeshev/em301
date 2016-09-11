y <- c(1,2,2,5)
x <- c(1,0,0,1)
z <- c(1,2,3,0)

# рассмотрим модель:
# y=\beta_1+\beta_2 x +\beta_3 z + \epsilon

df <- data.frame(y,x,z)
df
X <- model.matrix(data=df,y~x+z)
X
model <- lm(data=df,y~x+z)
summary(model)
RSS <- deviance(model) # RSS
TSS <- sum((y-mean(y))^2) 
ESS <- TSS- RSS
ESS

coef(model) # \hat{\beta}
R2 <- summary(model)$r.squared # R^2
R2

