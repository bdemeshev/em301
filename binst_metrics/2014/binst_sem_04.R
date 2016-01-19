library("AER")
library("mvtnorm")
library("ggplot2")

mu <- c(0,0)
Sigma <- matrix(c(9, -3, -3, 16), nrow=2)
mu
Sigma

z <- rmvnorm(n=10^6, mu, Sigma)
z
qplot(z[,1],z[,2])
mean(z[,1])
cov(z[,1],z[,2])


n_exp <- 10^6

mu <- c(0,0)
Sigma <- matrix(c(10,-5,-5,9),nrow=2)
z <- rmvnorm(n=n_exp, mu, Sigma)
x <- z[,1]
e <- z[,2]
cov(x,e)
y <- 2*x + e 
model <- lm(y~x)
summary(model)

model <- lm(y~0+x)
summary(model)

