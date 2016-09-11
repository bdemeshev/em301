# задача 4 
# https://github.com/bdemeshev/em301/raw/master/2014/kr_02/kr_02.pdf
# if you can't see cyrillic letters, please, File - Reopen with encoding - utf8

# решение 1
y <- c(1,2,3,4,2)
x <- c(0,0,1,1,1)
z <- c(0,0,0,0,1)

model <- lm(y~x+z)
summary(model)

# решение 2
X <- matrix(c(rep(1,5),x,z),nrow=5)
X

XtX <- t(X) %*% X
hat_beta <- solve(XtX) %*% t(X) %*% y
hat_beta

# etc

