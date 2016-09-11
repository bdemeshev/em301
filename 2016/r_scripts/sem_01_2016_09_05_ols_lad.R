x <- c(1, 2, 0)
y <- c(10, 0, 4)
md <- data.frame(problem = x, pokemon = y)
md

# OLS - ordinary least squares
model_1 <- lm(data = md, pokemon~problem)
summary(model_1)

# LAD - least absolute deviations
install.packages("quantreg") # install a package once
library("quantreg") # attach each time you use it

model_1_lad <- rq(data = md, pokemon~problem)
summary(model_1_lad)


