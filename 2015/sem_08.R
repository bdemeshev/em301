# em301, 03.11.2015, семинар 8

# problem 4.13

df <- data.frame(y = c(1,2,3,4,5),
                 x = c(0,0,0,1,1),
                 z = c(0,0,0,0,1))
df
model <- lm(data=df, y~x+z)
summary(model)

otchet <- summary(model)

# table with coefficients
coef(otchet)

# extract t statistics for beta_2
t_x <- coef(otchet)[2,3]
t_x

# rss
deviance(model)

# tss
tss <- sum(  ( df$y - mean(df$y) )^2  )
tss

# extract column 3
df[ ,3]

# extract row 2
df[2, ]

# джин, covariance matrix
vcov(model)
resid(model) # epsilon hat (residuals)
fitted(model) # y_hat (forecast of y)

V <- vcov(model)

# se(b_x + b_z)
se <- sqrt(V[2,2] + V[3,3] + 2*V[2,3])
se
b <- coef(model)
(b[2] + b[3] - 2)/se
qt(0.025, df=2)

coef(otchet)

df$yy <- df$y - 2*df$x
df$zz <- df$z - df$x
df

model2 <- lm(data=df, yy~x+zz)
summary(model2)