setwd("~/Downloads/wages_in_the_USA")
library("foreign")
library("lmtest")

v <- read.dta("wages1.dta")

str(v)
cor(v)
glimpse(v)


model1 <- lm(data=v, wage~exper+school)
summary(model1)
nrow(v)
rss_r <- deviance(model1)


model2 <- lm(data=v, wage~exper+school+male)
rss_ur <- deviance(model2)

f_obs <- (rss_r - rss_ur)/1/(rss_ur/(nrow(v)-4))
f_obs
nrow(v)-4
f_cr <- qf(0.95,df1=1,df2=nrow(v)-4)
f_cr

waldtest(model2,model1)

