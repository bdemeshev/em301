
df <- data.frame(
  pr1=rnorm(100,mean=5,sd=1),
  pr2=rnorm(100,mean=10,sd=2))
head(df)
head(df,10)
tail(df)
df
library("dplyr")
df <- mutate(df, kr=pr1+pr2)
head(df)
model <- lm(data=df, kr~pr1+pr2)
summary(model)

deviance(model) # RSS

library("ggplot2")
h <- diamonds
help(diamonds)
glimpse(h)

ola_rast <- lm(data=h, price~carat)
summary(ola_rast)

qplot(data=h, carat, price)
qplot(data=h, log(carat), log(price))
library("hexbin")
qplot(data=h, log(carat), log(price)) + 
  geom_hex()
str(h)

help(diamonds)
qnorm(0.025)
qnorm(0.975)

library("lmtest")
confint(ola_rast)
confint(ola_rast, level = 0.9)

setwd("~/Downloads") # Session - Set Working Directory - Choose
spt <- read.csv("data.csv",
                header=TRUE,
                sep=",",
                dec=".")
spt

library("devtools")
install_github("bdemeshev/rlms") # this package in not yet officially released, but it works
# simple demo may be found at https://github.com/bdemeshev/rlms

library("rlms")
# here you should download the rlms data from www.hse.ru/rlms and save it somewhere
h <- read.rlms("r21i_os24a.sav")
glimpse(h)
