# data goo.gl/Pbgo7Y
# all files goo.gl/2Y1yrV
# form for emails: goo.gl/YdZYgd

library(tidyverse)
library(rio)
library(lmtest)
library(sandwich)

# load data
flats <- import("~/Downloads/flats_moscow.txt")

# if you don't know where file is located
file.choose()

# look at the data
glimpse(flats)

# some plots
qplot(data = flats, x = price)

# maybe I should take logs?
qplot(data = flats, x = log(price))

# scatter plots
qplot(data = flats, y = log(price), x = log(totsp))

# basic operations with data frame

# 1. create new variable
flats2 <- mutate(flats, ln_price = log(price))
glimpse(flats2)

# result in flats3
# othersp = totsp - kitsp - livesp
flats3 <- mutate(flats2, othersp = totsp - livesp - kitsp)

# 2. filtering observations
flats4 <- filter(flats3, price > mean(price))

# filter flats in zone 1 of moscow (center), look at the code variable
flats5 <- filter(flats3, code == 1)

# 3. selecting variables
flats6 <- select(flats3, price, totsp, livesp, kitsp)
# drop some variables
flats7 <- select(flats3, -n, -floor)


# small remark on nesting functions :)
log(cos(0))
0 %>% cos %>% log

# 4. group by and arranging
flats8 <- group_by(flats3, code) %>% 
  summarise(average_price = mean(price), n_flats = n()) %>%
  arrange(average_price)
flats8

flats8$average_price

options(digits = 6)

# linear regression
model_1 <- lm(data = flats3, price ~ livesp + kitsp + othersp)
summary(model_1)

# strict linear dependancy in regressors
model_2 <- lm(data = flats3, price ~ livesp + kitsp + othersp + totsp)
summary(model_2)


# list is a bag of junk!
mylist <- list(a = 5, model = model_1, v = c(1, 2, 3, 7))

mylist$v
mylist[[3]]

mylist[["model"]]
mylist$model

summary_2 <- summary(model_2)
summary_2[["adj.r.squared"]]
summary_2[["fstatistic"]][["value"]]

# group_by and regressions :)
reg_table <- group_by(flats3, code) %>%
  do(model = lm(data = ., price ~ livesp + kitsp + othersp))

reg_table

# working with lists :)
m2 <- reg_table$model[[2]]
# structure of a list
str(m2)

# extract second coefficient:
m2$coefficients[2]
m2[["coefficients"]][2]

# all coefficients:
m2$coefficients
attr(m2$coefficients, "names")

# get the class of an object
class(m2)

reg_table2 <- mutate(reg_table, 
                     beta_hat = map_dbl(model, ~ .$coefficients[2]))
# ??? ??? ???

reg_table$beta_hat <- map_dbl(reg_table$model, ~ .$coefficients[2])

extract_r2 <- function(model) {
  model_summ <- summary(model)
  r2 <- model_summ[["r.squared"]]
  return(r2)
}

# test a function on one model
extract_r2(model_1)

# apply a function to all models
reg_table$r2 <- map_dbl(reg_table$model, ~ extract_r2(.))

reg_table

# confidence intervals
confint(model_1, level = 0.9)

# compare two nested models using F-test
glimpse(flats3)
model_big <- lm(data = flats3, price ~ livesp + kitsp + othersp + 
                  floor + walk + brick)
# H0: beta_floor = beta_walk = beta_brick = 0
model_1 <- lm(data = flats3, price ~ livesp + kitsp + othersp)

# F-test in R
waldtest(model_1, model_big)
# conclusion: big model is preferred            

# pick up RSS
deviance(model_1)
deviance(model_big)
