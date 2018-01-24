# data goo.gl/Pbgo7Y
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


