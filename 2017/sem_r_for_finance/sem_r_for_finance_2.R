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
