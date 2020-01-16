library(rio)
library(tidyverse)
library(skimr)

# stats::filter() # function filter from package stats
# dplyr::filter() # function filter from package dplyr

my_favorite_number = 9
my_favorite_number

# if you know the location of the data file
cake = import("~/Downloads/cakes_data.xlsx")
cake

# if you don't know the location of the data file:
# file.choose()
cake = import("/home/boris/Downloads/cakes_data.xlsx")
cake
glimpse(cake)
skim(cake)

# first regression in R
model_a = lm(data = cake, result ~ cake)
summary(model_a)

# second regression in R
model_b = lm(data = cake, result ~ cake + gender)
summary(model_b)
# genderM — dummy variable that replaces character variable gender
# genderM = 1 if gender = "M"


# data transformation
# built-in dataset on diamonds
glimpse(diamonds)

# select variables
d2 = select(diamonds, carat, price, x, y, z)
glimpse(d2)

d3 = select(diamonds, -color, -clarity, -table)
glimpse(d3)


help(diamonds)
help(select)

# filter observations
d4 = filter(diamonds, price > 1000, x > y) # AND
glimpse(d4)

d5 = filter(diamonds, cut > "Good" | price == 2757) # OR
glimpse(d5)

# create/modify a variable
d6 = mutate(diamonds, price_sq = price^2, log_price = log(price))
glimpse(d6)

# Gauss tradition
log(cos(0.1))
# Natural order of operations
0.1 %>% cos %>% log

# natural order of operations
diamonds %>% filter(carat > mean(carat)) %>%
  select(price, carat) %>%
  arrange(price) %>%
  head(10)

# gauss tradition
head(arrange(select(filter(diamonds, carat > mean(carat)), 
                     price, carat), price), 10)

# sort dataset
d7 = arrange(diamonds, cut, -price) %>% head(10)
glimpse(d7)

# two similar function
head(diamonds, 5)
top_n(diamonds, 5, price)

# how to adress an element of a dataset
cake

cake[3, 2] # bad style
cake[3, "result"]

cake[3, ] # full row

cake[, "result"] # full column
cake["result"]
cake$result
pull(cake, result)

cake[2:4, "cake"] # rows from 2 to 4, column "cake

# group operations

mean(diamonds$price) # average price of all diamonds

glimpse(diamonds)

# group dataset by cut
# calculate average price for each group of diamonds
cut_summary = group_by(diamonds, cut) %>% 
  summarise(av_price = mean(price), low_price = min(price), high_price = max(price))
cut_summary

color_summary = group_by(diamonds, color) %>% 
  summarise(av_weight = mean(carat), sd_weight = sd(carat), n_obs = n())
color_summary


# join two datasets
d8 = left_join(diamonds, cut_summary, by = "cut")
glimpse(d8)
View(d8)

# some basic plots :)
# histogram
qplot(data = diamonds, x = price) + 
  xlab("Price in dollars") + 
  ylab("Quantity") + 
  ggtitle("Diamonds dataset")

qplot(data = diamonds, x = log(price)) + 
  xlab("Log price in dollars") + 
  ylab("Quantity") + 
  ggtitle("Diamonds dataset")

# scatterplot
qplot(data = diamonds, x = log(carat), y = log(price),
      color = cut) + 
  xlab("Log weight in carats") + 
  ylab("Log price in dollars") + 
  ggtitle("Diamonds dataset")

# recoding variables!

d9 = mutate(diamonds, weight = case_when(log(carat) < -1 ~ "A",
                                         log(carat) < 0 ~ "B",
                                         log(carat) < 1 ~ "C",
                                         log(carat) >= 1 ~ "D"))
glimpse(d9)

