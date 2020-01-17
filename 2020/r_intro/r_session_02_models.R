library(tidyverse) # data manipulations + plots
library(rio) # import data from various file types
library(lmtest) # waldtest and many more tests
library(fable) # ts forecasting
library(estimatr) # HC robust se 
library(texreg) # tables with regression comparison

# 3 basic object types: tibble, vector and list
foo = tibble(x = 1:20, y = rnorm(20, mean = 5, sd = 7), z = rep(5, 20))
foo
foo[4, "y"]

bar = foo$y # foo["y"]
bar
bar[5]

garbage = list(first = foo, second = bar)
glimpse(garbage)
garbage["second"]
garbage[["second"]]


apple = cos(1)
apple

my_shopping_bag = list(garbage[["second"]], apple)

model_0 = lm(data = foo, y ~ x)
report = summary(model_0)
report
glimpse(model_0)
glimpse(report)

report[["adj.r.squared"]]

# heteroskedasticity robust standard errors in regression
# OLS with non-robust to heterosk standard errors
model_1 = lm(data = diamonds, price ~ carat + x + y + z)
summary(model_1)

capybara = lm_robust(data = diamonds, price ~ carat + x + y + z)
summary(capybara)

# batch processing (? bad name ?)
log(5)
my_lovely_numbers = c(5, 7, 9, 3, -6)
log(my_lovely_numbers)

my_lovely_models = c(price ~ carat, price ~ x + y + z, price ~ carat + x + y + z)
result = lm(data = diamonds, my_lovely_models) # fails :( :(
# lm does not accept many formulas at once

model_1 = lm(data = diamonds, price ~ carat)
model_2 = lm(data = diamonds, price ~ x + y + z)


deep_table = tibble(formulas = my_lovely_models)
deep_table = mutate(deep_table, 
                    model = map(formulas, ~ lm(data = diamonds, .)))
deep_table
deep_table[["model"]][[2]]

deep_table = mutate(deep_table, 
                    report = map(model, ~ summary(.)))
deep_table

deep_table = mutate(deep_table, 
                    r2 = map_dbl(report, ~ .[["r.squared"]]))
deep_table

# the same thing once again :)
my_lovely_models = c(price ~ carat, 
                     price ~ x + y + z, 
                     price ~ carat + x + y + z)

deep_table = tibble(formulas = my_lovely_models)
deep_table = mutate(deep_table, 
                    model = map(formulas, ~ lm(data = diamonds, .)),
                    report = map(model, ~ summary(.)),
                    r2 = map_dbl(report, ~ .[["r.squared"]]))
deep_table

# compare two models using F/chisq test and missing data :)

model_1 = lm(data = diamonds, price ~ carat)
model_2 = lm(data = diamonds, price ~ carat + x + y + z)

waldtest(model_1, model_2)

# publish table with model comparison

screenreg(list(model_1, model_2))
texreg(list(model_1, model_2))
htmlreg(list(model_1, model_2))


