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

# time series forecasting
# let's create ts object — for regular time series
y = ts(rnorm(200, mean = 5, sd = 7), start = c(2001, 7), frequency = 12)
y[2]

data = as_tsibble(y)
data

?ARIMA
model_table = model(data, 
                    arma11 = ARIMA(value ~ pdq(1, 0, 1)),
                    ma2 = ARIMA(value ~ pdq(0, 0, 2)),
                    ar3 = ARIMA(value ~ pdq(3, 0, 0)),
                    naive = NAIVE(value),
                    snaive = SNAIVE(value))

fcst_table = forecast(model_table, h = "2 years")
fcst_table
autoplot(fcst_table, data)

# let's introduce ETS model on real data
AirPassengers
?AirPassengers
glimpse(AirPassengers)
AirPassengers[5]

airpass = as_tsibble(AirPassengers)
airpass = mutate(airpass, p2 = value^2, log_pass = log(value))
airpass

airpass = select(airpass, date = index, pass = value)
airpass

mtable = model(airpass, 
    sarima = ARIMA(pass ~ pdq(1, 1, 1) + PDQ(1, 1, 1)),
    ets = ETS(pass ~ error("A") + trend("A") + season("A")))

fabletools::forecast(mtable, h = "2 years")
mtable[["ets"]][[1]]
mtable[["sarima"]][[1]]

