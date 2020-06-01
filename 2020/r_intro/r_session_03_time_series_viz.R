# use open source if possible :)

# install development version of R package
library(devtools)
install_github("https://github.com/tidyverts/fable")

# don't hesitate to make a bug report or a wish :)

# google "cran task view" if you don't know which package you need

# how to work with bad names?
library(tidyverse)
bad_names_data = tibble(`a + b` = 1:10, `2020` = rnorm(10))
bad_names_data

good_names_data = rename(bad_names_data, sum = `a + b`, year_2020 = `2020`)
good_names_data

# long and wide tables

wide_data = tibble(indicator = c("gdp", "unemp"), 
                   `2000` = c(5, 6),
                   `2001` = c(7, 3),
                   `2002` = c(5, 9),
                   `2003` = c(4, 9))
wide_data

# pivot_longer and pivot_wider
long_data_a = pivot_longer(wide_data, `2000`:`2003`)
long_data_a


wide_data_b = pivot_wider(long_data_a, id_cols = "indicator")
wide_data_b

long_data_b = 
  pivot_wider(long_data_a, id_cols = "name", names_from = "indicator")
long_data_b

library(tsibble) # time series in rich tsibble format
library(feasts) # visualizations for time series
library(fable) # ARIMA, ETS, NAIVE... models for tsibble format
library(lubridate) # nice functions for date manipulations 

ymd("2020-01-23") + days(1:20) - months(5)
ymd("2020-01-06") > ymd("2018-06-12")
ymd("2020/03/08")
dmy("20.01.2020")

df = tibble(x = rnorm(200, mean = 5, sd = 10), 
            date = ymd("2000-01-01") + days(1:200))
df
df2 = as_tsibble(df, index = date)
df2

df3 = mutate(df2, x2 = x^2)
df3
autoplot(df3, x2)

tourism

holiday_data = filter(tourism, Purpose == "Holiday") %>% 
  group_by(State) %>% summarise(Trips = sum(Trips))
holiday_data

autoplot(holiday_data, Trips)
gg_season(holiday_data, Trips) # it should work :)
gg_subseries(holiday_data, Trips)

# autocorrelation plots
acf_data = filter(holiday_data, State == "ACT") %>% 
  ACF(Trips) 
acf_data
autoplot(acf_data)

ACF(holiday_data, Trips) %>% autoplot()

filter(holiday_data, State == "ACT") %>%
  gg_tsdisplay(Trips)

