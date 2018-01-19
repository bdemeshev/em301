library(tidyverse)
library(imputeTS)
library(forecast)
library(lubridate)

# data: goo.gl/M8Du9p
# this file: goo.gl/AdQrB8
# form for emails: goo.gl/YdZYgd

cpi_data <- read_csv("~/Downloads/cpi_weekly.csv")
glimpse(cpi_data)

# set working directory from menu
# session - set working directory - choose directory
setwd("~/Downloads")
cpi_data <- read_csv("cpi_weekly.csv")

colnames(cpi_data)
colnames(cpi_data) <- c("date", "actual")
colnames(cpi_data)

# create a vector
b <- c(7, 6, 8, -2, NA)
b

# 1. create new variable (or change existing one)
cpi_data2 <- mutate(cpi_data, 
                    y2 = actual^2)
glimpse(cpi_data2)

# 2. filtration
cpi_data3 <- filter(cpi_data, 
                    actual > 100.1)
glimpse(cpi_data3)


# first black magic :)
cpi_data4 <- mutate(cpi_data, 
                    date = as.Date(date))
glimpse(cpi_data4)

# transform character string into date
ymd("2017-07-14") > ymd("2018-01-8")
ymd("2018-01-17") + days(200)

max(cpi_data$date)
min(cpi_data4$date)

max(cpi_data4$date) - min(cpi_data4$date)
as.numeric(max(cpi_data4$date) - min(cpi_data4$date)) / 7
nrow(cpi_data4)

# just a moment!
# arithmetic progressions
5:10
days(5:10)
weeks(5:10)

# create new table with regular dates
cpi_clean <- tibble(date = min(cpi_data4$date) + weeks(0:485))

cpi_clean2 <- left_join(cpi_clean, cpi_data4, by = "date")

# for simplicity I will assume that 1 year = 52 weeks

as.numeric(min(cpi_clean$date) - ymd("2008-01-01")) / 7

# transform data frame into regular time series object
cpi_ts <- ts(start = c(2008, 32), frequency = 52,
             cpi_clean2$actual)

plotNA.distribution(cpi_ts)
statsNA(cpi_ts)

# linear interpolation for missing observations
cpi_filled <- na.interpolation(cpi_ts)
plotNA.distribution(cpi_filled)

# modelling time series
ggtsdisplay(cpi_filled)


ar2_sar1 <- Arima(cpi_filled,
                  order = c(2, 0, 0),
                  seasonal = c(1, 0, 0))
ar2_sar1
forecast(ar2_sar1, h = 5)

ar2_sar1_forecast <- forecast(ar2_sar1, h = 5)
autoplot(ar2_sar1_forecast)

naive(cpi_filled, h = 5)


# estimate a lot of naive models using growing window
e_naive <- tsCV(cpi_filled, naive, h = 1)

arsar_forecast <- function(y, h) {
  model <- Arima(y,
                 order = c(2, 0, 0),
                 seasonal = c(1, 0, 0))
  fcst <- forecast(model, h)
  return(fcst)
}

arsar_forecast(cpi_filled, h = 2)
e_arsar <- tsCV(cpi_filled, arsar_forecast, h = 1)

