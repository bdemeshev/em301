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


