library(readr)
library(dplyr)
library(lattice)
library(dplyr)
library(dygraphs)
library(zoo)


# some examples
# http://timelyportfolio.github.io/rCharts_time_series/history.html

a <- read_delim("GDP.csv", delim = ";")
str(a)

ats <- ts(a$GDP,frequency = 4, start=c(1995,1))
str(ats)

plot(ats)

xyplot(ats)

autoplot.zoo(ats)

dygraph(ats, main = "gdp, mln tugrikov") %>% 
  dyShading(from = "2000-10-1", to = "2005-6-1")
# https://rstudio.github.io/dygraphs/index.html


