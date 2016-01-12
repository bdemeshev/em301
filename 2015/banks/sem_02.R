setwd("~/Downloads")

flats <- read.table("flats_moscow.txt",
                    header = TRUE,
                    sep = "\t",
                    dec = ".")
help(read.table)

library("dplyr") # манипуляции с данными
library("ggplot2") # графики
library("lmtest") # тесты для линейных моделей
library("psych") # описательные статистики
library("vcd") # графики для качественных переменных

describe(flats)

flats2 <- mutate(flats, ne_znau = rnorm(2040))
describe(flats2)

describe(flats) -> otchiot
otchiot
mini_otchet <- select(otchiot, mean, min, max)

mechta_Svetki <- filter(flats, kitsp > 10,
                        brick == 1)
minimini <- filter(otchiot, vars %in% c(5, 9))

some_vars <- select(flats, brick, kitsp)
describe(some_vars)

library("readxl")
demo <- read_excel("demo.xlsx", sheet = 2)

library("quantmod")
getSymbols(Symbols = c("AAPL","GOOG"),
                   src = "google",
                   from = "2011-01-01",
                   to = "2016-01-01")

library("rusquant")

library("devtools")
install_github("rforge/rusquant/pkg")

library("rusquant")
getSymbols(Symbols = "GAZP",
           src = "Finam",
           from = "2011-01-01",
           to = "2016-01-01")


qplot(data = flats, x = totsp,
      y = price, color = kitsp)

qplot(data = flats, x = totsp)

mosaic(data = flats, ~brick + walk)
mosaic(data = flats, ~walk + brick)


flats <- mutate(flats,
      othersp = totsp - livesp - kitsp)


model_1 <- lm(data = flats,
  log(price) ~ log(kitsp) +
    log(livesp) + log(othersp))
summary(model_1)

library("broom")
glance(model_1)
tidy(model_1)
glance(flats)

confint(model_1, level = 0.9)
tidy(model_1)

qt(0.95, df = 2036)

