library("ggplot2")
library("dplyr")
library("lmtest")
library("psych")

setwd("~/Downloads/")
flats <- read.table("flats_moscow.txt",
     header = TRUE, sep = "\t", dec = ".")
model_0 <- lm(data = flats, price~totsp)
summary(model_0)

confint(model_0)
confint(model_0, level = 0.8)

new <- data_frame(totsp = c(60, 80, 100, 120))

predict(model_0, new)

predict(model_0, new, interval = "confidence")
predict(model_0, new, interval = "prediction")

vcov(model_0)
predict(model_0, new,
        interval = "confidence", se.fit = TRUE)

qt(0.975, df = 48)
qf(0.95, df1 = 5, df2 = 10)

flats2 <- mutate(flats,
          othersp = totsp - livesp - kitsp)
glimpse(flats2)
model_ur <- lm(data = flats2,
               price ~ livesp + kitsp + othersp)
rss_r <- deviance(model_0)
rss_ur <- deviance(model_ur)
rss_ur
rss_r
nrow(flats)
F_obs <- ((rss_r - rss_ur) / 2) /
                      (rss_ur / (nrow(flats) - 4))
F_obs
F_crit <- qf(0.95, df1 = 2, df2 = nrow(flats) - 4)
F_crit

library("car")
model_ur <- lm(data = flats2,
               price ~ livesp + kitsp + othersp)
linearHypothesis(model_ur,
        c("livesp = kitsp", "livesp = othersp"))

