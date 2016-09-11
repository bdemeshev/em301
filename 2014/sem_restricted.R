library("foreign")
library("dplyr")
library("lmtest")

# if you can't read cyrillic letters here, please File-Reopen with encoding - utf8

# скачайте данные `wages in USA` со странички
# http://eu.wiley.com/legacy/wileychi/verbeek2ed/datasets.html

# разархивируйте и установите в качестве рабочей папке ту, в которой находится файл wages1.dta
# Session-Set working directory-....
# setwd("~/Downloads/wages_in_the_USA")
w <- read.dta("wages1.dta")
glimpse(w)
#str(w)

model <- lm(wage~exper+school,data=w)
summary(model)

wm <- filter(w,male==1)
nrow(w)
nrow(wm)
glimpse(wm)
model_m <- lm(wage~exper+school,data=wm)
summary(model_m)
deviance(model_m) # RSS

wf <- filter(w,male==0)
model_f <- lm(wage~exper+school,data=wf)
summary(model_f)

model_ur <- lm(wage~exper+school+male+male*exper+male*school,
               data=w)
summary(model_ur)
rss_ur <- deviance(model_ur)
rss_r <- deviance(model)
rss_r
rss_ur
f_obs <- (rss_r-rss_ur)/3/(rss_ur/(nrow(w)-6))
f_obs
nrow(w)-6
f_cr <- qf(0.95,df1=3,df2=nrow(w)-6)
f_cr

waldtest(model_ur,model)
