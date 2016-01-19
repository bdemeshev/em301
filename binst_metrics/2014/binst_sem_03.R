Fobs <-  ( (0.3 - 0.2)/2 ) / ( 0.2 / (100 - 4) )
Fobs
Fcr <- qf(0.95, df1=2, df2=96)
Fcr

library("ggplot2")
library("dplyr")
library("memisc")
library("lmtest")
library("psych")

list.files()
h <- read.csv("flats_moscow.txt", header=TRUE,
              sep="\t", dec=".")
glimpse(h)

write.csv(h, "h_copy.csv")
edit(h)
View(h)

describe(h)
qplot(data=h, totsp, price)
qplot(data=h, log(totsp), log(price))

mod1 <- lm(log(price)~log(totsp)+log(livesp)+
             walk+log(metrdist), data=h)
summary(mod1)
mtable(mod1)

mod2 <- lm(log(price)~log(totsp)+log(livesp), data=h)
mtable(mod1, mod2)

waldtest(mod2, mod1)
?waldtest

h <- mutate(h, remsp = totsp - livesp - kitsp)
glimpse(h)
mod3 <- lm(data=h, log(price)~
             log(livesp) + log(kitsp) + log(remsp) +
             log(metrdist) + walk + brick)
mtable(mod3)

mod4 <- lm(data=h, log(price)~ log(livesp) + brick)
mtable(mod4)

mod5 <- lm(data=h, log(price)~ 
             log(livesp) + brick + log(livesp):brick)
mtable(mod5)

mod5 <- lm(data=h, log(price)~ 
             log(livesp) * brick)
mtable(mod5)

mod6 <- lm(data=h, log(price)~ 
             (log(livesp) + log(kitsp)) * brick)
mtable(mod6)

waldtest(mod5, mod6)
resettest(mod6)


