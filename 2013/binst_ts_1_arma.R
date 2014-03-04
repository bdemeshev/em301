kings <- c(60,43,67,50,56,42,50,65,68,43,65,34,47,34,49,41,13,35,53,56,16,43,69,59,48,59,86,55,68,51,33,49,67,77,81,67,71,81,68,70,77,56)

summary(kings)
str(kings)

require(erer)


qplot(1:42,kings,geom="line")

mod.arma <- arma(kings,order=c(1,1))
summary(mod.arma)

mod.ar <- arma(kings,order=c(1,0))
summary(mod.ar)

mod.ma <- arma(kings,order=c(0,1))
summary(mod.ma)

acf(kings)
acf(kings,plot=FALSE)
