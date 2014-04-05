require(quantmod) # для загрузки цен акций
require(erer)
require(fGarch)

d <- getSymbols(Symbols="AAPL",
                from="2011-11-11",
                to="2014-03-21",
                env=NULL)
head(d)
tail(d)

# ret_t=(P_t-P_{t-1})/P_{t-1}
d$ret <- diff(d$AAPL.Close)/lag(d$AAPL.Close)

# r_t=ln(P_t) - ln(P_{t-1})
d$r <- diff(log(d$AAPL.Close))

# r7_t=ln(P_t) - ln(P_{t-7})
d$r7 <- diff(log(d$AAPL.Close),lag=7)

plot(d$r)
acf(d$r)
acf(d$r,na.action=na.pass)
pacf(d$r,na.action=na.pass)

acf(d$r,na.action=na.pass,plot=FALSE)
pacf(d$r,na.action=na.pass,plot=FALSE)

m.arma <- arima(d$r,order=c(1,0,1))
m.arma


kings <- c(60,43,67,50,56,42,50,65,68,43,65,34,47,34,49,41,13,35,53,56,16,43,69,
           59,48,59,86,55,68,51,33,
           49,67,77,81,67,71,81,68,70,77,56)
str(kings)
sum(kings)
plot(kings,type="l")

acf(kings)
pacf(kings)

acf(kings,plot=FALSE)
pacf(kings,plot=FALSE)

k.arma <- arima(kings,order=c(1,0,1))
k.arma

k.ar <- arima(kings,order=c(1,0,0))
k.ar
resid(k.ar) # остатки модели
kings

f <- predict(k.ar,n.ahead=5)
f$pred
f$se
f$pred-2*f$se
f$pred+2*f$se

str(kings)
kings[42]
coefs <- coef(k.ar)
coefs[2]
coefs[1]
coefs[2]-coefs[1]*coefs[2]+coefs[1]*kings[42]

d$r2 <- d$r^2
model <- arima(d$r2,order=c(1,0,1))
model

mg <- garchFit(formula=~arma(1,1)+
        garch(1,1), data=na.omit(d$r))
mg


predict(mg,n.ahead=7)
predict(mg,n.ahead=7,mse="uncond") # прогноз без учета условной гетероскедастичности

