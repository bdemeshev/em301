library(forecast)

y0 = arima.sim(model = list(order = c(0, 0, 0)), n = 200)
ggtsdisplay(y0)
acf(y0, plot = FALSE)

y1 = arima.sim(model = list(ma = 0.5, order = c(0, 0, 1)), n = 200)
ggtsdisplay(y1)

y2 = arima.sim(model = list(ma = c(0.5, 0.3), order = c(0, 0, 2)), n = 200)
ggtsdisplay(y2)

y3 = arima.sim(model = list(ar = 0.6, order = c(1, 0, 0)), n = 200)
ggtsdisplay(y3)

y4 = arima.sim(model = list(order = c(0, 1, 0)), n = 199)
ggtsdisplay(y4)
length(y4)

dy4 = diff(y4)
ggtsdisplay(dy4)

trend = 1:200
model = lm(y4 ~ trend)
ry4 = resid(model)
ggtsdisplay(ry4)

y5 = arima.sim(model = list(order = c(0, 1, 1), ma = 0.5), n = 199)
ggtsdisplay(y5)

dy5 = diff(y5)
ggtsdisplay(dy5)

model = lm(y5 ~ trend)
ry5 = resid(model)
ggtsdisplay(ry5)



y6 = 0.1 * (1:200) + arima.sim(model = list(order = c(0, 0, 0)), n = 200)
ggtsdisplay(y6)

dy6 = diff(y6)
ggtsdisplay(dy6)

y7 = 0.1 * (1:200) + arima.sim(model = list(ma = 0.5, order = c(0, 0, 1)), n = 200)
ggtsdisplay(y7)

model = lm(y1 ~ y2)
summary(model)


model = lm(y4 ~ y5)
summary(model)


