library(forecast)

# создадим регулярный временной ряд
# белый шум: независимые N(0;9)
# месячные данные с февраля 2001
# ts — регулярный временной ряд
set.seed(1)
y <- ts(rnorm(40, mean = 0, sd = 3), 
  start = c(2001, 2), frequency = 12)
ggtsdisplay(y)

# для сравнения реальный ряд
ggtsdisplay(LakeHuron)

