# encoding = utf8

# устанавливаем рабочую папку
setwd("/home/boris/science/econometrix/em502/housing/")

# библиотека для чтения чуждых R форматов (stata, spss, etc)
# без нее R не знает функции read.dta
library(foreign)

# читаем файл в табличку h
h=read.dta("housing.dta")

# начало таблички h
head(h)

# описательные статистики для переменных из h
summary(h)

# гистограмма
hist(h$bedrooms)

# табличка с частотами
table(h$bedrooms)

# двумерный график
plot(h$lotsize,h$price,main="График")

hist(h$price)

hist(log(h$price))

plot(log(h$lotsize),log(h$price))

# отбираем случайные строки из h в h.sel
sel=sample(1:546,500)
h.sel=h[sel,]

# модель 1
m1=glm(price~lotsize,data=h.sel)
summary(m1)

# модель 2
m2=glm(price~lotsize+bedrooms,data=h.sel)
summary(m2)

# модель 3
m3=glm(h.sel)
summary(m3)



