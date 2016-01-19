
library(erer)
library(bstats)
library(sandwich)

# файл с данными доступен по ссылке
# goo.gl/zL5JQ
h <- read.table(
  "../datasets/flats_moscow.txt",
  header=TRUE)
head(h)

qplot(totsp,price,data=h)

# тест Уайта ручками
model <- lm(price~totsp,data=h)
summary(model)
res <- resid(model)
h$res2 <- res^2
h$totsp2 <- h$totsp^2
head(h)
vspom <- lm(res2~totsp+totsp2,
            data=h)
summ <- summary(vspom)
summ$r.squared*nrow(h)
str(h)
summary(vspom)
qchisq(0.95,df=2)

# автоматически
white.test(model)

# тест Голдфельда-Квандта ручками
h.ord <- h[order(h$totsp),]
h.ord$totsp
h$totsp

mod1 <- lm(price~totsp,
           data=h.ord[1:510,])
mod2 <- lm(price~totsp,
           data=h.ord[1531:2040,])
deviance(mod2)/deviance(mod1)

qf(0.95,df1=508,df2=508)

# автоматически
m.ord <- lm(price~totsp,data=h.ord)
gqtest(m.ord,fraction=0.5,
       order.by=~totsp,data=h.ord)
?gqtest
gqtest

# неправильная оценка ковариационной матрицы
vcov(model)
coeftest(model)
confint(model)
# правильная оценка ковариационной матрицы
vcovHC(model)
coeftest(model,vcov.=vcovHC(model))
tabl <- coeftest(model,vcov.=vcovHC(model))
ci <- data.frame(estimate=tabl[,1],
                 se=tabl[,2],
                 left=tabl[,1]-1.96*tabl[,2],
                 right=tabl[,1]+1.96*tabl[,2])
ci

# борьба с гетероскедастичностью
# модель в логарифмах
qplot(log(totsp),log(price),
      data=h)
qplot(totsp,price,data=h)

h$ln_price <- log(h$price)
h$ln_totsp <- log(h$totsp)
mod3 <- lm(ln_price~ln_totsp,
           data=h)
white.test(mod3)
# лучше, но гетероскедастичность осталась

# поделить каждое наблюдение на корень из
# предполагаемой дисперсии

# для этого указываются веса
# вес должен быть равен weights=1/(предполагаемая дисперсия)
# mod4 <- lm(price~totsp,
#            weights=...,data=h)


# outlier
qplot(h$totsp,
      resid(model)^2)

which(resid(model)^2>150000)
h[967,]

summary(h)
qplot(x=factor(floor),y=price,
      geom="violin",data=h)

