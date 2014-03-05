require(maxLik)

# лог. функция правдоподобия с 
# одним параметром
lnL <- function(t) {
  ans <- log(2)+
    3*log(t)+
    log(1-3*t)
  return(ans)
}
lnL(0.5)
lnL(0.1)
lnL(0.15)
lnL(0.005)

res <- maxLik(lnL,
    start=0.1)
summary(res)

# лог. функция правдоподобия в задаче
# об Эмэндэмсинах :)
alexandra <- function(t) {
  ans <- 22*log(t[1])+
    33*log(t[2])+45*log(1-t[1]-t[2])
  return(ans)
}
alexandra(c(0.3,0.4))
alexandra(c(2,7))

rs <- maxLik(alexandra,
      start=c(0.1,0.2))

# отчет о максимизации
summary(rs)

# оценки параметров
rs$estimate

# матрица вторых производных
rs$hessian

# оценка ковариационной матрицы
(-1)*solve(rs$hessian) 

# корень из диагонали оценки ков. матрицы
# или стандартные ошибки оценок коэффициентов
sqrt(diag((-1)*solve(rs$hessian) ))
# совпадают с Std. error в summary()

# оценки параметров в UR модели
t.ur <- rs$estimate
t.ur

# макс. значение лог. правдоподобия
# в UR модели
l.ur <- rs$maximum
l.ur

# ограниченная модель
# t1+t2=0.5
venera <- function(t) {
  ans <- 22*log(t)+
    33*log(0.5-t)+45*log(0.5)
  return(ans)
}
rs2 <- maxLik(venera,start=0.1)
summary(rs2)

# оценки параметров в R модели
t.r <- rs2$estimate
t.r

# макс. значение лог. правдоподобия
# в R модели
l.r <- rs2$maximum
l.r

LR <- 2*(l.ur - l.r)
LR

# критическое значение хи-квадрат
# статистики с одной степенью свободы
qchisq(0.95,df=1)

# вывод: H0 p1+p2=0.5 не отвергается

# простая гипотеза
# p2 = 0.2
rs3 <- maxLik(alexandra,
      start=c(p1=0.1,p2=0.2),
        fixed="p2")
summary(rs3)

# максимум лог. правдоподобия в 
#  R модели
l.r <- rs3$maximum

LR <- 2*(l.ur-l.r)
LR
# H0: t2=0.5 отвергается