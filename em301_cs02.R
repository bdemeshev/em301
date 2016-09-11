setwd('/home/boris/science/econometrix/sem2_ec2/')
d=read.csv('tvims2012_data.csv',encoding="UTF-8")
n=read.csv('names_sex.csv',encoding="UTF-8")

# проверка целостности данных
str(d)
str(n)

n.obs=nrow(d) # узнаём число наблюдений в d
d$one.m=0 # создаём нулевую переменную в табличке d

# тут лучше было merge использовать, смотрите как Хрюша в своей домашке сделал.
for (i in 1:n.obs) {
  d$one.m[i]=n$male_name[n$vec_names==d$name[i]]
}
  

d$one.f=1-d$one.m

# оцениваем модели
m1=glm(kr2~one.m,data=d)
summary(m1)

# Что R автоматом считает при оценке модели?
m1$coefficients # оценки коэффициентов
m1$residuals # остатки
m1$fitted.values # прогнозы,\hat{y}
m1$deviance # так RSS называется

# Что ещё можно раздобыть?
vcov(m1) # ковариационная матрица коэффициентов, \hat{\sigma}^2 (X'X)^{-1}

m2=glm(kr2~one.f,data=d)
summary(m2)

m3=glm(kr2~0+one.f+one.m,data=d)
summary(m3)

m4=glm(kr2~one.f+one.m,data=d)
summary(m4)

m5=glm(kr2~group,data=d)
summary(m5)

m6=glm(kr2~0+group,data=d)
summary(m6)
