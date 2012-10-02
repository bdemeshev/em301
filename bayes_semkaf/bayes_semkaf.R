library(mvtnorm) # package for generate multivariate normal
library(rjags) # package for interaction with jags

setwd('/home/boris/science/stat_methods/bayes_semkaf/')

### Загружаем данные

d1=read.table('youngmennonworking.csv',header=TRUE,sep=';')

names(d1) # выводим имена доступных переменных


# оставляем только регрессоры и зависимую переменную
YX=na.omit(as.matrix(d1[c('Smoker','Age','Child','Smokepar')]))

# оставляем только регрессоры
X1=as.matrix(YX[,c('Age','Child','Smokepar')])

# зависимые переменные
y1=YX[,'Smoker']+1 # единичка добавлена чтобы превратить 0 и 1 в 1 и 2
# y1=d1$Ordbeer


# считаем число зависимых переменных
k1=ncol(X1)

# узнаем максимум у 
max1=max(y1)

# считаем число наблюдений
n.obs=nrow(X1) 




################################### estimation



################### equation ordered probit 
# ordered probit or simple probit
jags.data <- list(y1=y1, X1=X1, N=n.obs, m1=max1, k1=k1) # data available for estimation

inits <- list("tau1u" = 1:(max1-1))
# Pass Model to JAGS
m1 <- jags.model(file="ordered_probit.bug", data=jags.data, inits=inits, n.chains=2, n.adapt=2000) 

# Burn in
update(m1, n.iter=5000) 

# Run Sampler
m1.out <- coda.samples(model=m1, variable.names=c("b1","tau1"), n.iter=5000) 

# visualisation
summary(m1.out)
plot(m1.out)


#### comparison
library(MASS)
y1factor=ordered(y1,levels=1:max1)
res_comp <- glm(y1factor~X1, family=binomial(link="probit"))

# res_comp <- polr(y1factor ~ X1, method="probit")
summary(res_comp)

