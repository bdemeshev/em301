library("lmtest") 
library("Ecdat") 
library("dplyr") 
library("erer")

x <- c(0,1,-1)
y <- c(0,1,4)
var(y)*2

m0 <- lm(y~0+x)
m1 <- lm(y~x)
summary(m1)
summary(m0)
deviance(m0)

h <- na.omit(BudgetFood)

model <- lm(wfood~totexp+size,data=h)
summary(model)
confint(model, level=0.9)
nd <- data.frame(size=4,totexp=700000)
predict(model, newdata = nd, interval = "prediction",level = 0.9)

model_ur <- lm(wfood~totexp+size+size:sex+totexp:sex,data=h)
waldtest(model,model_ur)


d <- Doctor
d$y <- (Doctor$doctor>0)
mlogit <- glm(data=d, y~access+health+children, 
              family=binomial(link="logit") , x=TRUE)
summary(mlogit)
confint(mlogit, level=0.9)

maBina(mlogit)

ndl <- data.frame(children=2, health=0, access=0.5)

a <- predict(mlogit, newdata= ndl, type = "response", se.fit = TRUE)

a$fit-1.96*a$se.fit
a$fit+1.96*a$se.fit
a
