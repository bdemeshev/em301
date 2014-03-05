require(erer)
require(vcd)

z <- seq(-5,5,by=0.05)
fnorm <- dnorm(z,mean=0,sd=pi/sqrt(3))
flog <- dlogis(z)

qplot(z,fnorm,geom="line")

qplot(z,flog,geom="line")+
  geom_line(y=fnorm,color="red")

h <- read.table("train.csv",
                header=TRUE,
                sep=",")
str(h)
summary(h)
summary(h$Survived)
summary(h$Age)
nrow(h)

mosaic(data=h,~Sex+Pclass+Survived,
       shade=TRUE)

qplot(Age,data=h,geom="density")
qplot(Age,fill=factor(Survived),
      data=h,geom="density",
      alpha=0.5)

str(h$Survived)

mod1 <- glm(data=h,family=binomial(),x=TRUE,
            Survived~Sex+Age+Pclass+SibSp+Parch)
summary(mod1)
maBina(mod1)
summary(h$Sex)

new <- data.frame(Sex=c("female","male"),
                  Age=c(22,29),
                  Pclass=c(1,3),
                  SibSp=c(1,2),
                  Parch=c(0,2))
new
predict(mod1,newdata=new,type="response")
predict(mod1,newdata=new)
prognoz <- predict(mod1,newdata=new,type="response",
        se.fit=TRUE)
new$prob <- prognoz$fit
new$prob.se <- prognoz$se.fit
new
new$left <- new$prob-1.96*new$prob.se
new$right <- new$prob+1.96*new$prob.se
new

# probit
mod2 <- glm(data=h,family=binomial(link="probit"),
            x=TRUE,
            Survived~Sex+Age+Pclass+SibSp+Parch)
summary(mod2)
summary(mod1)

comparison <- data.frame(logit=coef(mod1),
                         probit=coef(mod2))
comparison$prob2 <- comparison$probit*pi/sqrt(3)
comparison
pi/sqrt(3)

eff1 <- maBina(mod1)
eff2 <- maBina(mod2)
comp2 <- data.frame(logit=eff1$out$effect,
                    probit=eff2$out$effect,
                    vars=rownames(eff1$out)
                    )
comp2

# forbidden OLS
mod3 <- glm(data=h,
            x=TRUE,
            Survived~Age+Pclass+SibSp+Parch)
predict(mod3,new)
new2 <- data.frame(Age=30,Pclass=2,SibSp=1,
                   Parch=1)
predict(mod3,new2)
mod4 <- glm(data=h,family=binomial(),
            x=TRUE,
            Survived~Age+Pclass+SibSp+Parch)
predict(mod4,new2,type="response")
