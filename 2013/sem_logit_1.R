require(maxLik)

lnL <- function(beta) {
  answ <- 65*beta[1]+50*beta[2]-30*log(1+exp(beta[1]))-70*log(1+exp(beta[1]+beta[2]))
  return(answ)
}

lnL(c(0,0))
lnL(c(0,log(2.5)))

model <- maxLik(lnL,start=c(5,7))
summary(model)
log(2.5)

help(numericGradient)

numericGradient(lnL,c(0,0))
numericNHessian(lnL,c(0,0))

model$code
help(maxLik)

z.cr <- qnorm(0.975)
z.cr
hat.beta <- model$estimate
se <- sqrt(diag(solve(-model$hessian)))

intervals <- data.frame(hat.beta,
                left=hat.beta-z.cr*se,
                right=hat.beta+z.cr*se)
intervals


LR <- 2*(lnL(hat.beta)-lnL(c(0,0)))
LR
LR.cr <- qchisq(0.95,df=2)
LR.cr

mtcars
help(mtcars)

model2 <- glm(am~wt+hp,data=mtcars,
              family=binomial(link="logit"))
summary(model2)




