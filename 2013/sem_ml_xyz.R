# семинар x

lnL <- function(p) {
  return(25*log(p[1]) +
    40*log(p[2])+35*log(1-p[1]-p[2]))
}

lnL(c(0.2,0.3))

LR <- 2*(lnL(c(0.25,0.4)) -
           lnL(c(0.2,0.3)))
LR

qchisq(0.95,df=2)


minus_lnL <- function(p) {  
  return(-lnL(p))
}

result <- nlm(minus_lnL,c(0.2,0.3),hessian=TRUE)

p.hat <- c(0.25,0.4)
p0 <- c(0.2,0.3)

I.hat <- result$hessian

I.hat

W <- t(p.hat-p0)%*%I.hat%*%(p.hat-p0)
W

Var.hat <- solve(I.hat)
Var.hat
se <- sqrt(Var.hat[1,1])
p.hat[1]+2*se
p.hat[1]-2*se

# семинар y

hat.Ia.H0 <-  matrix(c(25/0.2^2+35/0.5^2,35/0.5^2,
                       35/0.5^2,40/0.3^2+35/0.5^2),
                     nrow=2)
hat.Ia.H0

n<-100

hat.Ib.H0 <-  matrix(c(n/0.2+n/0.5,n/0.5,
                       n/0.5,n/0.3+n/0.5),
                     nrow=2)
hat.Ib.H0

hat.Ia.H0

gr <- c(25/0.2-35/0.5,40/0.3-35/0.5)

gr
LMa <- t(gr) %*% solve(hat.Ia.H0) %*% gr
LMa

LMb <- t(gr) %*% solve(hat.Ib.H0) %*% gr
LMb


# семинар z

Xobs <- c(15,20)

lnL <- function(p,X) {
  ans <- X[1]*log(p[1])+X[2]*log(p[2])+
    (100-X[1]-X[2])*log(1-p[1]-p[2])
  return(ans)  
}

lnL(c(0.3,0.3),Xobs)
lnL(c(0.2,0.25),Xobs)

minus_lnL <- function(p,X) {
  return(-lnL(p,X))
}

result <- nlm(minus_lnL,c(0.3,0.3),X=Xobs,
              hessian=TRUE)

result
solve(result$hessian)



