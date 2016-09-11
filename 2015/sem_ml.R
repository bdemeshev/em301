library("maxLik")

ell <- function(gamma, data) {
  p1 <- exp(gamma[1]) / (1 + exp(gamma[1]))
  p2 <- exp(gamma[2]) / (1 + exp(gamma[2]))
  p3 <- 1 - p1 - p2
  result <- data[1] * log(p1) + data[2] * log(p2) +
    data[3] * log(p3)
  return(result)
}
g <- c(-1,-1)
x <- c(90, 50, 60)
ell(g, x)

model <- maxLik(ell, start = c(-1, -1), data = x)
summary(model)
gamma_ur <- coef(model)
p_ur <- plogis(coef(model))

H_ur <- hessian(model)
l_ur <- logLik(model)
p_r <- c(0.5, 0.25)
gamma_r <- qlogis(p_r)
gamma_r
gamma_ur
l_r <- ell(gamma_r, data = x)
l_r
LR <- 2 * (l_ur - l_r)
LR

dl_dg <- numericGradient(ell, t0 = gamma_r, data = x)
H_r <- numericHessian(ell, t0 = gamma_r, data = x)
H_r
dl_dg

LM <- dl_dg %*% solve(- H_r) %*% t(dl_dg)
LM

delta <- gamma_ur - gamma_r
delta
W <- t(delta) %*% (-H_ur) %*% delta
W

chi_crit <- qchisq(0.95, df = 2)
chi_crit
