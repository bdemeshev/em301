library(rstan)
library(bayesplot)

data <- list(y = c(15, 5, 80))

model <- stan_model(file = "multinomial2.stan")

fit <- sampling(model, data = data, seed = 42) 

?sampling
fit

fit_array <- as.array(fit)
mcmc_hist(fit_array)

mcmc_trace(fit_array, pars = "a")
mcmc_violin(fit_array, pars = "a")
mcmc_hist(fit_array, pars = "y_new[3]")


fit_array[ , 2, 1] # 2-ая цепь, 1-ый параметр (а)

library(shinystan)
launch_shinystan(fit)
