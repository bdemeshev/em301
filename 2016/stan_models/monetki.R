library(tidyverse)
library(rstan)
library(bayesplot)

model <- stan_model(file = "monetki.stan")

monetki_data <- list(N = 2, y = c(1, 1))

fit <- sampling(model, data = monetki_data, seed = 42)



?vb
fit

fit_array <- as.array(fit)
fit_array

mcmc_hist(fit_array)

# approximate ADVI 
fit_advi <- vb(model, data = monetki_data, seed = 43, iter = 10000)
fit_advi

