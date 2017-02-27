library(tidyverse)
library(rstan)
library(bayesplot)
library(sophisthse)

model <- stan_model(file = "ar_population.stan")

popnum <- sophisthse("POPNUM_Y")
plot(popnum)

data = list(y = as.vector(popnum),
            N = length(popnum), 
            H = 2)

fit <- sampling(model, data = data)
fit

fit_array <- as.array(fit)
fit_array

sigma_chain_2 <- fit_array[, 2, "sigma"]
sigma_all_chains <- fit_array[, , "sigma"]
str(sigma_all_chains)

mcmc_hist(fit_array)

mcmc_trace(fit_array, pars = c("y_new[1]", "beta_2"))
