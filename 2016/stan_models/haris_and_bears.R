library(tidyverse)
library(rstan)
library(bayesplot)

model <- stan_model(file = "haris_and_bears.stan")

bears_data <- list(N = 2, y = c(26, 11))

fit <- sampling(model, data = bears_data)
fit



fit_array <- as.array(fit)
fit_array

mcmc_hist(fit_array)

