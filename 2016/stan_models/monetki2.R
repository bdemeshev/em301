library(rstan)
library(bayesplot)

monetki_data <- list(N = 2, y = c(1, 1))

monetki_model <- stan_model(file = "monetki2.stan")

monetki_fit <- sampling(monetki_model, 
                        data = monetki_data)

monetki_fit

monetki_array <- as.array(monetki_fit)
mcmc_hist(monetki_array)
