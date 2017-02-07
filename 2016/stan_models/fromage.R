library(rstan)
library(bayesplot)

fro_data <- list(N = 3, y = c(9, 18, 12))

fro_model <- stan_model(file = "fromage.stan")

fro_fit <- sampling(fro_model, 
                        data = fro_data)

fro_fit

fro_array <- as.array(fro_fit)
mcmc_hist(fro_array)


