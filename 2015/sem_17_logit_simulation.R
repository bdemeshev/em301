library("ggplot2")
library("dplyr")
library("shiny")
library("mfx")

n_obs <- 500
b <- 0.2 # proportion of correct bees

one_simulation <- function(n_obs, b, beta_2) {
  bee <- rbinom(n_obs,
    size = 1, prob = b)
  winnie_data <-
    data_frame(bee = bee)

  winnie_data <- mutate(winnie_data,
    honey_x = -1 + beta_2 * bee + rlogis(n_obs),
    honey = ifelse(honey_x > 0, 1, 0)
  )

  model <- glm(data = winnie_data,
    honey ~ bee,
    family = binomial(link = "logit"))
  return(coef(model))
}

one_simulation(100, 0.2, 3)

n_sim <- 300
beta_hat <- matrix(NA, nrow = n_sim, ncol = 2)

for (i in 1:n_sim) {
  beta_hat[i, ] <- one_simulation(n_obs, b, 3)
}
qplot(beta_hat[ , 2])

