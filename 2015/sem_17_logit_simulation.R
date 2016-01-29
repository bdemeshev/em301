library("ggplot2")
library("dplyr")
library("shiny")

n_obs <- 500
b <- 0.2 # proportion of correct bees

one_simulation <- function(n_obs, b) {
  bee <- rbinom(n_obs,
    size = 1, prob = b)
  winnie_data <-
    data_frame(bee = bee)

  winnie_data <- mutate(winnie_data,
    honey_x = -1 + 3 * bee + rlogis(n_obs),
    honey = ifelse(honey_x > 0, 1, 0)
  )

  model <- glm(data = winnie_data,
    honey~bee,
    family = binomial(link = "logit"))
  return(coef(model))
}
one_simulation(100, 0.2)

n_sim <- 300
result <- matrix(NA, nrow = n_sim, ncol = 2)
result
for (i in 1:n_sim) {
  result[i, ] <- one_simulation(n_obs, b)
}
qplot(result[ , 2])

