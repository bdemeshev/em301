library("ggplot2")
library("dplyr")

b <- 0.2 # proportion of correct bees
n_sim <- 300

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

shinyServer(function(input, output) {

  update_beta_hat <- reactive({
    beta_hat <- matrix(NA, nrow = n_sim, ncol = 2)
    for (i in 1:n_sim) {
      beta_hat[i, ] <-
        one_simulation(input$n_obs, b, input$beta_2)
    }
    beta_hat
  })

  output$beta_hat_plot <- renderPlot({
    beta_hat <- update_beta_hat()
    qplot( beta_hat[, 2])
  })

  output$beta_hat_top <- renderTable({
    beta_hat <- update_beta_hat()
    head(beta_hat)
  })

})
