library("shiny")

shinyUI(pageWithSidebar(
 headerPanel("Эксперименты от Винни-Пуха"),
 sidebarPanel(
   sliderInput("beta_2", "Коэффициент бета 2",
      min = -5, max = 5, value = 3, step = 0.2,
      animate = TRUE),
   sliderInput("n_obs", "Число наблюдений",
      min = 20, max = 500, value = 100, step = 10.2,
      animate = TRUE)
 ),
 mainPanel(
   plotOutput("beta_hat_plot"),
   tableOutput("beta_hat_top")
 )
))
