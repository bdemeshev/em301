library(tidyverse) # data manipulation packages collection
library(broom) # representation of many models as tables
library(rio) # import / export data 

# difference between vector and list
korzinka <- c(6, 7, -3, 4, 9)
korzinka[2]

chulan <- list(6, 7, korzinka, cars)
chulan[[1]]
chulan[[3]]
chulan[[3]][5]

chulan2 <- list(a = 6, b = 7, vector = korzinka, df = cars)
chulan2[["vector"]]
chulan2[[3]]

chulan2[["df"]]

# built-in data frame "cars":
glimpse(cars)

# estimate linear model
model <- lm(data = cars, dist ~ speed)

# extract coefficient beta hat before speed variable
model[["coefficients"]][["speed"]]

model_summ <- summary(model)
model_summ

coef_table <- tidy(model)
coef_table
overview_table <- glance(model)
overview_table

# from model_summ
r2a <- model_summ[["r.squared"]]
r2a 
# from overview_table
r2b <- overview_table$r.squared
r2b


# writing functions :)
# f(x) = 1/x
f <- function(x) {
  inverse <- 1 / x
  return(inverse)
}

f(5)
f(9)

extract_r2 <- function(model) {
  overview_table <- glance(model)
  r2 <- overview_table$r.squared
  return(r2)
}

model <- lm(data = cars, dist ~ speed)
extract_r2(model)

model_2 <- lm(data = cars, log(dist) ~ log(speed))
extract_r2(model_2)

# a lot of regression for diamonds data set
glimpse(diamonds)

# calculate o lot of models

reg_table <- group_by(diamonds, clarity, cut) %>%
  do(model = lm(data = ., 
    log(price) ~ log(carat) + x + y + z)) %>%
  ungroup()

reg_table$clarity[3]
reg_table$model[[3]]

# some functions work with collections of objects
# and some don't work
cos(korzinka) # ok
f(korzinka) # ok
extract_r2(list(model, model_2)) # FAIL

reg_table2 <- mutate(reg_table, 
                     r2 = map_dbl(model, extract_r2),
                     corr_abs = sqrt(r2))
reg_table2

top <- reg_table2 %>%  top_n(3, r2)
top

# what I obtain using the following code:
top_x <- reg_table2 %>% group_by(clarity) %>%
  top_n(2, r2)
top_x

reg_table3 <- mutate(reg_table2, 
        coef_table = map(model, tidy),
        overview = map(model, glance))
reg_table3

reg_table3$coef_table[[2]]

coef_table[1, 2]

# function to extract intercept from coefficient table
extract_intercept <- function(coef_table) {
  return(coef_table[1, 2])
}

table_example <- tidy(model_2)
table_example
extract_intercept(table_example)

reg_table4 <- mutate(reg_table3, 
        intercept = map_dbl(coef_table, extract_intercept))
glimpse(reg_table4)

# export some columns to excel format
export_table <- select(reg_table4, -model, -coef_table, -overview)
glimpse(export_table)
export(export_table, "~/Downloads/results.xlsx")
# on windows you need Rtools to create xlsx files
# https://cran.r-project.org/bin/windows/Rtools/
