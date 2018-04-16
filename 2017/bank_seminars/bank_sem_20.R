library(tidyverse)
library(forecast)

x <- 5
cos(x)

y <- c(5, 6, 7)
cos(y)

model <- lm(data = cars, dist ~ speed)
AIC(model)

model_2 <- lm(data = cars, dist ~ log(speed))
AIC(model_2)

models <- list(model, model_2)
AIC(models)

# работаем с функциями :)

df <- cars
glimpse(df)

df2 <- mutate(df, ln_speed = log(speed))
glimpse(df2)

df3 <- mutate(df, ln_speed = map(speed, ~log(.)))
glimpse(df3)

df4 <- mutate(df, ln_speed = map_dbl(speed, ~log(.)))
glimpse(df4)

# упражнение 1:
# оценим модели AR(1) ... AR(6)
AirPassengers
?AirPassengers

models <- tibble(p = 1:6)
glimpse(models)

# руками оценим AR(2)
test <- Arima(AirPassengers, order = c(2, 0, 0))
test
models2 <- mutate(models, 
      model = map(p, ~Arima(AirPassengers, 
              order = c(., 0, 0))))
glimpse(models2)      
      
# вытащим из 2го столбца третью модель!
models2 %>% .$model %>% .[[3]]
models2$model[[3]]

# считаем AIC отдельной модели
AIC(models2$model[[3]])

models3 <- mutate(models2, 
      aic = map_dbl(model, ~AIC(.)))
glimpse(models3)

models3 %>% arrange(aic) %>% head(2)

# сохраняю во внутреннем формате R всё
write_rds(models3, "all_models.Rds")
models_report <- dplyr::select(models3, -model)
# сохраняем в универсальном csv только числа
write_csv(models_report, "models_info.csv")
getwd() # куда сохраняются файлы

?select
# упражнение 2
# оценим регрессию вида dist = beta_1 + beta_2 speed + u
# на разных подвыборках
all_models <- tibble(first_obs = 1:6)
glimpse(all_models)

# отберем наблюдения со 2го по последнее
cars_2 <- cars[2:nrow(cars), ]
all_models2 <- mutate(all_models,
    dataset = map(first_obs, ~cars[.:nrow(cars), ]))
glimpse(all_models2)

# строю отдельную регрессию
model <- lm(data = cars, dist ~ speed)

all_models3 <- mutate(all_models2, 
    model = map(dataset, ~lm(data = ., dist ~ speed)))
all_models4 <- mutate(all_models3,
    aic = map_dbl(model, ~AIC(.)))
glimpse(all_models4)
all_models4

summary(model)
all_models5 <- mutate(all_models4,
    report = map(model, ~summary(.)))
all_models5

all_models5$report[[2]]$r.squared
all_models6 <- mutate(all_models5,
    r2 = map_dbl(report, ~.$r.squared))
all_models6

# CausalImpact — оценить эффект от разового вмешательства
library(CausalImpact)
library(lubridate)
AirPassengers
ggtsdisplay(AirPassengers)

air <- AirPassengers
set.seed(777)
air[71:144] <- air[71:144] + rnorm(74, mean = 10, sd = 2)
ggtsdisplay(air)

dates <- ymd("1949-01-01") + months(0:143)
air2 <- zoo(air, order.by = dates)

setup <- list()
setup2 <- AddLocalLevel(setup, air2)
setup3 <- AddSeasonal(setup2, air2, nseasons = 12)


post_period_response <- air[71:144]
air2[71:144] <- NA

bsts_model <- bsts(air2, 
                   state.specification = setup3,
                   niter = 1000)
model <- CausalImpact(bsts.model = bsts_model, 
      post.period.response = post_period_response)
plot(model)
?CausalImpact
summary(model)
summary(model, "report")


## вопрос про barchart
y <- c(rep("A", 10), rep("B", 20))
df <- tibble(y = y)
ggplot(df) + geom_bar(aes(x = y)) + coord_flip()
