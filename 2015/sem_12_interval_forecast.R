library("dplyr")

# оценим простенькую модель
model <- lm(data=cars, dist~speed)
summary(model)

# создадим новый набор данных
new <- data_frame(speed = c(20, 50, 60))

new

# точечный прогноз
predict(model, new)

# доверительный интервал для среднего значения зависимой переменной
predict(model, new,
        interval = "confidence")

# предиктивный интервал для конкретного будущего значения
predict(model, new,
        interval = "prediction")

# дополнительные опции при построении интервала
predict(object = model, 
        newdata = new, 
        level = 0.999,
        se.fit = TRUE,
        interval = "confidence")




