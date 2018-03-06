library(tidyverse) # манипуляции с данными
library(mfx) # предельные эффекты
# library(PRROC) # пакет для ROC-кривой
library(pROC) # пакет для ROC-кривой

glimpse(diamonds) # осмотрим набор данных
head(diamonds$cut) # начало набора данных
tail(diamonds$cut) # хвост набора данных
tail(diamonds$cut, 10) # хвост набора данных

# руками сделаем бинарную переменную 
# для набора данных diamonds
d2 <- mutate(diamonds,
    ideal = ifelse(cut == "Ideal", 1, 0))

# == — это сравнение :)
5 == 6
5 = 6
5 == 5
3 + 1 == 4
0.3 + 0.1 == 0.4 
4 - 1 == 3
0.4 - 0.1 == 0.3 # здесь ошибка округления накапливается

# предельные эффекты для среднестатистического бриллианта
model <- logitmfx(data = d2, 
    ideal ~ price + carat + x + y + z)
# help("diamonds")
model 

# усредненный предельный эффект по всем бриллиантам
model_2 <- logitmfx(data = d2, atmean = FALSE, 
    ideal ~ price + carat + x + y + z)
model_2

# вытаскиваем исходные бета с крышкой
summary(model$fit)

# экспоненты от всех бета с крышкой
exp(coef(model$fit))

# прогнозирование
new_diamonds <- tibble(
  price = c(300, 500), carat = c(0.3, 0.3),
  x = c(3, 4), y = c(3, 4), z = c(3, 2)
)
new_diamonds

# прогноз аргумента логистической функции
# это ещё не вероятность :)
predict(model$fit, 
        newdata = new_diamonds)

# прогноз вероятности P(y=1)
predict(model$fit, newdata = new_diamonds,
        type = "response")

# проверка 
predict(model$fit, 
        newdata = new_diamonds) %>% plogis()

# добавим прогноз вероятности в исходный набор данных
d3 <- mutate(d2, 
  prob_fit = predict(model$fit, type = "response"))
glimpse(d3)
d4 <- mutate(d3, 
  ideal_fit = ifelse(prob_fit > 0.5, 1, 0))

# табличка сопряженности для фактических ideal
# и прогнозных ideal_fit
table(d4$ideal, d4$ideal_fit)

# сравнение двух моделей
# оценили более простую модель (без размеров x, y, z)
model_r <- logitmfx(data = d2, 
  ideal ~ price + carat)
# сравниваем
waldtest(model_r$fit, model$fit, test = "Chisq")
# H0: обе модели верны
# Ha: верна только неограниченная
# p_value < alpha (уровень значимости) 5 %
# вывод: H0 отвергается


roc_ur <- roc(d3$ideal, d3$prob_fit)
plot(roc_ur)
# google: ROC-кривая
