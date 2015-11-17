# problem 3.27
F_obs <- (0.894-0.851)/1/0.851*(27-3)
F_obs
F_crit <- qf(0.95, df1=1, df2=24)
F_crit

library("ggplot2")
library("car")

# сравним ограниченную модель и неограниченную
model_ur <- lm(data=diamonds,
            price~carat+x+y+z)
head(diamonds)


model_r <- lm(data=diamonds,
            price~carat)

# можно извлечь RSS и ручками посчитать F статистику
rss_ur <- deviance(model_ur)
rss_ur
# ...

# но нам лень, и мы воспользуемся готовой командой
anova(model_r, model_ur)


# а воистину ленивые даже ограниченную модель сами не оценивают
linearHypothesis(model_ur,
            c("x=0","y=0","z=0"))

# для любой модели команда summary автоматом проверяет 
# гипотезу о неадекватности регрессии
summary(model_ur)
summary(model_r)
