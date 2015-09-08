# em301, 08.09.2015, семинар 1

# вес для каждого из трех человек
x <- c(59, 45, 87)
# рост для каждого из трех человек
y <- c(173, 162, 178)

# оценка для модели $\hat y_i = \hat \beta x_i$
sum(x*y)/sum(x^2)

# оформляем в виде таблички с данными
df <- data.frame(rost=y, ves=x)
df

# оцениваем модель $\hat y_i = \hat \beta x_i$
model <- lm(data=df, rost~0+ves)
summary(model)

# оцениваем модель $\hat y_i = \hat \beta_1 + \hat \beta_2 x_i$
model <- lm(data=df, rost~ves)
summary(model)

# добавляем в таблицу df квадрат переменной ves
df$ves2 <- df$ves^2

# оцениваем модель $\hat y_i = \hat \beta_1 + \hat \beta_2 x_i + \hat \beta_3 x_i^2$
model <- lm(data=df, rost~ves+ves2)
summary(model)

