require(plm)

# загружаем встроенный набор данных Grunfeld
data("Grunfeld")
summary(Grunfeld)
head(Grunfeld)
?Grunfeld

# задаем структуру панели --- указываем индексы i и t
g <- pdata.frame(Grunfeld,c("firm","year"))
?pdata.frame
head(g)

# оцениваем Pooled модель
m.pooled <- plm(inv~value+capital,data=g,
                model="pooling")
summary(m.pooled)
?plm

# оцениваем Pooled модель руками по старинке
m.pooled2 <- lm(inv~value+capital,data=g)
summary(m.pooled2)

# оцениваем FE модель
m.fe <- plm(inv~value+capital,data=g,
            model="within")
summary(m.fe)
# вытаскиваем фиксированные эффекты
fe <- fixef(m.fe)
summary(fe)

# оцениваем FE модель руками по старинке
m.fe2 <- lm(inv~0+capital+value+as.factor(firm),
            data=g)
summary(m.fe2)

# оцениваем RE модель
m.re <- plm(inv~capital+value,
            data=g,model="random")
summary(m.re)

## тесты

# тест Pooled vs RE
# H0: Pooled
plmtest(m.pooled,type="bp")


# тест Pooled vs FE
# H0: Pooled
pFtest(m.fe,m.pooled)


# тест Хаусмана, FE vs RE
# H0: RE
phtest(m.fe,m.re)
?phtest
