# em301, первый семинар в R

# Спасите-помогите, ссылки
help(glm)
??regression

# R как калькулятор




# Особые числа 

# Данные
# по типу: текстовые, логические, числа, порядковые переменные, упорядоченные, Date
# по форме: вектор, матрица, список, таблица данных

a=c("Москва","Питер","Лондон")
b=c(TRUE,TRUE,FALSE)

nn=rnorm(200,mean=7,sd=3)

sex=cfj('Male','Female','Male')
s=factor(sex)

edu=c('school','school','bac','master')
edu2=ordered(edu,labels=c('school','bac','master'))


# функция str - друг, товарищ и брат!

# работа с векторами/матрицами/таблицами данных 

A=matrix(777,nrow=3,ncol=4)

# имена строк/столбцов

e=rnorm(n=200,mean=0,sd=6)
x=rnorm(n=200,mean=3,sd=5)
y=2+10*x+e
plot(x,y)
A=cbind(y,x,e) 

A[,"x"]

d=data.frame(A)

d$x

summary(d)


# Выбор элемента, строки, столбца, нескольких столбцов
# склеивание
# размер вектора/матрицы
# выбор части вектора


# удалить объект


# Загрузить данные
# из файла (csv, stata, spss) или с сайта

# Оценивать glm разные модели


model1=glm(y~x,data=d)

summary(model1)

