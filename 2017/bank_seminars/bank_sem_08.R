library(cluster) # визуализация кластеров
library(ggdendro) # визуализация кластеров
library(factoextra) # визуализация кластеров
library(tidyverse) # манипуляции с данными
library(FactoMineR) # набор данных wine

data(wine)
glimpse(wine)

# удалим из набора две категориальных переменных
wine_num <- select(wine, -Label, -Soil)
glimpse(wine_num)

# проведем нормировку всех переменных
wine_norm <- scale(wine_num)

# поделим на 3 кластера с помощью k-средних:
# алгоритм k-средних случайный, 
# поэтому зададим зерно генератора случайных чисел
# чтобы при повторе скрипта был тот же результат
set.seed(1804) 
wine_clust3 <- kmeans(wine_norm, 3)

# смотрим на итоги кластеризации
wine_clust3

# достаем общую сумму квадратов
wine_clust3$totss

# достаем сумму квадратов внутри каждого кластера
wine_clust3$betweenss

# достаём сумму квадратов между кластерами
wine_clust3$betweenss 

# общая сумма квадратов всегда разлагается на отдельные слагаемые
# total SS = sum(within SS) + between SS
sum(wine_clust3$withinss) + wine_clust3$betweenss 

# извлечём кластеры и добавим их в исходную выборку
wine2 <- mutate(wine, cluster = wine_clust3$cluster)

# справка по функции hclust
?hclust

# находим все расстояния между точками
wine_dist <- dist(wine_num, method = "euclidean")
wine_dist

# строим иерархическую кластеризацию
wine_hclust <- hclust(wine_dist)

# две простых визуализации:
plot(wine_hclust)
ggdendrogram(wine_hclust, rotate = TRUE)

# мы только что построили дендрограмму для наблюдений
# можем аналогично построить дендрограмму для переменных
wine_dist_t <- dist(t(wine_num), method = "euclidean")
wine_hclust_t <- hclust(wine_dist_t)
ggdendrogram(wine_hclust_t, rotate = TRUE)
