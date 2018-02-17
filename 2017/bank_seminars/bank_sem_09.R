library(tidyverse) # манипуляции с данными
library(FactoMineR) # отсюда возьмем набор данных wine

data(wine) # активируем набор данных wine
glimpse(wine) # смотрим на количество наблюдений...

# удаляем категориальные переменные:
wine_num <- select(wine, -Label, -Soil)

# отмасштабируем!
wine_scaled <- scale(wine_num)

# считаем расстояния между каждой парой наблюдений:
wine_dist <- dist(wine_scaled)
# строим дендрограмму
wine_hclust <- hclust(wine_dist)
plot(wine_hclust)
# вытащим 3 кластера с дерева:
clust <- cutree(wine_hclust, 3)
clust

# делаем кластеризацию k средних
set.seed(18)
wine_3clust <- kmeans(wine_scaled, 3)
wine_3clust
# вытащим 3 кластера от метода k-средних
clust_b <- wine_3clust$cluster
clust_b

# сравним кластеры
table(clust, clust_b)

# метод главных компонент: 
wine_pc <- prcomp(wine_scaled)
wine_pc
plot(wine_pc)

wine_ext <- mutate(wine_num, 
        pc1 = wine_pc$x[, 1],
        pc2 = wine_pc$x[, 2],
        clust_h = cutree(wine_hclust, 3),
        clust_km = wine_3clust$cluster)
glimpse(wine_ext)
qplot(data = wine_ext, 
      x = pc1, y = pc2,
      color = factor(clust_h),
      shape = factor(clust_km))


