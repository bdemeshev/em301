# Карты, деньги, два ствола

Решим несколько типичных задач, связанных с картами

## Раскрасим регионы России

Загружаем данные c [www.gadm.org](www.gadm.org)

```r
require(sp)
require(maptools)
require(rgdal)

# скачаем с сайта gadm данные по России с детализацией по регионам:
rus <- url("http://www.gadm.org/data/rda/RUS_adm1.RData")
load(rus)
# по умолчанию получаем полигоны всех регионов в объекте gadm
```


Можно посмотреть названия регионов:

```r
print(gadm$NAME_1)
```

```
##  [1] Karachay-Cherkess      Karelia                Kemerovo              
##  [4] Khabarovsk             Khakass                Khanty-Mansiy         
##  [7] Adygey                 Aga Buryat             Altay                 
## [10] Amur                   Arkhangel'sk           Astrakhan'            
## [13] Bashkortostan          Belgorod               Bryansk               
## [16] Buryat                 Chechnya               Chelyabinsk           
## [19] Chita                  Chukot                 Chuvash               
## [22] City of St. Petersburg Dagestan               Evenk                 
## [25] Gorno-Altay            Ingush                 Irkutsk               
## [28] Ivanovo                Kabardin-Balkar        Kaliningrad           
## [31] Kalmyk                 Kaluga                 Kamchatka             
## [34] Kirov                  Komi                   Komi-Permyak          
## [37] Koryak                 Kostroma               Krasnodar             
## [40] Krasnoyarsk            Kurgan                 Kursk                 
## [43] Leningrad              Lipetsk                Maga Buryatdan        
## [46] Mariy-El               Mordovia               Moskva                
## [49] Murmansk               Nenets                 Nizhegorod            
## [52] North Ossetia          Novgorod               Novosibirsk           
## [55] Omsk                   Orel                   Orenburg              
## [58] Penza                  Perm'                  Primor'ye             
## [61] Pskov                  Rostov                 Ryazan'               
## [64] Sakha                  Sakhalin               Samara                
## [67] Saratov                Smolensk               Stavropol'            
## [70] Sverdlovsk             Tambov                 Tatarstan             
## [73] Taymyr                 Tomsk                  Tula                  
## [76] Tuva                   Tver'                  Tyumen'               
## [79] Udmurt                 Ul'yanovsk             Ust-Orda Buryat       
## [82] Vladimir               Volgograd              Vologda               
## [85] Voronezh               Yamal-Nenets           Yaroslavl'            
## [88] Yevrey                
## 88 Levels: Adygey Aga Buryat Altay Amur Arkhangel'sk ... Yevrey
```


Есть альтернативные названия регионов `gadm$VARNAME_1` и названия регионов на русском `gadm$NL_NAME_1`, которые я не могу прочитать из-за кодировки.

Можно посмотреть деление регионов по типам:

```r
table(gadm$TYPE_1)
```

```
## 
## Avtonomnaya Oblast   Avtonomnyy Okrug           Gorsovet 
##                  1                 10                  1 
##               Kray             Oblast         Respublika 
##                  7                 48                 21
```

Википедия даёт чуть-чуть [другое деление](http://ru.wikipedia.org/wiki/%D0%A4%D0%B5%D0%B4%D0%B5%D1%80%D0%B0%D1%82%D0%B8%D0%B2%D0%BD%D0%BE%D0%B5_%D1%83%D1%81%D1%82%D1%80%D0%BE%D0%B9%D1%81%D1%82%D0%B2%D0%BE_%D0%A0%D0%BE%D1%81%D1%81%D0%B8%D0%B8#.D0.A0.D0.B0.D1.81.D0.BF.D1.80.D0.B5.D0.B4.D0.B5.D0.BB.D0.B5.D0.BD.D0.B8.D0.B5_.D1.81.D1.83.D0.B1.D1.8A.D0.B5.D0.BA.D1.82.D0.BE.D0.B2_.D1.84.D0.B5.D0.B4.D0.B5.D1.80.D0.B0.D1.86.D0.B8.D0.B8_.D0.BF.D0.BE_.D1.82.D0.B8.D0.BF.D0.B0.D0.BC)

В объекте `gadm` есть данные по длине границы и площади региона:

```r
plot(log(gadm$Shape_Leng), log(gadm$Shape_Area), xlab = "Лог-длина границы", 
    ylab = "Лог-площадь региона", main = "Зависимость площади региона и длины границы")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Этим измерениям стоит доверять лишь очень примерно. Единицы измерения --- пока не определены, есть подозрение, что это квадратные градусы :) 

[Россия --- родина слонов](http://ru.wikipedia.org/wiki/%D0%A0%D0%BE%D1%81%D1%81%D0%B8%D1%8F_%E2%80%94_%D1%80%D0%BE%D0%B4%D0%B8%D0%BD%D0%B0_%D1%81%D0%BB%D0%BE%D0%BD%D0%BE%D0%B2)! Точнее, Россия одна из двух стран, пересекающих 180-ый меридиан. Примерно по 180-му меридиану проходит [линия перемены дат](http://ru.wikipedia.org/wiki/%D0%9C%D0%B5%D0%B6%D0%B4%D1%83%D0%BD%D0%B0%D1%80%D0%BE%D0%B4%D0%BD%D0%B0%D1%8F_%D0%BB%D0%B8%D0%BD%D0%B8%D1%8F_%D0%BF%D0%B5%D1%80%D0%B5%D0%BC%D0%B5%D0%BD%D1%8B_%D0%B4%D0%B0%D1%82). Слева --- "вчера", справа --- "сегодня":


![](http://upload.wikimedia.org/wikipedia/commons/c/c6/Diomede_Islands_Bering_Sea_Jul_2006.jpg)

Если построить карту без корректировок, то она будет разрезана по 180-му меридиану, и кусок Чукотки будет резко слева от всей остальной карты. Чтобы избежать этого, нужно построить другую проекцию:


```r
proj4.str <- CRS("+init=epsg:3413 +lon_0=105")
gadm.prj <- spTransform(gadm, proj4.str)
spplot(gadm.prj, "NAME_1")  # `NAME_1` --- переменная, отвечающая за раскраску
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


## Один регион более детально







