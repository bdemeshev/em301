df <- read.table("flats_moscow.txt", dec=".", sep="\t", header=TRUE)
str(df) # structure of loaded data frame

model <- lm(data=df, price~livesp+kitsp)
summary(model)

library("dplyr")
library("memisc")
library("ggplot2")

df_b <- df %>% filter(brick == 1)
df_nb <- df %>% filter(brick == 0)

model_b <- lm(data=df_b, price~livesp+kitsp)
model_nb <- lm(data=df_nb, price~livesp+kitsp)

mtable(model, model_b, model_nb)
n <- nrow(df)
k_ur <- 6 
r <- 3
rss_r <- deviance(model)
rss_ur <- deviance(model_b) + 
  deviance(model_nb)
F <- (rss_r - rss_ur)/rss_ur/r*(n-k_ur)
F
qf(0.95, df1=r, df2=n-k_ur)
