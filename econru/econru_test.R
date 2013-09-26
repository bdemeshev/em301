source("econru.R")

df <- get_stat_hse("CPI_Y_CHI")

df

qplot(as.Date(T),CPI_Y_CHI,data=df)

attributes(df)

df2 <- get_panoramio()

df2

df <- get_google_elevation_data()

str(df)

