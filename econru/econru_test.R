source("econru.R")

df <- get_stat_hse("IP_EA_M")

df

attributes(df)

df2 <- get_panoramio()

df2

df <- get_google_elevation_data()

str(df)

