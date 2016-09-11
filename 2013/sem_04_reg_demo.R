library(XML)
library(RCurl)
library(ggplot2)

url <- "http://www.databasesports.com/olympics/sport/sportevent.htm?sp=ATH&enum=120"
tables <- readHTMLTable(url)

str(tables)

df <- tables[[3]]

head(df)
str(df)

# converting variable types
df$Result <- as.numeric(as.character(df$Result))
df$Year <- as.numeric(as.character(df$Year))

# Look close in the case of a Warning
df$Result

df2 <- subset(df,Medal=="GOLD")

qplot(Year,Result,data=df2)

model <- lm(Result~Year,data=df2)
summary(model)

# get the vector beta.hat
betas.hat <- coef(model)

betas.hat
betas.hat[2]

