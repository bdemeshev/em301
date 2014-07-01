require(lubridate)

df <- read.csv("~/em301/buza/list_2014.csv")
df$lastname <- as.character(df$lastname)
df$firstname <- as.character(df$firstname)
str(df)

places <- c("Неполный бред","Динамлю всех","Овсянка с R",
            "Блондинка и динозавр","Первопечатник","Разноразбросие",
            "Матрица","Вспомнить всё")
df$written
df0 <- subset(df,!is.na(written))

cycle <- function(x,n) {
  if (n>0) {
    top <- head(x,-n)
    bottom <- tail(x,n)
    if (is.vector(x)) ans <- c(bottom,top)
    if (is.factor(x)) ans <- c(bottom,top)
    if (is.data.frame(x)) ans <- rbind(bottom,top)
  }
  if (n==0) ans <- x
  
  return(ans)
}

start <- ymd_hms("2014-06-19 12:10:00")


all <- NULL

for (i in 1:8) {
  add <- df0
  add$place <- places[i]
  add <- cycle(add,2*i-2)
  add$time <- start+minutes(10*(0:21))
  
  all <- rbind(all,add)  
}

all$hour <- hour(all$time)
all$minute <- minute(all$time)
all

write.csv(all,file="~/em301/buza/list_2014_time.csv")

