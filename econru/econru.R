require(XML)
require(RCurl)
require(zoo) # for year-quarterly class 
library(rjson)
library(plyr)


get_stat_hse_info <- function(series.name = "IP_EA_Q",
                              series.name.2 = "IP_EA_Q_SA",
                              info = c("methodology","source","comment")) {
    
  if (info=="methodology") 
    url <- paste("http://sophist.hse.ru/hse/1/met/",series.name,".html#",series.name.2,sep="")
  if (info=="source") 
    url <- paste("http://sophist.hse.ru/hse/1/sor/",series.name,".html#",series.name.2,sep="")
  if (info=="comment") 
    url <- paste("http://sophist.hse.ru/hse/1/com/",series.name,".html#",series.name.2,sep="")

  url.html <- getURL(url,.encoding="UTF-8")
  url.parsed <- htmlTreeParse(url.html)
  url.root <- xmlRoot(url.parsed)
  
  text <- xmlValue(url.root[[3]][[3]][[2]][[1]][[1]][[1]])
  return(text)
}

get_stat_hse_info_vector <- Vectorize(get_stat_hse_info,
                                      vectorize.args="series.name.2")
  



get_stat_hse <- function(series.name = "IP_EA_Q") {

  # download main data
  url <- paste("http://sophist.hse.ru/exes/tables/",series.name,".htm",sep="")
  url.html <- getURL(url,.encoding="UTF-8")

  # get main table
  tables <- readHTMLTable(url.html)
  df <- tables[[1]]  
  
  # html parse
  url.parsed <- htmlTreeParse(url.html)
  url.root <- xmlRoot(url.parsed)
  
  
  # all to character
  for (i in 1:ncol(df)) df[,i] <- as.character(df[,i])
  
  # save units of measure 
  attr(df,"units") <- gsub("&nbsp","",df[1,])
  
  # get full variable names
  full.names <- rep("",ncol(df))
  for (i in 2:ncol(df)) full.names[i] <- xmlValue(url.root[[3]][[1]][[1]][[i-1]])
  attr(df,"full.names") <- full.names
  
  # remove unused lines (units, info about series)
  df <- df[2:(nrow(df)-4),]
  
  # remove spaces, replace "," by ".", convert to numeric
  for (i in 2:ncol(df)) {
    df[,i] <- gsub(",",".",df[,i])
    df[,i] <- gsub(" ","",df[,i])
    df[,i] <- as.numeric(df[,i])
  }
  
  # pretty time index
  # ... todo
  # http://stackoverflow.com/questions/8514662/how-can-i-read-a-date-series-of-quarterly-data-into-r
  
  # get methodology, comment and source

  var.names <- names(df)[-1] # remove "T", the name of index
  
  attr(df,"methodology") <- c("",
                              get_stat_hse_info_vector(series.name,var.names,"methodology"))

  attr(df,"source") <- c("",
                              get_stat_hse_info_vector(series.name,var.names,"source"))
  
  attr(df,"comment") <- c("",
                              get_stat_hse_info_vector(series.name,var.names,"comment"))
  
  return(df)  
}

get_panoramio <- function(minx=37.606,miny=55.719,maxx=37.612,maxy=55.722,from=0,to=100) {

  url <- paste("http://www.panoramio.com/map/get_panoramas.php?set=public&from=",
    from,"&to=",to,"&minx=",minx,"&miny=",miny,"&maxx=",maxx,"&maxy=",maxy,
               sep="")
  url.html <- getURL(url)
  
  # this function creates an ugly list
  panor.list <- fromJSON(url.html)
  
  # we just need the list of list called panor.list$photos
  # the list of list as member of list... @#$%^!
  
  # convert it to a pretty data frame!
  df <- ldply(panor.list$photos,data.frame)  
  
  return(df)
}
