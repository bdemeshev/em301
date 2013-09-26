require(XML)
require(RCurl)
require(zoo) # for year-quarterly class 
library(rjson)
library(plyr)

get_stat_hse_info_vector <- function(series.name = "IP_EA_Q",
                                     n.vars = 1,
                                     info = c("methodology","source","comment")) {

  
  if (info=="methodology") 
    url <- paste("http://sophist.hse.ru/hse/1/met/",series.name,".html",sep="")
  if (info=="source") 
    url <- paste("http://sophist.hse.ru/hse/1/sor/",series.name,".html",sep="")
  if (info=="comment") 
    url <- paste("http://sophist.hse.ru/hse/1/com/",series.name,".html",sep="")
  
  url.html <- getURL(url,.encoding="UTF-8")
  url.parsed <- htmlTreeParse(url.html)
  url.root <- xmlRoot(url.parsed)

  
  # there maybe 2 situations:
  # one entry for each variable
  # one entry for all variables
  # or more than 2 ;)
  
  n.on.site <- length(xmlChildren(url.root[[3]][[3]])) %/% 2 # only approximate
  text <- rep("",n.vars)
  
  for (i in 1:min(n.on.site,n.vars)) {
    temp.value <- xmlValue(url.root[[3]][[3]][[2*i]])
    if (length(temp.value)>0) text[i] <- temp.value # avoid empty blocks 
  }
    
  return(text)
}  
  

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

  n.vars <- ncol(df)-1 # remove "T", the name of index
  
  attr(df,"methodology") <- c("",
                              get_stat_hse_info_vector(series.name,n.vars,"methodology"))

  attr(df,"source") <- c("",
                              get_stat_hse_info_vector(series.name,n.vars,"source"))
  
  attr(df,"comment") <- c("",
                              get_stat_hse_info_vector(series.name,n.vars,"comment"))
  
  return(df)  
}

get_panoramio <- function(minx=37.606,miny=55.719,maxx=37.612,maxy=55.722,from=0,to=100) {

  url <- paste("http://www.panoramio.com/map/get_panoramas.php?set=public&from=",
    from,"&to=",to,"&minx=",minx,"&miny=",miny,"&maxx=",maxx,"&maxy=",maxy,
               sep="")
  url.json <- getURL(url)
  
  # this function creates an ugly list
  panor.list <- fromJSON(url.json)
  
  # we just need the list of list called panor.list$photos
  # the list of list as member of list... @#$%^!
  
  # convert it to a pretty data frame!
  df <- ldply(panor.list$photos,data.frame)  
  
  return(df)
}


get_google_elevation_data <- function(from.x=36.5,from.y=55.3,to.x=37.5,to.y=56.5,samples=500) {
  
  url <- paste("http://maps.google.com/maps/api/elevation/json?path=",from.x,",",from.y,
               "|",to.x,",",to.y,"&samples=",samples,"&sensor=false",collapse="",sep="")
  url.json <- getURL(url)
  
  elev.list <- fromJSON(url.json)
  
  df <- ldply(elev.list$results,data.frame)
  return(df)
}

