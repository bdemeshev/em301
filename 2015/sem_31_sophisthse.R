devtools::install_github("bdemeshev/sophisthse") 

library("sophisthse") 
library("forecast") 

x <- sophisthse("POPNUM_Y") 
ggtsdisplay(x) 
y <- sophisthse("UNEMPL_M") 
ggtsdisplay(y) 

z <- sophisthse("AGR_M_I") 
head(z) 

agr <- z[, 1] 
ggtsdisplay(agr) 

wn <- rnorm(200) 
ggtsdisplay(wn) 

rw <- cumsum(wn) 
ggtsdisplay(rw) 
print(agr) 


dy <- diff(y) 
ggtsdisplay(dy) 


model <- Arima(y, order = c(1, 1, 0), 
               seasonal = c(1, 0, 0)) 
future <- forecast(model, h = 24) 
plot(future) 


model_2 <- auto.arima(y) 
summary(model_2) 


model 


new <- ts(rnorm(200), frequency = 4, start = c(1900, 3)) 
ggtsdisplay(new)
