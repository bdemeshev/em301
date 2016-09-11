
all_t <- "MCD.
MMM.
AAPL.
AXP.
BA.
CAT.
CSCO.
CVX.
DD.
DIS.
GE.
GS.
IBM.
JPM.
KO.
MRK.
MSFT.
NKE.
PG.
UNH.
UTX.
VZ.
XOM.
HD.
INTC.
JNJ.
PFE.
TRV.
V.
WMT."

library("stringr")
library("readr")
library("quantmod")
library("dplyr")
library("lubridate")

all_t2 <- str_split(all_t, fixed(".") )

all_t3 <- str_replace_all(all_t2[[1]], "\n", "")
tickers <- head(all_t3, -1)
tickers <- c(tickers, "^GSPC")

getSymbols(tickers, from="2013-01-01", to="2015-10-01")

# ACHTUNG: getSymbols: ^GSPC, Cl(GSPC) !
tickers[length(tickers)] <- "GSPC"

ClosePrices <- do.call(merge, lapply(tickers, function(x) Cl(get(x))))
df <- as.data.frame(ClosePrices)
df$time <- as.character(time(ClosePrices))
str(df)

write.table(df, file = "ticker30.csv", dec=",",
            sep = ";", row.names = FALSE)



dates_line <- "22/04/15
23/04/15
28/01/15
16/04/15
22/04/15
23/04/15
13/11/14
01/05/15
21/04/15
04/02/15
17/04/15
16/04/15
20/04/15
14/04/15
22/04/15
28/04/15
24/10/14
26/09/14
24/10/14
16/04/15
21/04/15
21/04/15
30/04/15
19/05/15
15/04/15
14/04/15
28/04/15
21/04/15
30/01/15
15/05/14"

dates2 <- dates_line %>% str_split("\n")
tickers_date <- as.character(dmy(dates2[[1]]))



tickers


head(df)

colnames(df)[1:length(tickers)] <- tickers

df_alligned <- data_frame(date=(-291:20))
df_alligned %>% tail()

for (i in 1:(length(tickers)-1)) {
  ticker <- tickers[i]
  df_temp <- df[ticker]
  df_temp$time <- df$time
  df_temp$sp500 <- df$GSPC
  colnames(df_temp)[colnames(df_temp)=="sp500"] <- paste0("sp500_",ticker)
  
  df_temp$id <- 1:nrow(df_temp)
  one_obs <- df_temp %>% filter(time == tickers_date[i])
  delta <- one_obs$id
  df_temp <- mutate(df_temp, date = id - delta)
  print(head(df_temp))
  df_alligned <- left_join(df_alligned, df_temp %>% select(-id, -time), by="date")
}
df_alligned %>% head()

write.table(df_alligned, file = "df_alligned.csv", dec=",",
            sep = ";", row.names = FALSE)


df_logret <- mutate_each(df_alligned, funs(c(NA, diff(log(.))) ), -date)
df_logret

results <- data_frame(ticker = head(tickers,-1), alpha=NA, beta=NA, 
                      alpha_se=NA, beta_se=NA, R2=NA)

all_eps <- NULL

# i-th row will be the diagonal of Vi
Vi_diag <- array(NA, dim = c(length(tickers)-1, 41))

for (i in 1:(length(tickers)-1)) {
  ticker <- tickers[i]
  y <- df_logret[[ticker]]
  x <- df_logret[[paste0("sp500_",ticker)]]
  
  temp_data <- data_frame(x = x[2:271], y = y[2:271])
  
  model <- lm(data=temp_data, y~x)
  res <- summary(model)
  table <- res$coefficients
  results$alpha[i] <- table[1,1]
  results$beta[i] <- table[2,1]
  results$alpha_se[i] <- table[1,2]
  results$beta_se[i] <- table[2,2]
  results$R2[i] <- res$r.squared
  
  X <- cbind(rep(1, 41), x[272:length(x)])
  Vi_diag[i,] <- res$sigma^2 * (1 + diag(X %*% solve(crossprod(X)) %*% t(X)) ) 
  
  y_hat <- predict(model, newdata = data_frame(x=x[272:length(x)]) )

  eps <- y[272:length(y)] - y_hat
  temp_eps <- data_frame(eps=eps, model="market", type="AR", ticker=ticker, event_day=(-20:20))
  all_eps <- bind_rows(all_eps, temp_eps)
  
  eps_cum <- cumsum(eps)
  temp_eps <- data_frame(eps=eps_cum, model="market", type="CAR", ticker=ticker, event_day=(-20:20))
  all_eps <- bind_rows(all_eps, temp_eps)
  
  model0 <- lm(data=temp_data, y~1)
  y_hat <- predict(model0, newdata = data_frame(x=x[272:length(x)]) )
  
  eps <- y[272:length(y)] - y_hat
  temp_eps <- data_frame(eps=eps, model="constant", type="AR", ticker=ticker, event_day=(-20:20))
  all_eps <- bind_rows(all_eps, temp_eps)
  
  eps_cum <- cumsum(eps)
  temp_eps <- data_frame(eps=eps_cum, model="constant", type="CAR", ticker=ticker, event_day=(-20:20))
  all_eps <- bind_rows(all_eps, temp_eps)
}

write.table(results, file="ticker_regressions.csv", dec=",",
            sep=";", row.names = FALSE)
results

news_type <- c(rep("bad", 2), rep("good", 21), rep("no", 7))
ticker_ntype <- data_frame(ticker=head(tickers,-1), ntype = news_type)

all_eps <- left_join(all_eps, ticker_ntype, by="ticker")
all_eps

mean_ret <- all_eps %>% group_by(model, type, event_day, ntype) %>% summarise(mean_ret=mean(eps))
mean_ret

library("reshape2")
mean_ret_table <- dcast(mean_ret, event_day~model+ntype+type, value.var = "mean_ret")
mean_ret_table

write.table(mean_ret_table, file="clm_table.csv", dec=",",
            sep=";", row.names = FALSE)

ticker_ntype$i <- 1:nrow(ticker_ntype)
ticker_ntype


ntypes <- c("good", "bad", "no")
periods <- c("event day", "two days", "all")
models <- c("market", "constant")

all_J <- expand.grid(ntype=ntypes, period=periods, model=models)
all_J$J1 <- NA
all_J$J1_BMP <- NA



for (i in 1:nrow(all_J)) {
  if (all_J$period[i] == "all") event_days <- -20:20
  if (all_J$period[i] == "event day") event_days <- 0
  if (all_J$period[i] == "two days") event_days <- 0:1
  
  current_ntype <- all_J$ntype[i]
  current_model <- all_J$model[i]
  current_tickers_no <- (ticker_ntype %>% 
    filter(ntype == current_ntype) ) [["i"]]
  current_tickers <- (ticker_ntype %>% 
    filter(ntype == current_ntype) ) [["ticker"]]
  N <- length(current_tickers)
  all_eps_selected <- filter(all_eps, model==current_model,
              type == "AR", event_day %in% event_days, 
              ticker %in% current_tickers) %>% select(-type, -model)
  all_CAR <- all_eps_selected %>% group_by(ticker) %>% 
    summarise(CAR = sum(eps))
  CAR <- all_CAR[["CAR"]]
  CAR_bar <- mean(CAR)
  Vi_selected <- Vi_diag[current_tickers_no, event_days+21]
  
  # the best name of a variable in this file :)
  var_CAR_bar <- sum(Vi_selected) / N^2
  
  # Boehmer, Musumeci, and Poulsen version
  var_CAR_bar_BMP <- sum((CAR - CAR_bar)^2) / N^2
  
  
  all_J$J1[i] <- CAR_bar / sqrt(var_CAR_bar)
  all_J$J1_BMP[i] <- CAR_bar / sqrt(var_CAR_bar_BMP)
}


write.table(all_J, file="all_J.csv", dec=",",
            sep=";", row.names = FALSE)
all_J
