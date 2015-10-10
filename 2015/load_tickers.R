
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

all_t2 <- str_split(all_t, fixed(".") )

all_t3 <- str_replace_all(all_t2[[1]], "\n", "")
tickers <- head(all_t3, -1)

getSymbols(tickers, from="2013-05-01", to="2015-10-01")
ClosePrices <- do.call(merge, lapply(tickers, function(x) Cl(get(x))))
df <- as.data.frame(ClosePrices)
df$time <- as.character(time(ClosePrices))
str(df)

write.table(df, file = "ticker30.csv", sep = ";", row.names = FALSE)
