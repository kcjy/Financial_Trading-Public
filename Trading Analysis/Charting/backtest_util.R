library('zoo')
library('PerformanceAnalytics')
library('TTR')


x <- read.csv('results.csv', header = T)


curr2 = colnames(x)

curr1 =c('USDJPY', 'EURUSD', 'GBPUSD', 'AUDUSD',
         'NZDUSD', 'USDCAD', 'USDCHF', 'XAUUSD',
         'DXY', 'ADXY', 'USDCNY', 'USDCNH', 'CCN.1M',
         'KWN.1M', 'NTN.1M', 'IRN.1M', 'IHN.1M', "MRN.1M",
         'PPN.1M', 'USDTHB','USDSGD', 'JPYKRW', 'EURJPY',
         'GBPJPY', 'AUDJPY', 'NZDJPY', 'CADJPY',
         'CHFJPY', 'EURGBP', 'EURAUD', 'EURNZD', "EURCHF",
         'EURCAD', 'GBPAUD', 'GBPNZD', 'GBPCAD', "GBPCHF", "GBPSGD",
         'AUDNZD', 'AUDSGD', 'AUDCAD',"AUDCHF",
         'NZDCAD', "NZDCHF", "CADCHF", 'VIX', 'NIFTY',
         'SHCOMP', 'SPX', 'ES1', 'NKY','KOSPI',
         'TWSE','HSCEI','HSI','SET','TY1','FV1',
         'ED3','XAU','YM1','RX1','CO1','CL1'
)

dates = as.character(x[,1])
dates <- dates[-(1)]
dates <- as.Date(dates)
currencylist <- list()

for (i in 1:length(curr1)){
  j <- curr1[i]
  data <- x[,which(grepl(j, curr2) == TRUE)]
  data <- data[-1,]
  data <- do.call(cbind, lapply(1:ncol(data), function(x) as.numeric(as.matrix(data)[,x]) + 1))
  data <- data.frame(do.call(cbind,lapply(1:ncol(data), function(x)cumprod(data[,x]))) -1)
  data[1,] <- 0
  colnames(data) <- c('Asia',"London","NY")
  data$date <- dates
  data[is.na(data)] <- 0
  currencylist[[j]] <- data
}

daily_sharpe <- function(data){
  px <- data[,c('Asia', 'London', 'NY')]
  result <-lapply(1:ncol(px),function(x) cumsum(px[,x]) / seq_along(px[,x]) / runSD(px[,x], n=1, cumulative=TRUE))
  result <- as.data.frame(do.call(cbind, result))
  colnames(result) <- c('Asia', 'London', 'NY')
  result$date <- data$date
  return(result)
}

sharperatios <- lapply(1:length(currencylist), function(y) daily_sharpe(currencylist[[y]]))
names(sharperatios) <- names(currencylist)
timezones <- c('Asia', 'London', 'NY')

signal <- matrix(,nrow = length(names(currencylist)), ncol = length(timezones))
row.names(signal) <- names(currencylist)
colnames(signal) <- timezones

parse_signals <- function(timezone){
  buy = sapply(names(currencylist), function(x) all(tail(currencylist[[x]][timezone],3)) > 0 & all(tail(sharperatios[[x]][timezone],3)) > 1)
  sell = sapply(names(currencylist), function(x) all(tail(currencylist[[x]][timezone],3)) < 0 & all(tail(sharperatios[[x]][timezone],3)) < -1)
  buy_signals <- names(buy[buy==TRUE])
  sell_signals <- names(sell[sell==TRUE])
  signal[buy_signals,timezone] <<- 'Buy'
  signal[sell_signals,timezone] <<- 'Sell'
}

for (name in timezones){
  parse_signals(timezones)
}



