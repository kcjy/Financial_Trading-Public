library(Rblpapi)
library(TTR)
library(zoo)
require(quantmod)
require(ggplot2)
library(tidyr)
library(readxl)
library(sets)
library(gridExtra)
library(grid)

blpConnect()

#Specifying Start End Date for BLP
start = Sys.Date() - 4000
end = Sys.Date()
opt <- c("periodicitySelection"="DAILY")

#Extracting and formatting data from Bloomberg

tickers = data.frame(read.csv("holdings.csv"))
tickers = tickers[!(tickers %in% c("601299 CH Equity","600485 CH Equity"))]
tickers = as.vector(tickers[,1])

index_tickers = c('MXCN Index', 'SHCOMP Index')

prices = bdh(tickers, c("PX_OPEN", "PX_LAST", "VOLUME"), start.date = start, end.date = end,
             options = opt)
names = bdp(tickers, c("NAME", "GICS_SECTOR_NAME"))

indices = bdh(index_tickers, c("PX_OPEN", "PX_LAST", "VOLUME"), start.date = start, end.date = end,
              options = opt)

#Stocks
px_last = as.data.frame(do.call(cbind, sapply(names(prices), function(x)prices[[x]]["PX_LAST"])))
last_open = as.data.frame(do.call(cbind, sapply(names(prices), function(x)prices[[x]]["PX_LAST"] - prices[[x]]["PX_OPEN"])))
volume = as.data.frame(do.call(cbind, sapply(names(prices), function(x)prices[[x]]["VOLUME"])))
index_price = as.data.frame(do.call(cbind, sapply(names(indices), function(x)indices[[x]]["PX_LAST"])))

colnames(px_last) = strsplit(colnames(px_last),".PX_LAST")
colnames(index_price) = strsplit(colnames(index_price),".PX_LAST")

date1 <- as.set(c(do.call(cbind, sapply(names(prices), function(x)prices[[x]]["date"]))))
date2 <- as.set(c(do.call(cbind, sapply(names(indices), function(x)indices[[x]]["date"]))))
date1 <- as.numeric(date1)
date2 <- as.numeric(date2)

#Sectors
sector_names = as.vector(unique(names$GICS_SECTOR_NAME))
sector_prices <- function(x) {
  stocks = row.names(names[which(names$GICS_SECTOR_NAME == x),])
  rowSums(px_last[stocks])
}
sectorprices <- vector()
for (y in sector_names){
  sectorprices <<- cbind(sectorprices, sector_prices(y))
}

colnames(sectorprices) <- sector_names
#sectorprices <- as.data.frame(do.call(cbind, lapply(sector_names, sector_prices)))
sectorprices <- data.frame(sectorprices)

######################################################################################

#10-Day Advancing Volume/ (Advance + Decline Volume)
volume = volume / 10^6
adv_volume <- data.frame(sapply(volume, function(x) diff(x, lag=10))>0)
adv_volume[is.na(adv_volume)] <- 0 
adv_volume <- round(rowSums(sapply(adv_volume, as.numeric))/ncol(adv_volume),3)
 

#% of stock at 30 Day New High
high30day <- data.frame(px_last == sapply(px_last, function(x) runMax(x, 30)))
high30day <- round(rowSums(sapply(high30day, as.numeric))/ncol(high30day),3)

############################################################################

#McClellan Oscillator
adv_px <- data.frame(sapply(px_last, function(x) diff(x, lag=1)) > 0)
adv_px <- round(rowSums(sapply(adv_px, as.numeric))/ncol(adv_px),3)

dec_px <- data.frame(sapply(px_last, function(x) diff(x, lag=1)) < 0)
dec_px <- round(rowSums(sapply(dec_px, as.numeric))/ncol(dec_px),3)

rana <- (adv_px - dec_px)/ (adv_px + dec_px)
MCL_osc <- c(NA, EMA(rana, 19) - EMA(rana, 39))
  
###############################################################################

#Percentage of stocks/sectors above 10D MA
MA_percentage <- function(data, period){
  imd <- data.frame(data > lapply(data, function(x) SMA(x, n = period)))
  return(round(rowSums(sapply(imd, as.numeric))/ncol(imd),3))
}

stock_10 <- MA_percentage(px_last, 10)
stock_50 <- MA_percentage(px_last, 50)
stock_200 <- MA_percentage(px_last, 200)

sector_10 <- MA_percentage(sectorprices, 10)
sector_50 <- MA_percentage(sectorprices, 50)
sector_200 <- MA_percentage(sectorprices, 200)

############################################################################################

#Moving Average Direction
upperiods50 <- data.frame(index_price > lapply(index_price, function(x) SMA(x, n = 50)))
upperiods50 = sum(na.omit(as.numeric(rowSums(sapply(upperiods50, as.numeric))==2)))
downperiods50 <- data.frame(index_price < lapply(index_price, function(x) SMA(x, n = 50)))
downperiods50 = sum(na.omit(as.numeric(rowSums(sapply(downperiods50, as.numeric))==2)))
MAD50 <- c(upperiods50/nrow(index_price), downperiods50/nrow(index_price), 
           (nrow(index_price) - upperiods50 - downperiods50)/nrow(index_price))


upperiods200 <- data.frame(index_price > lapply(index_price, function(x) SMA(x, n = 200)))
upperiods200 = sum(na.omit(as.numeric(rowSums(sapply(upperiods200, as.numeric))==2)))
downperiods200 <- data.frame(index_price < lapply(index_price, function(x) SMA(x, n = 200)))
downperiods200 = sum(na.omit(as.numeric(rowSums(sapply(downperiods200, as.numeric))==2)))
MAD200 <- c(upperiods200/nrow(index_price), downperiods200/nrow(index_price), 
           (nrow(index_price) - upperiods200 - downperiods200)/nrow(index_price))
###########################################################################################

#Sectors with Rising MA
sectorsMA <- data.frame(lapply(sectorprices, function(x) SMA(x, n = 50)))
sectorsMA <- data.frame(sapply(sectorsMA, function(x) c(NA, tail(x,n=-1) / head(x,n=-1))) > 1)
risingsectors50 <- round(rowSums(sapply(sectorsMA, as.numeric))/ncol(sectorsMA),3)

sectorsMA <- data.frame(lapply(sectorprices, function(x) SMA(x, n = 200)))
sectorsMA <- data.frame(sapply(sectorsMA, function(x) c(NA, tail(x,n=-1) / head(x,n=-1))) > 1)
risingsectors200 <- round(rowSums(sapply(sectorsMA, as.numeric))/ncol(sectorsMA),3)

#######################################################################

stock_values <- na.omit(data.frame(cbind(date1, stock_10, stock_50, stock_200, high30day, MCL_osc, adv_volume)))
sector_values <- data.frame(cbind(date1, sector_10, sector_50, sector_200, risingsectors50, risingsectors200))
index_values <- data.frame(cbind(date2, MAD50, MAD200))
colnames(index_values) <- c('date2', 'MXCN.Index.50', 'SHCOMP.Index.50',
                            'MXCN.Index.200', 'SHCOMP.Index.200')

stock_values$date1 <- as.Date(stock_values$date1)
sector_values$date1 <- as.Date(sector_values$date1)
index_values$date2 <- as.Date(index_values$date2)

#############################Plotting Functions############################################

############################STOCKS#######################################################################
ggplot(stock_values) +
  geom_line(aes(x=date1, y=adv_volume, color = 'legend')) + scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") + xlab('Dates') + ylab("Values") + ggtitle("10-Day Advancing Volume / (10-Day Advancing + Declining Volume)")

ggplot(stock_values) +
  geom_line(aes(x=date1, y=stock_10, color = 'legend')) + scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") + xlab('Dates') + ylab("Values") + ggtitle("% of Stocks above 10-day MA")

ggplot(stock_values) +
  geom_line(aes(x=date1, y=stock_50, color = 'legend')) + scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") + xlab('Dates') + ylab("Values") + ggtitle("% of Stocks above 50-day MA")

ggplot(stock_values) +
  geom_line(aes(x=date1, y=stock_200, color = 'legend')) + scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") + xlab('Dates') + ylab("Values") + ggtitle("% of Stocks above 200-day MA")

ggplot(stock_values) +
  geom_line(aes(x=date1, y=high30day, color = 'legend')) + scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") + xlab('Dates') + ylab("Values") + ggtitle("% of Stocks at 30-day New Highs")

ggplot(stock_values) +
  geom_line(aes(x=date1, y=MCL_osc, color = 'legend')) + scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") + xlab('Dates') + ylab("Values") + ggtitle("McClellan Oscillator")


##################################SECTORS################################################################
ggplot(sector_values) +
  geom_line(aes(x=date1, y=sector_10, color = 'legend')) + scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") + xlab('Dates') + ylab("Values") + ggtitle("% of Sectors above 10-day MA")

ggplot(sector_values) +
  geom_line(aes(x=date1, y=sector_50, color = 'legend')) + scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") + xlab('Dates') + ylab("Values") + ggtitle("% of Sectors above 50-day MA")

ggplot(sector_values) +
  geom_line(aes(x=date1, y=sector_200, color = 'legend')) + scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") + xlab('Dates') + ylab("Values") + ggtitle("% of Sectors above 200-day MA")

ggplot(sector_values) +
  geom_line(aes(x=date1, y=risingsectors50, colour='legend')) + scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") + xlab('Dates') + ylab("Values") + ggtitle("% of Sectors with rising 50-day MA")

ggplot(sector_values) +
  geom_line(aes(x=date1, y=risingsectors200, colour='legend')) + scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") + xlab('Dates') + ylab("Values") + ggtitle("% of Sectors with rising 200-day MA")

############################################INDEXES###################################################

MAD_table = data.frame(round(MAD50,3), round(MAD200,3), row.names = c('Both Rising', 
                                                  'Mixed', 'Both Falling'))
colnames(MAD_table) = c('50 Day Moving Average Direction', '200 Day Moving Average Direction')
grid.newpage()
grid.table(MAD_table)
