library(Rblpapi)
library(TTR)
library(zoo)
require(quantmod)
require(ggplot2)
library(tidyr)


blpConnect()


#Specifying Start End Date for BLP
start = Sys.Date() - 5
end = Sys.Date()

#Extracting and formatting data from Bloomberg
snp500 = bdh("SPX Index", c('HIGH', 'LOW','PX_LAST'), start = start, end = end)
row.names(snp500) <- as.Date(row.names(snp500), "%d/%m/%Y")
#row.names(snp500) <- as.Date(row.names(snp500))
snp500$date <- NULL

#Efficiency Ratio
eff_ratio <- function(data, period){
  difference <- diff(data$PX_LAST) #Daily Changes
  abs_change <- data$PX_LAST[period:length(data$PX_LAST)] - data$PX_LAST[1:(length(data$PX_LAST)-(period-1))] #Array of absolute changes 
  efficiency_ratio <<- abs(abs_change)/rollapply(abs(difference), width=(period-1), sum) #Calculating Eff Ratio
  MA <<- data.frame(efficiency_ratio, row.names = row.names(snp500)[period:dim(snp500)[1]]) #Storing output in MA dataframe (result dataframe)
  eff_plot <<- data.frame(efficiency_ratio, row.names = row.names(snp500)[period:dim(snp500)[1]]) #eff_plot object for plotting 
  eff_plot$dates <<- as.Date(row.names(snp500)[period:dim(snp500)[1]])
}

eff_ratio(snp500, period=63)

#200 Day SMA of Efficiency Ratio
eff_plot$SMA200 <- SMA(eff_plot[,1],200)

#ADX Calculation (42 day)
snp500_2 <- as.xts(snp500)
row.names(snp500_2) <- row.names(snp500)
adx <- ADX(snp500_2, n=21, maType='EMA')$ADX
MA <- merge(MA, data.frame(adx), by = 0, all.x=TRUE) #Storing output in MA (result dataframe)
row.names(MA) <- MA$Row.names
MA$Row.names <- NULL


#Bandpass Filter (Formula according to Ehler's approach in pdf)
bandpass_filter <- function(HLavg, period = 42, delta = 0.7){
  beta <- cos(360/period)
  gamma <- 1 / cos(720*delta / period)
  alpha <- gamma - sqrt(gamma^2-1)
  
  BP = c(0,0)
  #trend = c(0,0)
  
  for (i in 3:length(HLavg)){
    BP[i] <- 0.5*(1-alpha)*(HLavg[i] - HLavg[i-2]) + beta*(1+alpha)*BP[i-1] - alpha*BP[i-2]
  } 
  
  org_trend <- EMA(BP, n = period)
  #trend <<- na.omit(abs(SMA(trend, n = 200)))
  trend <- data.frame(org_trend, row.names = row.names(snp500))
  trend <- abs(EMA(EMA(trend, n = 63),30)) #EMA is used as results are better than SMA from trial and error
  MA <<- merge(MA, trend, by=0, all.x=TRUE)
  row.names(MA) <<- MA$Row.names
  MA$Row.names <<- NULL
}

bandpass_filter(as.vector((snp500[,1] + snp500[,2])/2))

#Average R-Squared
correlation <- data.frame(snp500$PX_LAST)
correlation$Time <- as.integer(row.names(correlation))
row.names(correlation) <- row.names(snp500)

#Average of 21-day, 63-day, 252-day R-squared
correlation$RSQ1 <- runCor(correlation[,1], correlation[,2], n=21)^2
correlation$RSQ2 <- runCor(correlation[,1], correlation[,2], n=63)^2
correlation$RSQ3 <- runCor(correlation[,1], correlation[,2], n=252)^2
MA$Avg_RSQ <- rowMeans(correlation[63:dim(correlation)[1],3:5])

#Calculating Z-Score of all values (As suggested by Zac)
dates <- as.Date(row.names(na.omit(MA)))
MA <- sapply(na.omit(MA), as.numeric)


#MA <- tail(MA,250)

for (i in 1:ncol(MA)) {
  MA[,i] <- (MA[,i] - mean(MA[,i]))/(sd(MA[,i]))
  MA[,i] <- round(percent_rank(MA[,i]),1)
  MA[,i][MA[,i] <= 0.4] = 0.4
  MA[,i][MA[,i] >= 0.6] = 0.6
  MA[,i] = round((MA[,i] - 0.4)/ 0.2 * 0.25,2)
}

#SOT Composite Calculations
#SOT_composite <- rowSums(MA * 0.25) 
SOT_composite <- rowSums(MA)
#SOT_composite <- 1 / ( 1 + exp(-1 * SOT_composite)) #Logistic function to make values between 0 and 1
#SOT_composite <- (SOT_composite - min(SOT_composite)) / (max(SOT_composite) - min(SOT_composite))
SOT_composite <- SMA(round(SOT_composite,2),n=10) #10 day SMA as specified

#Cutting off 90th percentile
#SOT_composite <- replace(SOT_composite, SOT_composite > quantile(SOT_composite, 0.9, 
#                                                na.rm = TRUE), quantile(SOT_composite, 0.9, na.rm = TRUE))
#Normalizing all values again
#SOT_composite <- (SOT_composite - min(na.omit(SOT_composite))) / (max(na.omit(SOT_composite)) - min(na.omit(SOT_composite)))


#Organizing result dataframe and all calculated outputs
plotdf <- data.frame(MA)
#504-day SMA as specified
plotdf$sma_504 <- abs(EMA(plotdf$EMA, 504))
plotdf <- tail(plotdf, 250)
plotdf$dates <- tail(dates,250)
plotdf$RSQ1 <- tail(correlation[as.Date(row.names(correlation)) %in% dates,3],250)
plotdf$RSQ2 <- tail(correlation[as.Date(row.names(correlation)) %in% dates,4],250)
plotdf$RSQ3 <- tail(correlation[as.Date(row.names(correlation)) %in% dates,5],250)
plotdf$SOT <- SOT_composite


#Plotting past 250 days as per NDF graphs
#SOT <- tail(plotdf, 250)
SOT <- plotdf
#SOT$SOT[SOT$SOT < 0.5] <- abs(0.5 - SOT$SOT[SOT$SOT < 0.5] + 0.5) #Normalizing all values from 0.5 (Trial and Error)
#SOT$SOT <- (SOT$SOT - 0.5) / 0.5 #Normalizing all values again to adjust between 0 and 1
SOT$SOT_line <- 0.5 #SOT line

#Plotting Functions
SOT %>%
  gather(key,value, SOT, SOT_line) %>%
  ggplot(aes(x=dates, y=value, colour=key)) +
  geom_line() + scale_x_date(date_breaks = "1 month", date_labels = "%B-%Y") + xlab('Dates') + ylab("Values") + ggtitle("Strength of Trend Composite")

SOT %>%
  gather(key,value, EMA, sma_504) %>%
  ggplot(aes(x=dates, y=value, colour=key)) +
  geom_line() + scale_x_date(date_breaks = "1 month", date_labels = "%B-%Y") + xlab('Dates') + ylab("Values") + ggtitle("Bandpass Filter")

avgrsq <- plotdf[,4:5]
avgrsq$line <- 0.6

avgrsq <- tail(avgrsq, 250)
avgrsq %>%
  gather(key,value, Avg_RSQ, line) %>%
  ggplot(aes(x=dates, y=value, colour=key)) +
  geom_line() + scale_x_date(date_breaks = "1 month", date_labels = "%B-%Y") + xlab('Dates') + ylab("Values") + ggtitle("Avg R-Squared")

adx_plot <- data.frame(tail(adx,250))
adx_plot$dates <- avgrsq$dates
adx_plot %>%
  gather(key,value, ADX) %>%
  ggplot(aes(x=dates, y=value, colour=key)) +
  geom_line() + scale_x_date(date_breaks = "1 month", date_labels = "%B-%Y") + xlab('Dates') + ylab("Values") + ggtitle("ADX")


eff_plot <- tail(eff_plot, 250)
eff_plot %>%
  gather(key,value, efficiency_ratio, SMA200) %>%
  ggplot(aes(x=dates, y=value, colour=key)) +
  geom_line() + scale_x_date(date_breaks = "1 month", date_labels = "%B-%Y") + xlab('Dates') + ylab("Values") + ggtitle("Efficiency Ratio")

eff_plot$diff <- eff_plot$efficiency_ratio / eff_plot$SMA200 - 1
eff_plot$zero <- 0
eff_plot$top <- 0.25
eff_plot$bottom <- -0.5
eff_plot %>%
  gather(key,value, diff, zero, top, bottom) %>%
  ggplot(aes(x=dates, y=value, colour=key)) +
  geom_line() + scale_x_date(date_breaks = "1 month", date_labels = "%B-%Y") + xlab('Dates') + ylab("Values") + ggtitle("Percentage Efficiency Ratio is above SMA200")

rsqs <- tail(plotdf[,5:8],250)
rsqs %>%
  gather(key,value, RSQ1, RSQ2, RSQ3) %>%
  ggplot(aes(x=dates, y=value, colour=key)) +
  geom_line() + scale_x_date(date_breaks = "1 month", date_labels = "%B-%Y") + xlab('Dates') + ylab("Values") + ggtitle("R-Squared")
