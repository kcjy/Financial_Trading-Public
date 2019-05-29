library(plotly)
library(quantmod)
require(reshape)
require(plyr)
require(tidyr)
require(dplyr)
library(openxlsx)
library(Rblpapi)
library(lubridate)
library(tidyquant)
library(birk)
blpConnect()

#---------------------- Functions ------------------------------------

## Retrieve daily data

get.live.tickData <-
  function(ticker,
           interval = 5,
           days = 5,
           start = "2017-12-01 12:00",
           end = Sys.time()) {
    data <-
      getBars(
        ticker,
        eventType = "TRADE",
        barInterval = interval,
        startTime =strptime(paste(Sys.Date() - days(days+2), "21:00"), tz="Asia/Singapore", format="%Y-%m-%d %H:%M"),
        tz = "Asia/Singapore",
        returnAs = getOption("blpType", "matrix")
      )
    data = data[, 1:5]
    colnames(data) = c("Date", "Open", "High", "Low", "Close")
    data = as.xts(data[, 2:5], order.by = data$Date)
    
    return(data)
  }

dl_bbg_data <- function(ticker, flds, start, end, per, days = NULL, method = NULL ,sort = FALSE,fill = FALSE){
  start <- as.Date(start)
  end <- as.Date(end)
  opt <- c("periodicitySelection"=per,"nonTradingDayFillOption"=days,"nonTradingDayFillMethod"=method)
  data <- bdh(ticker, flds, start, end, options=opt)
  
  if (sort){
    data <- data[order(data$date, decreasing=TRUE),]
  }
  
  if(fill & nrow(data) == 0) {
    data <- data.frame(date = as.Date(start:end),flds =  rep(1:2, length.out=length(start:end)) )
  }
  return(data)
}

## Generate Clusters using K-means and Demidenko's Best-K algorithm
## tmp is a dataframe with the Close prices
## max_clust is the total number of clusters that you want to start with
## Dates - dates for the cluster generation
## Index of the date that you're generating the cluster for

generate_clust<-function(tmp, max_clust, i){
  
  # First, find best K
  logtss = c()
  for(j in 1:max_clust){
    set.seed(100)
    tmp_cluster = kmeans(tmp$Close, j)
    logtss = c(logtss, log(tmp_cluster$tot.withinss))
  }
  
  tot_ss = c()
  # K from 2 to Kmax - 2
  for(k in 2:(max_clust-2)){
    reg1 = lm(logtss[1:k] ~ seq(1,k))
    ss1 = sum(reg1$residuals^2)
    
    reg2 = lm(logtss[(k+1):max_clust] ~ seq((k+1),max_clust))
    ss2 = sum(reg2$residuals^2)
    
    tot_ss = c(tot_ss, (ss1+ss2))
  }
  
  num_clust = which.min(tot_ss) + 1
  
  tmp_cluster = kmeans(tmp$Close, num_clust) # generate the actual clusters
  center = tmp_cluster$centers
  center = sort(center, decreasing = T)
  
  # Get min and max for each cluster
  max = c()
  min = c()
  tmp_clust = tmp_cluster$cluster
  for(l in 1:num_clust){
    tmp_ind = which(tmp_clust == l)
    max = c(max, max(tmp$Close[tmp_ind]))
    min = c(min, min(tmp$Close[tmp_ind]))
  }
  
  max = sort(max, decreasing = T)
  min = sort(min, decreasing = T)
  
  cluster_df = data.frame(level = center, start_date = rep(listofdates[week(listofdates) == i][1]), 
                          end_date= rep(listofdates[week(listofdates) == i][length(listofdates[week(listofdates) == i])]), 
                          max = max, min = min)
  cluster_df['combined'] = paste0(signif(cluster_df$level,5), " (", cluster_df$min,",",cluster_df$max, ")")
  colnames(cluster_df) = c("level", "start_date", "end_date", "max", "min", "combined")
  
  return(cluster_df)
}

intraday_dens<-function(ticker, days, max_clust=10){
  set.seed(100) # set seed for reproducibility 

  data.df <- get.live.tickData(ticker = ticker,interval = 1,days = days)
  data_15_df <- get.live.tickData(ticker = ticker,interval = 15,days = days)
  close.df <- dl_bbg_data(ticker = ticker,flds=c("PX_OPEN","PX_HIGH","PX_LOW","PX_LAST"),Sys.Date()-days-1,Sys.Date(), 'WEEKLY')
  colnames(close.df) <- c("date","open","high","low","close")
  #close.df <- close.df[close.df$date < Sys.Date(),]
  
  # Add Price Variables
  data.df$minDelta <- data.df$Close - data.df$Open
  data.df$magnitude <- abs(data.df$minDelta)
  data.df$direction <- ifelse(data.df$minDelta > 0, 1, ifelse(data.df$minDelta < 0, -1, 0))
  # data.df$ret = as.vector(Delt(data.df$Open, data.df$Close))
  
  data_15_df$minDelta <- data_15_df$Close - data_15_df$Open
  data_15_df$magnitude <- abs(data_15_df$minDelta)
  data_15_df$direction <- ifelse(data_15_df$minDelta > 0, 1, ifelse(data_15_df$minDelta < 0, -1, 0))
  
  # Find proportion of close up or close down minutes in each day
  # tmp.df = data.frame(matrix(nrow=0,ncol=7))

  #data.df[listofdates[week(listofdates) == min(week(listofdates))]]
  
  
  #corr_date = c()
  listofdates <<- unique(strftime(row.names(as.data.frame(data_15_df)),"%Y-%m-%d"))
  week_list = unique(week(listofdates))
  pos = c()
  even = c()
  neg = c()
  #dates = c(tmp[-c(1)], tmp[length(tmp)] + days(1))
  max_close = c()
  med_level = list()
  
  date_range <- c()

  for (i in week_list){
      #date_range = c(date_range, dates[i])
      tmp <- data_15_df[listofdates[week(listofdates) == i]]
      #corr_date = c(corr_date, rep(dates[i], nrow(tmp))) # Get the correct dates (rollover to next day)
      date_range <- c(date_range, listofdates[week(listofdates) == i][1])
      # Get clusters
      # vol = sd(tmp$ret)
      # num_clust = round(max(1,abs(log(vol*100))))
      # print(num_clust)
      
      ## Find Best K
      cluster_df = generate_clust(tmp, 10, i)
      med_level[[which(week_list == i)]] = cluster_df
      
      # Get df for 15 min intervals
      tmp = data_15_df[listofdates[week(listofdates) == i]]
      pos = c(pos,(nrow(tmp[tmp$minDelta > 0,]) / nrow(tmp) * 100))
      even = c(even,(nrow(tmp[tmp$minDelta == 0,]) / nrow(tmp) * 100))
      neg = c(neg,(nrow(tmp[tmp$minDelta < 0,]) / nrow(tmp) * 100))
      max_close = c(max_close, max(tmp$Close))
      max_curr_day = c()
      label_y_level = max(tmp$Close) * 1.0005
      
      for (j in 1:nrow(med_level[[which(week_list == i)]])){
        label_y_level = label_y_level * 1.0005
        max_curr_day = c(max_curr_day, label_y_level)
      }
      
      max_curr_day = sort(max_curr_day, decreasing=TRUE)
      med_level[[which(week_list == i)]] = cbind(med_level[[which(week_list == i)]], max_curr_day)
    }
  
  #corr_date = as.Date(corr_date)
  #date_range = as.Date(date_range)
  
  # Dataframe for the proportions
  prop_df <- data.frame(date = date_range, pos_prop = round(pos), 
                       neg_prop = round(neg), even_prop = round(even), max_day = max_close)
  prop_df$max_day = prop_df$max_day * 1.0005
  #prop_df$max_day = prop_df$max_day + (max(prop_df$max_day) - min(prop_df$max_day)) / days
  prop_df['combined'] = paste(prop_df$pos_prop, "/", prop_df$even_prop, "/", prop_df$neg_prop)
  prop_df$date <- as.Date(prop_df$date)
  # Re-allocate dates for the dataframe
  
  #if (wday(dates[1]) != 1){
  #  tmp = as.data.frame(data.df[paste0(dates[1],"T05:00/", dates[length(dates)], "T04:59")])
  #  tmp$date = corr_date
  #  data.df = tmp
  #} else {
  #  tmp = as.data.frame(data.df[paste0(dates[2],"T05:00/", dates[length(dates)], "T04:59")])
  #  tmp$date = corr_date
  #  data.df = tmp
  #}
  tmp = as.data.frame(data_15_df[paste0(listofdates[1],"T05:00/", listofdates[length(listofdates)], "T04:59")])
  
  # Generate the key levels
  data_15_df <- as.data.frame(tmp)
  data_15_df$date <- as.Date(row.names(data_15_df))
  data_15_df$date <- sapply(data_15_df$date, function(x) close.df$date[which.closest(close.df$date, x)])
  data_15_df$date <- as.Date(data_15_df$date)
  cluster_df = rbind(med_level[[1]])
  
  for (i in 2:(length(med_level))){
    cluster_df = rbind(cluster_df, med_level[[i]])
  }
  
  cluster_df$level = signif(cluster_df$level)
  cluster_df$start_date <- as.Date(cluster_df$start_date)
  # Generate Plots
  p <- ggplot(close.df,aes(date,close)) 
  p <- p + geom_candlestick(aes(open = open,high=high,low=low,close=close),alpha = 0.2, fill_up = "darkgreen", fill_down = "red") +   
    geom_jitter(data = data_15_df, alpha = 0.4,aes(x = date,y = Close, size = data_15_df$magnitude, colour=as.factor(direction)),stroke = 0) +
    labs(title = paste0(ticker,": ", "\nPositioning: Buy / Idle / Sell (%)", "\nCentroids: Center (Min, Max)"), x = "", y = "Closing Price") + theme_bw() + 
    scale_color_manual(values=c("red", "#E69F00", "darkgreen", "blue", "darkblue")) + guides(colour = FALSE)
  
  # add label
  p <- p + geom_text(aes(x=date, y=max_day, label=combined), data=prop_df, color= "red", fontface="bold")
  
  # add levels
  p <- p + geom_segment(aes(x=as.Date(start_date), y=level, xend=as.Date(end_date), yend=level), size=1,data=cluster_df)
  
  # add level labels
  p <- p + geom_text(aes(x=start_date, y=max_curr_day, label=combined), data=cluster_df)
  
  # scale y-axis
  #start_close = range(data.df$Close)[1]
  #end_close = range(data.df$Close)[2]
  
  #p <- p + scale_y_continuous(labels= round(seq(start_close, end_close, length.out=10),4), 
  #                            breaks = round(seq(start_close, end_close, length.out=10),4))
  
  return(p)
}

#------------------- Execution -----------------------------

ticker="USDJPY BGN Curncy"
days= 100

intraday_dens(ticker,days)
