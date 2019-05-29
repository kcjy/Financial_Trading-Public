"MainRun" <-
function()
{
library(Rbbg)
library(zoo)
	
conn <- blpConnect()

#	blpDisconnect(conn)
}

"CreateTrackingPortfolio" <-
function (index, tradedate, calibration=2, delta=0.05, number=25)
#############################################################################
#		index - the name of the index. e.g. "NIFTY"
#		tradedate - the date in YYYYMMDD format
#		calibration - number of years custom index prices to be generated
#		delta - the annual extra return to be added to the original index
#############################################################################
{
	end.date <- as.Date(tradedate, "%Y%m%d")
	tradedate <- as.POSIXlt(end.date, "Asia/Singapore")
	start.date <- tradedate
	start.date$year <- tradedate$year - calibration
	start.date <- as.Date(start.date)
	
	#Get the last trading day within the date range
	index.px.list <- bdh(conn, paste(index, "Index"), "PX_LAST", format(start.date, "%Y%m%d"), format(end.date, "%Y%m%d"), include.non.trading.days = FALSE)
	end.date <- as.Date(max(index.px.list$date))

	#Get the historical prices of the index members
	index.divisor <- bdh(conn, paste(index, "Index"), "INDX_DIVISOR", end.date, end.date)
	index.member <- bds(conn, paste(index, "Index"), "INDX_MWEIGHT_PX", override_fields="END_DATE_OVERRIDE", format(end.date, "%Y%m%d"))
	index.member$ticker <- paste(index.member[,1], "Equity")
	seclist <- index.member$ticker
	near.future.seclist <- sapply(seclist, function(x) paste(substr(strsplit(x, " ")[[1]][1],1,5), "=1 IS Equity", sep=""), USE.NAMES=FALSE)
	mid.future.seclist <- sapply(seclist, function(x) paste(substr(strsplit(x, " ")[[1]][1],1,5), "=2 IS Equity", sep=""), USE.NAMES=FALSE)
	
	#Get the prices of the index members on the last day
	px.last <- bdh(conn, seclist, "PX_LAST", format(end.date, "%Y%m%d"), format(end.date, "%Y%m%d"), include.non.trading.days = FALSE)
	#Get all the prices of the index members within the date range
	px.last.list <- bdh(conn, seclist, "PX_LAST", format(start.date, "%Y%m%d"), format(end.date, "%Y%m%d"), include.non.trading.days = FALSE)
	#Check whether all the tickers have data in all the trade dates
	ticker.count.list <- aggregate(px.last.list$PX_LAST, by=list(Count=px.last.list$ticker), FUN="length")
	#Get the real start date and end date by scanning through the prices
	if (min(ticker.count.list$x) != max(ticker.count.list$x)) {
		print("WARNING: Some Tickers are missing data!")
		cat("WARNING:", min(ticker.count.list$x), "data points will be used instead of", max(ticker.count.list$x), ".\n")
		min.ticker <- ticker.count.list[which(ticker.count.list$x == min(ticker.count.list$x)), 1]
	} else {
		min.ticker <- ticker.count.list[1,1]
	}
	min.ticker.data <- px.last.list[px.last.list$ticker==min.ticker,]
	start.date <- as.Date(min(min.ticker.data$date))
	end.date <- as.Date(max(min.ticker.data$date))
	px.last.list <- px.last.list[px.last.list$date>=start.date,]
	
	#Calculate the number of shares for each member in index
	index.member <- merge(px.last, index.member, by.x="ticker", by.y="ticker")
	index.member$shares <- index.member$'Actual Weight' * index.member$'Current Price' / index.member$PX_LAST
	
	#Calculate the total market cap for all the members
	market.cap.sum <- aggregate(px.last.list$PX_LAST, by=list(Date=px.last.list$date), FUN=function(x) sum(x*index.member$shares)/index.divisor$INDX_DIVISOR)
	market.cap.sum$x <- market.cap.sum$x * (1 + (calibration * delta)/length(market.cap.sum[,1]))

	#merge the custom index and the prices of selected tickers into zoo object
	custom.index.zoo <- zoo(market.cap.sum$x, market.cap.sum$Date)

	#Select the tickers by performance
	#selected.ticker <- StockSelectionByPerformance(seclist, start.date, end.date, number)
	selected.ticker <- StockSelectionByCointegration(seclist, px.last.list, custom.index.zoo, number)
	
	alldata <- custom.index.zoo
	for (i in 1:number) {
		cur.ticker <- selected.ticker[i]
		cur.px.list <- px.last.list[px.last.list$ticker==cur.ticker,]
		px.zoo <- zoo(cur.px.list$PX_LAST, cur.px.list$date)
		alldata <- cbind(alldata, px.zoo)
	}
	names(alldata) <- c("INDEX", selected.ticker)
	
	#perform johensen test on the log of all the data
	alldata.log <- log(alldata)
	alldata.vecm <- ca.jo(alldata.log, ecdet='const', type='eigen', K=2, spec='longrun')
	eigenvector <- alldata.vecm@V[,1]
	names(eigenvector) <- c(paste(index, "Index"), selected.ticker, "Constant")
	#lambda <- alldata.vecm@lambda[1]
	#ticker.weight <- coredata(-eigenvector[2:(number+1)])
	#ticker.weight <- cbind(selected.ticker, ticker.weight)
	
	#Calculate in sample residue
	alldata.log$Constant <- 1
	residue <- as.matrix(alldata.log) %*% as.matrix(eigenvector)
	plot.zoo(residue)
	
	#Generate out of sample residue
	
	return (list(member=selected.ticker, weight=eigenvector))
}

"PrepareData" <-
function (startdate, enddate, number, calibration, delta, rebalance)
{
	library(zoo)
	
	start.date <- as.Date(startdate, "%Y%m%d")
	end.date <- as.Date(enddate, "%Y%m%d")
	index <- "NIFTY Index"
	
	#Get the actual start date and end date
	index.px.list <- bdh(conn, index, "PX_LAST", format(start.date, "%Y%m%d"), format(end.date, "%Y%m%d"), include.non.trading.days = FALSE)
	end.date <- as.Date(max(index.px.list$date))
	start.date <- as.Date(min(index.px.list$date))
	
	#Download the prices of NIFTY index futures
	nifty.near.px <- bdh(conn, "NZ1 Index", "PX_LAST", format(start.date, "%Y%m%d"), format(end.date, "%Y%m%d"), include.non.trading.days = FALSE)
	nifty.mid.px <- bdh(conn, "NZ2 Index", "PX_LAST", format(start.date, "%Y%m%d"), format(end.date, "%Y%m%d"), include.non.trading.days = FALSE)
	banknifty.near.px <- bdh(conn, "AF1 Index", "PX_LAST", format(start.date, "%Y%m%d"), format(end.date, "%Y%m%d"), include.non.trading.days = FALSE)
	banknifty.mid.px <- bdh(conn, "AF2 Index", "PX_LAST", format(start.date, "%Y%m%d"), format(end.date, "%Y%m%d"), include.non.trading.days = FALSE)
	
	#Download the future prices of index members
	index.member <- bds(conn, index, "INDX_MWEIGHT_PX", override_fields="END_DATE_OVERRIDE", format(end.date, "%Y%m%d"))
	index.member$ticker <- paste(index.member[,1], "Equity")
	seclist <- index.member$ticker
	near.future.seclist <- sapply(seclist, function(x) paste(substr(strsplit(x, " ")[[1]][1],1,5), "=1 IS Equity", sep=""), USE.NAMES=FALSE)
	mid.future.seclist <- sapply(seclist, function(x) paste(substr(strsplit(x, " ")[[1]][1],1,5), "=2 IS Equity", sep=""), USE.NAMES=FALSE)
	near.future.px.list <- bdh(conn, near.future.seclist, "PX_LAST", format(start.date, "%Y%m%d"), format(end.date, "%Y%m%d"), include.non.trading.days = FALSE)
	mid.future.px.list <- bdh(conn, mid.future.seclist, "PX_LAST", format(start.date, "%Y%m%d"), format(end.date, "%Y%m%d"), include.non.trading.days = FALSE)
	
	#Smooth future prices
	nifty.near.zoo <- zoo(nifty.near.px$PX_LAST, nifty.near.px$date)
	nifty.mid.zoo <- zoo(nifty.mid.px$PX_LAST, nifty.mid.px$date)
	nifty.zoo <- merge(nifty.near.zoo, nifty.mid.zoo)
	nifty.smoothed <- SmoothFuturePrices(nifty.zoo)
	
	banknifty.near.zoo <- zoo(banknifty.near.px$PX_LAST, banknifty.near.px$date)
	banknifty.mid.zoo <- zoo(banknifty.mid.px$PX_LAST, banknifty.mid.px$date)
	banknifty.zoo <- merge(banknifty.near.zoo, banknifty.mid.zoo)
	banknifty.smoothed <- SmoothFuturePrices(banknifty.zoo)
	
	px.smoothed.matrix <- NULL
	for (i in 1:length(seclist)) {
		cur.near.ticker <- near.future.seclist[i]
		cur.mid.ticker <- mid.future.seclist[i]
		cur.near.px <- near.future.px.list[near.future.px.list$ticker==cur.near.ticker,]
		cur.mid.px <- mid.future.px.list[mid.future.px.list$ticker==cur.mid.ticker,]
		cur.near.zoo <- zoo(cur.near.px$PX_LAST, cur.near.px$date)
		cur.mid.zoo <- zoo(cur.mid.px$PX_LAST, cur.mid.px$date)
		cur.zoo <- merge(cur.near.zoo, cur.mid.zoo)
		cur.smoothed <- SmoothFuturePrices(cur.zoo)
		
		if (is.null(px.smoothed.matrix)) {
			px.smoothed.matrix <- cur.smoothed
		} else {
			px.smoothed.matrix <- cbind(px.smoothed.matrix, cur.smoothed)
		}
	}
	names(px.smoothed.matrix) <- seclist
	
	
}

"BacktestNifty" <-
function (tval=10, index, stock, number=5, period=750, delta, rollingwindow = 130, entrysigma = 2, stoplosswindow = 5)
{	
	library(zoo)
	library(tseries)
	library(urca)
	library(TTR)
	
	tval <- as.numeric(tval)
	period <- as.numeric(period)
	
	size <- length(stock[1,])
	days <- length(index)
	
	upperLimit <- entrysigma
	minratio <- 0.7 #the minimal level of entrysigma to start a trade
	
	trade.list <- NULL
	pair.record <- NULL
	recentlows <- rep(NA, stoplosswindow)
	sl <- 0
	
	margin <- 0.15
	monitordays <- 0
	maxmondays <- 10
	
	seclist <- names(stock)
	selected.sec <- NULL
	eigenvector <- NULL
	
	row.count <- 1
	entry.spread <- 0
	threshold <- 0.03

	#perform co-integration test using johansen procedure
	for (i in period:days) {
		currentDate <- time(stock[i])
		index.part <- index[(i-period+1):i]
		stock.part <- stock[(i-period+1):i,]
		index.part.log <- log(index.part)
		stock.part.log <- log(stock.part)
		
		if (is.null(pair.record$tradedate)) {
			res <- StockSelectionByCointegration(seclist, stock.part.log, index.part.log, number, rollingwindow)
			#Check whether the combination is cointegrated with 90% confidence level
			tenpct.cval <- last(res$vecm@cval[,1])
			tstat <- last(res$vecm@teststat)
			if (tstat >= tenpct.cval) {
				pass <- TRUE
			} else {
				pass <- FALSE
			}
			pair.row <- data.frame(row.names=row.count, tradedate=currentDate, tenpct=pass, spread=tail(res$residue,1), stdev=tail(res$residue.sd, 1), mvavg=tail(res$residue.sma, 1), sigma=(tail(res$residue,1)-tail(res$residue.sma,1))/tail(res$residue.sd,1), monitor=FALSE, position=FALSE, direction="NA", action="NA")
			pair.record <- pair.row
			row.count <- row.count + 1
		} else {
			curlen <- length(pair.record$tradedate)
							
			if (pair.record$monitor[curlen] || pair.record$position[curlen]) {
				curlen <- curlen + 1
				pair.row <- tail(pair.record,1)  #retrieve the last row
				selected.px <- stock.part.log[, selected.sec]
				sprd <- cbind(merge(index.part.log, selected.px), 1) %*% eigenvector
				spread <- tail(sprd, 1)
				stdev <- 0
				movingavg <- 0
				
				stdev <- sd(sprd[(period-rollingwindow+1):period])
				movingavg <- tail(SMA(sprd, rollingwindow), 1)
				
				currentsigma <- (spread - movingavg)/stdev
				monitor <- pair.row$monitor
				position <- pair.row$position
				
				pair.row$tradedate <- currentDate
				row.names(pair.row) <- as.numeric(row.names(pair.row))+1
				direction <- pair.row$direction
				pair.row$spread <- spread
				pair.row$stdev <- stdev
				pair.row$mvavg <- movingavg
				pair.row$sigma <- currentsigma
								
				if (monitor) {
					#currently monitor the pair
					#check whether the spread comes back to the range
					if ((direction == "DOWN" && currentsigma <= upperLimit ) || (direction == "UP" && currentsigma >= -upperLimit)) {
						pair.row$monitor <- FALSE
						
						if ((direction == "DOWN" && currentsigma >= upperLimit*minratio) || (direction == "UP" && currentsigma <= -upperLimit*minratio)) {
							pair.row$position <- TRUE
							ratio.stock <- 0
							if (direction == "DOWN") {
								ratio.stock <- -eigenvector
							} else if (direction == "UP") {
								ratio.stock <- eigenvector
							}
							price.stock <- tail(stock.part[, selected.sec], 1)
							names(price.stock) <- c(1:number)
							names(ratio.stock) <- c("index", c(1:number), "constant")
							
							trade.row <- data.frame(tradedate=currentDate, action="Entry", spread=spread, price.index=tail(index.part,1), price.stock=price.stock, ratio.stock=t(ratio.stock))
							if (is.null(trade.list$tradedate)) {
								trade.list <- trade.row
							} else {
								trade.list <- rbind(trade.list, trade.row)
							}
							pair.row$action <- "ENTRY"
							pair.row$position <- TRUE
							pair.row$monitor <- FALSE
							entry.spread <- spread
						}
						monitordays <- 0
					} else if (monitordays > maxmondays) {
						pair.row$monitor <- FALSE
						monitordays <- 0
					} else {
						monitordays <- monitordays + 1
					}
				} else if (position) {
					#currently trading the pair
					#checking for exit

					#if the trade hits the expected return, exit
					if (((direction == "DOWN" && (entry.spread - spread) >= threshold) || (direction == "UP" && (spread - entry.spread) >= threshold)) || ((direction == "DOWN" && (spread - entry.spread) >= threshold) || (direction == "UP" && (entry.spread - spread) >= threshold)))
					{
						#loss exceeds stoploss threshold, exit the trade
						pair.row$monitor <- FALSE
						pair.row$position <- FALSE
						pair.row$direction <- "NA"
						price.stock <- tail(stock.part[, selected.sec], 1)
						names(price.stock) <- c(1:number)
						names(ratio.stock) <- c("index", c(1:number), "constant")
					
						trade.row <- data.frame(tradedate=currentDate, action="Exit", spread=spread, price.index=tail(index.part,1), price.stock=price.stock, ratio.stock=t(ratio.stock))
						trade.list <- rbind(trade.list, trade.row)
						
						entry.spread <- 0
						pair.row$action <- "EXIT"
					} else {
						pair.row$action <- "INPOS"
					}
				}
			} else {
				res <- StockSelectionByCointegration(seclist, stock.part.log, index.part.log, number, rollingwindow)
				tenpct.cval <- last(res$vecm@cval[,1])
				tstat <- last(res$vecm@teststat)
				if (tstat >= tenpct.cval) {
					pass <- TRUE
				} else {
					pass <- FALSE
				}
				
				if (pass == TRUE) {
					spread <- tail(res$residue, 1)
					stdev <- tail(res$residue.sd, 1)
					movingavg <- tail(res$residue.sma, 1)
								
					if (spread >= (movingavg+upperLimit*stdev) || spread <= (movingavg-upperLimit*stdev)) {
						#start to monitor the pair
						selected.sec <- res$seclist
						eigenvector <- res$eigenvector
						pair.row <- data.frame(row.names=row.count, tradedate=currentDate, tenpct=pass, spread=spread, stdev=stdev, mvavg=movingavg, sigma=(spread-movingavg)/stdev, monitor=TRUE, position=FALSE, direction="NA", action="NA")
						if (spread >= (movingavg+upperLimit*stdev)) {
							pair.row$direction = "DOWN"
						} else if (spread <= (movingavg-upperLimit*stdev)) {
							pair.row$direction = "UP"
						}
					} else {
						pair.row <- data.frame(row.names=row.count, tradedate=currentDate, tenpct=pass, spread=spread, stdev=stdev, mvavg=movingavg, sigma=(spread-movingavg)/stdev, monitor=FALSE, position=FALSE, direction="NA", action="NA")
					}
				} else {
					pair.row <- data.frame(row.names=row.count, tradedate=currentDate, tenpct=pass, spread=tail(res$residue,1), stdev=tail(res$residue.sd, 1), mvavg=tail(res$residue.sma, 1), sigma=(tail(res$residue,1)-tail(res$residue.sma,1))/tail(res$residue.sd,1), monitor=FALSE, position=FALSE, direction="NA", action="NA")
				}
			}
			
			pair.record <- rbind(pair.record, pair.row)
			row.count <- row.count + 1
		}
	}
	
	#start backtesting
	return (list(pairrecord=pair.record, tradelist=trade.list))

}

"StockSelectionByIndustry" <-
function (seclist, tradedate, number=25)
{
	sub.industry.list <- bdp(conn, seclist, "GICS_SUB_INDUSTRY_NAME")
	market.cap.list <- bdh(conn, seclist, "CUR_MKT_CAP", tradedate, tradedate)
	merged.list <- merge(sub.industry.list, market.cap.list, by.x = 0, by.y = "ticker")
	merged.list <- merged.list[order(merged.list$GICS_SUB_INDUSTRY_NAME),]
}

"StockSelectionByPerformance" <-
function (seclist, startdate, enddate, number=25)
#Pick the best performing N stocks for cointegration test
{
	price.list.start <- bdh(conn, seclist, "PX_LAST", startdate, startdate)
	price.list.end <- bdh(conn, seclist, "PX_LAST", enddate, enddate)
	merged.list <- merge(price.list.start, price.list.end, by.x = "ticker", by.y = "ticker")
	merged.list$RETURN <- (merged.list$PX_LAST.y - merged.list$PX_LAST.x)/merged.list$PX_LAST.x
	merged.list <- merged.list[order(-merged.list$RETURN),]
	ticker.list <- merged.list$ticker[1:number]

	return (ticker.list)
}

"StockSelectionByCointegration" <-
function (seclist, price.list, index, number=5, rollingwindow)
#Pick the tickers by performing cointegration test one by one and choose the most cointegrating ones
{
	library(urca)
	library(pastecs)
	
	testdata <- index
	testheader <- c("Index")
	sourcedata <- price.list
	selected.sec <- NULL
	selected.data <- testdata
	eigenvector <- NULL
	residue <- NULL
	vecm <- NULL
	
	for (i in 1:number) {
		tstat.list <- c(rep(0, length(seclist)))
		names(tstat.list) <- seclist
	
		for (j in 1:length(sourcedata[1,])) {
			cur.sec <- seclist[j]
			#Check whether any data points are missing
			if (NA %in% coredata(sourcedata[,j])) {
				tstat.list[cur.sec] <- 0
			} else {
				tempdata <- cbind(testdata, sourcedata[,j])
				tempdata.vecm <- ca.jo(tempdata, ecdet='const', type='eigen', K=2, spec='longrun')
				tstat.list[cur.sec] <- last(tempdata.vecm@teststat)
			}
		}
		max.sec <- names(tstat.list[tstat.list==max(tstat.list)])
		
		max.sec.data <- sourcedata[,names(sourcedata)==max.sec]
		selected.data <- cbind(selected.data, max.sec.data)
		selected.data.vecm <- ca.jo(selected.data, ecdet='const', type='eigen', K=2, spec='longrun')
		eigenvector <- selected.data.vecm@V[,1]
		residue <- cbind(selected.data, 1) %*% eigenvector
		testdata <- zoo(residue, index(sourcedata))
		selected.sec <- c(selected.sec, max.sec)
		
		testheader <- c(testheader, max.sec)
		#testdata <- cbind(testdata, sourcedata[,names(sourcedata)==max.sec])
		names(selected.data) <- testheader
		names(eigenvector) <- testheader
		sourcedata <- sourcedata[,names(sourcedata)!=max.sec]
		seclist <- names(sourcedata)	
		vecm <- selected.data.vecm
	}

	#selected.sec <- names(testdata)[2:(number+1)]
	curlen <- length(index)
	if (curlen < rollingwindow) {
		residue.sma <- SMA(residue, curlen)
		residue.sd <- rollapply(residue, curlen, sd, align='right')
	} else {
		residue.sma <- SMA(residue, rollingwindow)
		residue.sd <- rollapply(residue, rollingwindow, sd, align='right')
	}

	return (list(seclist=selected.sec, vecm=vecm, eigenvector=eigenvector, residue=residue, residue.sma=residue.sma, residue.sd=residue.sd))
}

"SmoothFuturePrices" <-
function (rawdata) 
{
	datelist <- time(rawdata)
	coredata <- coredata(rawdata)
	size <- length(datelist)
	smoothed.data <- NULL
	new.month.flag <- TRUE
	
	for (i in 1:size) {
		currentdate <- as.Date(datelist[i])
		if (new.month.flag == TRUE) {
			maturity.date <- nextmaturitydate(currentdate)
			new.month.flag <- FALSE
		}
		if ((maturity.date - currentdate) < 4) {
			smoothed.data[i] <- coredata[i,2]
			if ((maturity.date - currentdate) < 1) {
				new.month.flag <- TRUE
			}
		} else {
			smoothed.data[i] <- coredata[i,1]
		}
	}
	
	smoothed.zoo <- zoo(smoothed.data, datelist)
	return (smoothed.zoo)
}

"nextmaturitydate" <-
function (tradedate) 
{
	tradedate <- as.Date(tradedate)
	first.day <- as.Date(paste(format(tradedate, format="%Y-%m"), "01", sep="-"))
	next.month <- seq(first.day, length=2, by="1 month")[2]
	next.month.weekday <- as.numeric(format(next.month, format="%w"))
	last.thursday <- next.month - ((next.month.weekday + 2) %% 7) - 1
	
	#if the tradedate is after the current month's maturity date, use next month's
	if ((tradedate - last.thursday) > 0) {
		next.month <- seq(next.month, length=2, by="1 month")[2]
		next.month.weekday <- as.numeric(format(next.month, format="%w"))
		last.thursday <- next.month - ((next.month.weekday + 2) %% 7) - 1
	}
	
	#it was a public holiday on 2008-10-30, 2012-01-26
	if (last.thursday == as.Date('2008-10-30')) {
		last.thursday <- as.Date('2008-10-29')
	}
	if (last.thursday == as.Date('2011-10-27')) {
		last.thursday <- as.Date('2011-10-25')
	}
	if (tradedate == as.Date('2011-10-26')) {
		last.thursday <- as.Date('2011-11-24')
	}
	if (last.thursday == as.Date('2012-01-26')) {
		last.thursday <- as.Date('2012-01-25')
	}
	
	return (last.thursday)
}

