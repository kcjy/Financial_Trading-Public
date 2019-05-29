library(Rbbg)
library(zoo)
	
conn <- blpConnect()

#	blpDisconnect(conn)

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

"BackTestIndexArb" <-
function (startdate, enddate, index, number, calibration, delta, rebalance)
{
	rebalance.dates <- seq(as.Date(startdate, "%Y%m%d"), to=as.Date(enddate, "%Y%m%d"), by=rebalance)
	
	for (j in 1:(length(rebalance.dates)-1)) {
		currentdate <- rebalance.dates[j]
		rebalancedate <- rebalance.dates[j+1]
		result <- CreateTrackingPortfolio(index, currentdate, calibration, delta, number)
		seclist <- c(paste(index, "Index"), result$member)
		eigenvector <- result$weight
		
		px.last.list <- bdh(conn, seclist, "PX_LAST", currentdate, rebalancedate, include.non.trading.days = FALSE)
		alldata <- NULL
		for (i in 1:(number+1)) {
			cur.ticker <- seclist[i]
			cur.px.list <- px.last.list[px.last.list$ticker==cur.ticker,]
			px.zoo <- zoo(cur.px.list$PX_LAST, cur.px.list$date)
			if (is.null(alldata)) {
				alldata <- px.zoo
			} else {
				alldata <- cbind(alldata, px.zoo)
			}
		}
		names(alldata) <- seclist
		alldata.log <- log(alldata)
		alldata.log$Constant <- 1
		residue <- as.matrix(alldata.log) %*% as.matrix(eigenvector)
		plot.zoo(residue)
	}

}

"DownloadIndexReport" <-
function (tradedate, index)
{
	library(RCurl)
	library(bitops)
	
	#download the data from NSE website
	current.date <- as.Date(tradedate, "%Y%m%d")
	link <- paste("http://www.nseindia.com/archives/ix/Ix", format(current.date, "%d%m%y"), ".zip", sep="")
	zipdir <- tempfile()
	dir.create(zipdir)
	bin <- getBinaryURL(link, httpheader=c(Accept = "*.*"), verbose=TRUE, useragent=getOption("HTTPUserAgent"))
	zipfile	<- paste(zipdir, "Ix.zip", sep="\\") 
	con <- file(zipfile, open="wb")
	writeBin(bin, con)
	close(con)
	
	#unzip the file
	unzip(zipfile, exdir=zipdir)
	unlink(zipfile)
	files <- list.files(zipdir)
	if (length(files) > 1) {
		stop ("More than one data files inside zip")
	}
	file <- paste(zipdir, files[1], sep="\\")
	index.data <- read.csv(file, header=TRUE)
	unlink(zipdir, recursive=TRUE, force = TRUE)
	return(index.data[index.data$INDEX_FLG==index,])
	
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
function (seclist, price.list, custom.index, number=5)
#Pick the tickers by performing cointegration test one by one and choose the most cointegrating ones
{
	library(urca)
	library(pastecs)
	
	price.list.matrix <- NULL
	for (i in 1:length(seclist)) {
		cur.ticker <- seclist[i]
		cur.px.list <- price.list[price.list$ticker==cur.ticker,]
		px.zoo <- zoo(cur.px.list$PX_LAST, cur.px.list$date)
		if (is.null(price.list.matrix)) {
			price.list.matrix <- px.zoo
		} else {
			price.list.matrix <- cbind(price.list.matrix, px.zoo)
		}
	}
	names(price.list.matrix) <- seclist
	
	testdata <- log(custom.index)
	testheader <- c("Index")
	sourcedata <- log(price.list.matrix)
	selected.sec <- NULL
	selected.data <- testdata
	
	for (i in 1:number) {
		tstat.list <- c(rep(0, length(seclist)))
		names(tstat.list) <- seclist
	
		for (j in 1:length(sourcedata[1,])) {
			cur.sec <- seclist[j]
			tempdata <- cbind(testdata, sourcedata[,j])
			tempdata.vecm <- ca.jo(tempdata, ecdet='const', type='eigen', K=2, spec='longrun')
			tstat.list[cur.sec] <- last(tempdata.vecm@teststat)
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
		sourcedata <- sourcedata[,names(sourcedata)!=max.sec]
		seclist <- names(sourcedata)	
	}

	#selected.sec <- names(testdata)[2:(number+1)]

	return (selected.sec)
}


