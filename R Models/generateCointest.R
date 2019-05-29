"loadCoinTestResult" <-
function (startdate, enddate, period = 500, rollingwindow = 130) 
{
	library(RODBC)
	library(zoo)

	channel <- odbcConnect("Database")
	securityList <- sqlQuery(channel, "select * from SecurityList")
	tickerList <- securityList$Ticker
	alldata <- NULL
	subIndustryList <- sort(unique(securityList$SubIndustry))
	if (period==500) { 
		param <- 1
		if (rollingwindow==20) {
			param <- 3
		}
	} else if (period==250) {
		param <- 2
		if (rollingwindow==20) {
			param <- 4
		}
	} else if (period==125) {
		param <- 5
	} else if (period==750) {
		param <- 6
	}

	for (subIndustry in subIndustryList) {
		subSecList <- unique(as.character(securityList[securityList$SubIndustry==subIndustry,9]))
		secNo <- length(subSecList)
		if (secNo >= 2) {
			for (i in 1:(secNo-1)) {
				for (j in (i+1):secNo) {
					#Get the price dadta of security A
					underlyingA <- as.character(subSecList[i])
					tickerAList <- sort(as.character(securityList[securityList$Underlying==underlyingA,2]))
					#Get the NEAR month data
					tickerA.near <- tickerAList[1]
					IDA <- as.numeric(securityList[securityList$Ticker==tickerA.near,1])
					sql <- paste("select DailyPrice.TradeDate, DailyPrice.[Close], DailyPrice.Volume from DailyPrice, SecurityList where DailyPrice.SecurityID = SecurityList.ID and Ticker='",tickerA.near,"' and TradeDate between '",startdate,"' and '",enddate,"'",sep="")
					dataA.near <- sqlQuery(channel, sql)
					dataA.near.zoo <- zoo(dataA.near[,2], as.Date(dataA.near[,1]))
					#Get the NEXT month data
					tickerA.next <- tickerAList[2]
					sql <- paste("select DailyPrice.TradeDate, DailyPrice.[Close], DailyPrice.Volume from DailyPrice, SecurityList where DailyPrice.SecurityID = SecurityList.ID and Ticker='",tickerA.next,"' and TradeDate between '",startdate,"' and '",enddate,"'",sep="")
					dataA.next <- sqlQuery(channel, sql)
					dataA.next.zoo <- zoo(dataA.next[,2], as.Date(dataA.next[,1]))
					#Get the smoothed data
					dataA.zoo <- SmoothFuturePrices(merge(dataA.near.zoo, dataA.next.zoo))
					
					#Get the price data of security B
					underlyingB <- as.character(subSecList[j])
					tickerBList <- sort(as.character(securityList[securityList$Underlying==underlyingB,2]))
					#Get the NEAR month data
					tickerB.near <- tickerBList[1]
					IDB <- as.numeric(securityList[securityList$Ticker==tickerB.near,1])
					sql <- paste("select DailyPrice.TradeDate, DailyPrice.[Close], DailyPrice.Volume from DailyPrice, SecurityList where DailyPrice.SecurityID = SecurityList.ID and Ticker='",tickerB.near,"' and TradeDate between '",startdate,"' and '",enddate,"'",sep="")
					dataB.near <- sqlQuery(channel, sql)
					dataB.near.zoo <- zoo(dataB.near[,2], as.Date(dataB.near[,1]))
					#Get the NEXT month data
					tickerB.next <- tickerBList[2]
					sql <- paste("select DailyPrice.TradeDate, DailyPrice.[Close], DailyPrice.Volume from DailyPrice, SecurityList where DailyPrice.SecurityID = SecurityList.ID and Ticker='",tickerB.next,"' and TradeDate between '",startdate,"' and '",enddate,"'",sep="")
					dataB.next <- sqlQuery(channel, sql)
					dataB.next.zoo <- zoo(dataB.next[,2], as.Date(dataB.next[,1]))
					#Get the smoothed data
					dataB.zoo <- SmoothFuturePrices(merge(dataB.near.zoo, dataB.next.zoo))
					
					pairdata <- merge(log(dataA.zoo), log(dataB.zoo), all=FALSE)
					if (length(pairdata[,1]) > period) {
						testresult <- singlecointest(pairdata, period, rollingwindow)
						
						rowno <- length(testresult[,1])
						for (k in 1:rowno) {
							datarow <- testresult[k,]
							tradedate <- paste("'",format(datarow$tradedate, "%Y-%m-%d"), "'", sep="")
							startstr <- paste("INSERT INTO dbo.CoinTest VALUES (", IDA, sep="")
							endstr <- paste(datarow$sigma, ")", sep="")
							
							sql <- paste(startstr, IDB, param, tradedate, datarow$tstats, datarow$x, datarow$y, datarow$const, datarow$spread, datarow$stdev, datarow$mvavg, endstr , sep=",")
							sqlQuery(channel, sql)
						}
					}
				}
			}
		}
	}
}

"singlecointest"<-
function (d, period = 500, rollingwindow = 130)
{
#
#-----------------------------------------------------------
#
#where:			d = data frame that contains all the price data of security A and B
#				period = testing period (no of days for co-integration test)
#				rollingwindow = the rolling window for average and standard deviation
#-----------------------------------------------------------
	library(zoo)
	library(tseries)
	library(urca)
	library(TTR)
	period <- as.numeric(period)
	
	size <- length(d[1,])
	days <- length(d[,1])
	pair.frame <- NULL

	#perform co-integration test using johansen procedure
	for (i in period:days) {
		currentDate <- time(d[i])

		a.d <- d[(i-period+1):i,1]
		b.d <- d[(i-period+1):i,2]
		a.lv <- is.na(a.d)
		b.lv <- is.na(b.d)
		if (sum(a.lv)==0 && sum(b.lv)==0) {	
			a.name <- names(d)[1]
			b.name <- names(d)[2]

			pair.name <- paste(a.name, b.name, sep="/")
			res <- cointest(a.d, b.d, rollingwindow)
			pair.row <- data.frame(tradedate=currentDate, tstats=res$tstat, x=res$x, y=res$y, const=res$const, spread=tail(res$sprd,1), stdev=tail(res$sprd.sd, 1), mvavg=tail(res$sprd.sma, 1), sigma=(tail(res$sprd,1)-tail(res$sprd.sma,1))/tail(res$sprd.sd, 1))
						
			if (is.null(pair.frame)) {
				pair.frame <- pair.row
			} else {
				pair.frame <- rbind(pair.frame, pair.row)
			}

		}
		
	}
	
	return (pair.frame)
}

"cointest" <-
function (a.d, b.d, rwindow)
{
	t.zoo <- merge(a.d, b.d)
	t <- coredata(t.zoo)
	t.vecm <- ca.jo(t, ecdet='const', type='eigen', K=2, spec='longrun')
	x <- t.vecm@V[1,1]
	y <- t.vecm@V[2,1]
	const <- t.vecm@V[3,1]
	tstat <- t.vecm@teststat[2]
	sprd.zoo <- x*a.d + y*b.d + const
	sprd <- coredata(sprd.zoo)
	curlen <- length(a.d)
	if (curlen < rwindow) {
		sprd.sma <- SMA(sprd, curlen)
		sprd.sd <- rollapply(sprd, curlen, sd, align='right')
	} else {
		sprd.sma <- SMA(sprd, rwindow)
		sprd.sd <- rollapply(sprd, rwindow, sd, align='right')
	}
	return(list(x=x, y=y, const=const, tstat=tstat, sprd=sprd, sprd.sma=sprd.sma, sprd.sd=sprd.sd))
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
		currentdate <- as.Date(datelist[i], "Asia/Singapore")
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
