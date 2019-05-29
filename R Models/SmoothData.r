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

"SmoothFutureReturns" <-
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
			smoothed.data[i] <- (coredata[i,2] - coredata[i-1,2])/coredata[i-1,2]
			if ((maturity.date - currentdate) < 1) {
				new.month.flag <- TRUE
			}
		} else {
			smoothed.data[i] <- (coredata[i,1] - coredata[i-1,1])/coredata[i-1,1]
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