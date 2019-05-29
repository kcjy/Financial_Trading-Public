testStrategy <- function(symbol, nHold=100, nHigh=200, cost=.005, ylog=FALSE, wealth.index = FALSE, ...) {
	require(quantmod)
	
	#Load Data
	myStock <- getSymbols(symbol,from='1900-01-01')
	myStock <- adjustOHLC(get(myStock),symbol.name=symbol)
	myStock <- Cl(myStock)
	
	#Determine position
	myPosition <- myStrat(myStock,nHold,nHigh)
	bmkReturns <- dailyReturn(myStock, type = "arithmetic")
	myReturns <- bmkReturns*Lag(myPosition,1)
	myReturns[1] <- 0
	names(bmkReturns) <- symbol
	names(myReturns) <- 'Me'
	
	#Add trading costs
	trade =  as.numeric(myPosition!=Lag(myPosition,1))
	trade[1] = 1
	trade = trade*cost
	myReturns = myReturns-trade
	
	#Make plot
	require(PerformanceAnalytics)
	symbol <- sub('^','',symbol,fixed=TRUE)
	Title <- paste('High=',nHigh,' Hold=',nHold,' on ',symbol,sep='')
	if (ylog) {wealth.index = TRUE}
	layout(matrix(c(1, 2, 3)), height = c(2, 1, 1.3), width = 1)
	par(mar = c(1, 4, 4, 2))
	chart.CumReturns(cbind(bmkReturns,myReturns), main=Title, ylab = "Cumulative Return",
						wealth.index = wealth.index,ylog=ylog,...)
	chart.RelativePerformance(myReturns,bmkReturns, ylab = "Relative Return", main = "")
	chart.Drawdown(cbind(bmkReturns,myReturns),legend.loc = 'bottomleft', ylab = "Drawdown", main = "")
	
	#Return Benchmarked Stats
	cbind(Me=Performance(myReturns),Index=Performance(bmkReturns))
}


#Adding Indicators to chart
chart.Posn(Portfolio = portfolio.st, symbol = "FB")
add_TA(sma50, on = 1, col = "blue")
add_TA(sma200, on = 1, col = "red")
add_TA(dvo)


chartSeries(
  c(google.stock, amazon.stock),
  theme = chartTheme("black"),
  title = "Google & Amazon Price Chart",
  TA = c(addBBands(), addSMA(n=50),addSMA(n=200))