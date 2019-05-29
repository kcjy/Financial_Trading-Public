require(quantstrat)
require(quantmod)
require(PerformanceAnalytics)

initdate = "1999-01-01"
from <- "2000-01-01"
to <- "2016-12-31"

Sys.setenv(TZ = "UTC")
currency("USD")
getSymbols("FB", from = from, to = to, src = "yahoo", adjust = TRUE)
stock("FB", currency = "USD", multiplier = 1)

tradesize <- 100000
initeq <- 100000

strategy.st <- portfolio.st <- account.st <- "firststrat"

initPortf(portfolio.st, symbols = "FB", initDate = initdate, currency = "USD")
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, 
currency = "USD", initEq = initeq)

initOrders(portfolio.st, initDate = initdate)
strategy(strategy.st, store = TRUE)


add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n = 50),
              label = "SMA50")
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n = 200),
              label = "SMA200")


#applyindicators
chart <- applyIndicators(strategy = strategy.st, mktdata = OHLC(FB))
head(chart, n = 5)

add.signal(strategy.st, name = "sigComparison",
		arguments = list(columns = c("SMA50", "SMA200"), relationship = "lt",
		label = "filterexit"))

add.signal(strategy.st, name = "sigCrossover",
		arguments = list(columns = c("SMA50", "SMA200"), relationship = "gt",
		label = "longfilter"))

add.signal(strategy.st, name = "sigThreshold",
		arguments = list(columns = c("SMA50", "SMA200"),
		threshold = 20,
		cross = FALSE,
		relationship = "lt",
		label = "thresholdfilter"))

add.signal(strategy.st,
		name = "sigFormula",
		arguments = list(formula = "longthreshold & longfilter", cross = TRUE),
		label = "longentry")

#RULES
#enter = buy shares, exit = sell shares
add.rule(strategy.st, name = "ruleSignal",
		arguments = list(sigcol = "filterexit", sigval = TRUE,
		orderqty = "all", ordertype = "market", orderside = "long",
		replace = FALSE, prefer = "Open", tradeSize = 1000,
		maxSize = 10000),
		type = "exit")


applyStrategy(strategy = strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]

updateAcct(account.st, daterange)
updateEndEq(account.st)

tradeStats(Portfolios = portfolio.st)
chart.Posn(Portfolio = portfolio.st, Symbol = "FB")

#David Varadi Oscillator Function
DVO <- function(HLC, nAvg=2, pctLookback=126, 
                maType="SMA", deTrend=TRUE, 
                nDT=126) {
  maType <- match.fun(maType)
  ratio <- Cl(HLC)/(Hi(HLC)+Lo(HLC))
  avgRatio <- maType(ratio, n=nAvg)
  DVO <- runPercentRank(avgRatio, n=pctLookback, exact.multiplier=1)*100
  if(deTrend) {
    DVO <- DVO + 50 - SMA(DVO, nDT)
  }
  colnames(DVO) <- "DVO"
  return(DVO)
}

#Recalculate indicators outside of strategy to add to chart
sma50 <- SMA(x = Cl(FB), n = 50)
sma200 <- SMA(x = Cl(FB), n = 200)
dvo <- DVO(HLC = HLC(FB), nAvg = 2, percentLookback = 126)



SharpeRatio.annualized(portPL, geometric = FALSE)
PortfReturns(account.st)

#StockPerformance

Performance <- function(x) {

	cumRetx = Return.cumulative(x)
	annRetx = Return.annualized(x, scale=252)
	sharpex = SharpeRatio.annualized(x, scale=252)
	winpctx = length(x[x > 0])/length(x[x != 0])
	annSDx = sd.annualized(x, scale=252)
	
	DDs <- findDrawdowns(x)
	maxDDx = min(DDs$return)
	maxLx = max(DDs$length)

	Perf = c(cumRetx, annRetx, sharpex, winpctx, annSDx, maxDDx, maxLx)
	names(Perf) = c("Cumulative Return", "Annual Return","Annualized Sharpe Ratio",
		"Win %", "Annualized Volatility", "Maximum Drawdown", "Max Length Drawdown")
	return(Perf)
}
						
#Generate P&L series
portPL <- .blotter$portfolio.firststrat$summary$Net.Trading.PL
head(portPL)

SharpeRatio.annualized(portPL, geometric = FALSE)

#Getting Returns
#Ratio between profit or loss on a given trade, divided by initial equity
instrets <- PortfReturns(account.st)
head(instrets, n = 3)
tail(instrets, n = 3)

#Getting SharpeRatio for returns
SharpeRatio.annualized(instrets, geometric = FALSE)


ibm_xts <- xts(ibm$Close,order.by=ibm$Date,frequency=365)
lnkd_xts <- xts(lnkd$Close,order.by=lnkd$Date,frequency=365)

stocks <- cbind(ibm_xts,lnkd_xts)

dygraph(stocks,ylab="Close", 
        main="IBM and Linkedin Closing Stock Prices") %>%
  dySeries("..1",label="IBM") %>%
  dySeries("..2",label="LNKD") %>%
  dyOptions(colors = c("blue","brown")) %>%
  dyRangeSelector()
	