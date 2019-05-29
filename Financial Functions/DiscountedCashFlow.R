load.packages('quantmod') 
tickers = spl('AAPL')
tickers.temp = spl('NASDAQ:AAPL')

data.fund <- new.env()
for(i in 1:len(tickers))
  data.fund[[tickers[i]]] = fund.data(tickers.temp[i], 80, 'annual')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)            

fund = data.fund[[tickers[1]]]
fund.date = date.fund.data(fund)            
price = Cl(data[[tickers[1]]]['2000::'])


FCF = get.fund.data('free cash flow', fund, fund.date)
IC = get.fund.data('invested capital', fund, fund.date)
SALE = get.fund.data('total revenue', fund, fund.date)
CEQ = get.fund.data('total equity', fund, fund.date)
CSHO = get.fund.data('total common shares out', fund, fund.date)
EPS = get.fund.data('basic eps - total', fund, fund.date)
DPS = get.fund.data('dividends per share', fund, fund.date)
NP = get.fund.data('net profit', fund, fund.date)

CROIC = FCF/IC

g = runMean(CROIC, 5)
cash = runMean(FCF, 5)

               
compute.DCF.IV <- function(cash, eqity, shares, g, R) {
  if( cash <= 0 ) return(NA)
  
  if( len(R) == 1 ) R = rep(R, len(g))
  
  value = eqity + sum(cash * cumprod(1 + g) / cumprod(1 + R))
  return( value / shares )
}

# Assumptions: Company will grow for 1st 3 years at current Growth Rate,
# slowed down by 20% for the next 4 year, and slow down by a further 20% for the next 3 years
# and finally 3% growth for the next 10 years

# Discount Rate is 10.57%
              
dcf.price = NA * g
i.start = which(!is.na(g))[1] 

for(i in i.start : nrow(g)) {
  # Create Growth Rate scenario:      
  g.scenario = c(rep(g[i],3), rep(g[i],4)*0.8, rep(g[i],3)*0.8*0.8, rep(3/100,10))
  
  # Compute Intrinsic Value
  dcf.price[i] =
    compute.DCF.IV(cash[i], CEQ[i], CSHO[i], g.scenario, 0.1057)
}

df <- read.xlsx2("C:/Users/Kenneth/Desktop/Book1.xlsx", sheetIndex = 1)
FCF = df$Free.Cash.Flow
IC = df$Invested.Capital
SALE = df$Total.Revenue
CEQ = df$Total.Equity
CSHO = df$Shares.Outstanding
EPS = df$Basic.EPS
DPS = df$Dividends.per.share
NP = df$Net.Profit

CROIC = FCF/IC

g = runMean(CROIC, 5)
cash = runMean(FCF, 5)

g.scenario <- c(0.045, 0.045, 0.045)
d <- cash * cumprod(1+g.scenario) / cumprod(0.061)
d <- na.trim(d)
value <- CEQ + sum(d)
value/CSHO


plota(price, type='l', log = 'y', col='blue', main=tickers[1],
      ylim=range(price,dcf.price,na.rm=T))
plota.lines(dcf.price, type='s', col='red', lwd=2)
plota.legend('Close,Intrinsic Value', 'blue,red', list(price, dcf.price))   


plota(g, type='b', col='blue', pch=0, main='Growth Rate')   


plota(cash, type='b', col='blue', pch=0, main='Free Cash Flows')

#Historical Mean of S&P500
GSPC_prices <- GSPC[, "GSPC.Adjusted", drop = FALSE]

GSPC_mean <- mean(periodReturn(GSPC_prices, period='yearly', subset = NULL, 
                    type = 'arithmetic', leading = TRUE))

eq_wt <- 736929.008 / (736929.008 + 75680)
eq_dbt <- 75680 / (736929.008 + 75680)
#calculating cost of equity
rf_rate <- 0.0260
apple_stockbeta <- 1.45
cost_equity <- rf_rate + apple_stockbeta*(0.0875 - rf_rate)
#Interest Expense / Book Value of Debt
cost_debt <- 1456/75680
#Two-year average Tax Rate
t <- 0.25965

r <- (eq_wt * cost_equity) + (eq_dbt * cost_debt * (1 - t))

#Least Squares Estimation of EPS
plot(lsfit(fund.date, EPS, wt = NULL))


plot(fund.date, EPS, type = "l", col = "blue", main = "Least Square Estimate of EPS", 
     xlab = "Year", ylab = "EPS")
lsfit(fund.date, EPS)
abline(lm(EPS ~ fund.date))

plot(x,y, axes = FALSE) # Fifth plot
axis(2, at = yticks, labels = yticks, col.axis="red", las=2)
title("Manipulated Y-axis")

#Polynomial Regression
data.frame(date=index(EPS), coredata(EPS))
EPS_data <- na.trim(EPS_data)
lm(EPS_data$coredata.EPS. ~ poly(EPS_data$date, 6))
fit6 <- lm(formula = EPS_data$coredata.EPS. ~ poly(EPS_data$date, 6))
plot(EPS_data$date, EPS_data$coredata.EPS., type="p", lwd=3, xlab = "Year", 
     ylab = "EPS", main = "Polynomial Regression of EPS", col="blue")
points(EPS_data$date, predict(fit6), type = "l", lwd=2)

dates <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
           2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
plot(dates, EPS_data$coredata.EPS., type="p", lwd=3, xlab = "Year", 
     ylab = "EPS", main = "Least Square Estimation of EPS", col="blue")
fit <- lm(EPS_data$coredata.EPS. ~ dates)
abline(fit)

legend("topleft", legend=c("LSQ Estimate", "EPS"),
        col=c("red", "blue"), lty=1:2, cex=0.8)

YearOverYear<-function (x,periodsPerYear){
  if(NROW(x)<=periodsPerYear){
    stop("too few rows")
    }
  else{
    indexes<-1:(NROW(x)-periodsPerYear)
    return(c(rep(NA,periodsPerYear),(x[indexes+periodsPerYear]-x[indexes])/x[indexes]))
    }
}
lsfit(dates, EPS)
lsfit(dates, log(EPS))
mean(EPS)
g = 0.08351796
g.scenario = c(rep(g,3), rep(g,4)*0.75, rep(g,3)*0.75*0.75, rep(3/100,10))
compute.DCF.IV(cash[i], CEQ[i], CSHO[i], g.scenario, 0.105775)
#Q1 2017 Results
compute.DCF.IV(52455, 132390, 5255.4, g.scenario, 0.105775)