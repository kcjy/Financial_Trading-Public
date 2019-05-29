# We're obtaining the weekly data
# More information about Yahoo Finance Historical Quotes API
# https://code.google.com/p/yahoo-finance-managed/wiki/csvHistQuotesDownload
indexUrl <- 'http://ichart.yahoo.com/table.csv?s=%5EGSPC&a=00&b=3&c=1950&d=11&e=19&f=2015&g=w'
stockUrl <- 'http://ichart.yahoo.com/table.csv?s=GS&a=04&b=4&c=1999&d=11&e=19&f=2015&g=w'
# stockUrl <- 'http://ichart.yahoo.com/table.csv?s=MSFT&a=02&b=13&c=1986&d=11&e=19&f=2015&g=w'


# read.csv fetches the csv data from an api endpoint if a valid url is provided 
indexData <- read.csv(indexUrl)
stockData <- read.csv(stockUrl)

# Making sure R knows the Date column is Date
indexData$Date <- as.Date(indexData$Date)
stockData$Date <- as.Date(stockData$Date)

# Usually index contains more values
# Data received form yahoo endpoints are ordered from latest to oldest so 
# we only subset the part of index data that contains stock information in it
indexData <- indexData[1:length(stockData$Date), ]

# Making sure dates are matching and then we grab the weekly close prices of both index and the stock
range <- indexData$Date == stockData$Date
indexPrices <- indexData$Close[range]
stockPrices <- stockData$Close[range]

# Calculating the weekly return, e.g (x2-x1)/x1
indexReturns <- ( indexPrices[1:(length(indexPrices) - 1)] - indexPrices[2:length(indexPrices)] ) / indexPrices[2:length(indexPrices)]
stockReturns <- ( stockPrices[1:(length(stockPrices) - 1)] - stockPrices[2:length(stockPrices)] ) / stockPrices[2:length(stockPrices)]

# using R's lm function, we run a  regression analysis 
# we're using stockReturns as our y value
# and using indexReturns as our x value
# y ~ x is our formula
fit <- lm(stockReturns ~ indexReturns)
result <- summary(fit)
# summary gives us a lot of useful information, but we're mostly in beta value !!
beta <- result$coefficients[2,1]
print(beta)