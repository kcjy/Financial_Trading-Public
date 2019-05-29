#calculating weight

values <- c(500000, 200000, 100000, 20000)
names(values) <- c("Inv 1", "Inv 2", "Inv 3", "Inv 4")
weights <- values/sum(values)
barplot(weights)

returns <- Return.calculate(prices)
returns <- returns[(-1),]

Return.portfolio <- function(R, weights = NULL,
			rebalance_on = c(NA, "years", "quarters", "months", "weeks",
			"days")

#Analysing Statistics
library(PerformanceAnalytics)
sample_returns <- c(-0.02, 0.00, 0.06, 0.02)

Return.annualized(sample_returns, scale = 12) / 
Std.Dev.annualized(sample_returns, scale = 12)