library(stockPortfolio)
library(ggplot2)
library(reshape2)
library(quadprog)
 
stocks <- c(
 "VTSMX" = .0,
 "SPY" = .20,
 "EFA" = .10,
 "IWM" = .10,
 "VWO" = .30,
 "LQD" = .20,
 "HYG" = .10)
 
returns <- getReturns(names(stocks[-1]), freq="week") #Currently, drop index
 
eff.frontier <- function (returns, short="no", max.allocation=NULL,
 risk.premium.up=.5, risk.increment=.005){
 
 covariance <- cov(returns)
 print(covariance)
 n <- ncol(covariance)
 

 Amat <- matrix (1, nrow=n)
 bvec <- 1
 meq <- 1
 

 if(short=="no"){
 Amat <- cbind(1, diag(n))
 bvec <- c(bvec, rep(0, n))
 }
 

 if(!is.null(max.allocation)){
 if(max.allocation > 1 | max.allocation <0){
 stop("max.allocation must be greater than 0 and less than 1")
 }
 if(max.allocation * n < 1){
 stop("Need to set max.allocation higher; not enough assets to add to 1")
 }
 Amat <- cbind(Amat, -diag(n))
 bvec <- c(bvec, rep(-max.allocation, n))
 }
 
 loops <- risk.premium.up / risk.increment + 1
 loop <- 1
 
 eff <- matrix(nrow=loops, ncol=n+3)
 
 colnames(eff) <- c(colnames(returns), "Std.Dev", "Exp.Return", "sharpe")
 
 for (i in seq(from=0, to=risk.premium.up, by=risk.increment)){
 dvec <- colMeans(returns) * i
 sol <- solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
 eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution*colSums((covariance*sol$solution))))
 eff[loop,"Exp.Return"] <- as.numeric(sol$solution %*% colMeans(returns))
 eff[loop,"sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
 eff[loop,1:n] <- sol$solution
 loop <- loop+1
 }
 
 return(as.data.frame(eff))
}
 
eff <- eff.frontier(returns=returns$R, short="no", max.allocation=.50,
 risk.premium.up=1, risk.increment=.001)
 
eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),]
ealred <- "#7D110C"
ealtan <- "#CDC4B6"
eallighttan <- "#F7F6F0"
ealdark <- "#423C30"
 
ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + geom_point(alpha=.1, color=ealdark) +
 geom_point(data=eff.optimal.point, aes(x=Std.Dev, y=Exp.Return, label=sharpe),
 color=ealred, size=5) +
 annotate(geom="text", x=eff.optimal.point$Std.Dev,
 y=eff.optimal.point$Exp.Return,
 label=paste("Risk: ",
 round(eff.optimal.point$Std.Dev*100, digits=3),"\nReturn: ",
 round(eff.optimal.point$Exp.Return*100, digits=4),"%\nSharpe: ",
 round(eff.optimal.point$sharpe*100, digits=2), "%", sep=""),
 hjust=0, vjust=1.2) +
 ggtitle("Efficient Frontier\nand Optimal Portfolio") +
 labs(x="Risk (standard deviation of portfolio)", y="Return") +
 theme(panel.background=element_rect(fill=eallighttan),
 text=element_text(color=ealdark),
 plot.title=element_text(size=24, color=ealred))