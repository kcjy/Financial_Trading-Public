# This script:
#     1. Generate table and chart reports for all curncy in tickers in pdf. 
#     2. Store plotly charts and summary table for shiny app
#
# eXECUTION PART: RUN section in line 340
# Manual: 1. Add curncy  by changing tickers in trendline_function.r
#       2. Adjust parameters in gen_chart_table function (adjust according to the price range and volatility)
#       3. Run the whole script

library("grid")
library("gridExtra")
library(lubridate)
source("trendline_function.r")
source("table_chart.r")
# Processing time: can be changed by changing days.pivots variable
# 5Y_daily: 30 min
# 3Y_daily: 10 min
# 1Y_daily: 5 min
# 3Y_weekly: 3 min
# 5Y_weekly: 6 min
# 10Y_weekly: 30 min
# 10D_hourly: 5 min
# 30D_hourly: 20 min

# send report: daily 5Y & 1Y
setwd('U:/Kenneth/Model Consolidation')

blgformat <-function(x) {
  # function to format numbers according to bloomberg convention
  options(scipen = 999)
  x = as.numeric(x)
  
  if (is.na(x)) return(x)

  if(x<10)  return(format(x,nsmall = 4, digits = 4,big.mark=","))
  if(x<100) return(format(x, nsmall= 3, digits=3, big.mark=","))
  return(format(x, nsmall=2, digits=2, big.mark=","))
}

generate.trendline.plot <-
  function(dat,# dat:xts format, cols : OHLC
           curncy, # ticker to generate trendlines
           min.sup.trend.length = 60, # minimum length of support trendline (length = x2- x1)
           min.res.trend.length = 90, # minimum length of resistance
           alert.thres.pip = 60, # alert threshold in pip
           break.thres.pip = 30, # threshold for breaks in [x1,x2]
           break.thres.pip.2 = 30, # # threshold for breaks in [x2,x.now]
           extend.window.length = 60, # Empty period to extend the trendlines
           # extend.trend.multiplier = 10, 
           days.pivots = 2,# defines the neignborhood of local minimum/maximum: peaks are the maximum in the neighborhood of 2*days.pivots+1
           show.pivots = T, # whether show pivots on pdf
           useHighLow = T, # method to find peaks and account for peaks/touches
           interpeak.thres = 4,# minimum distance between peaks
           gen.pdf = T,
           support.color = "red",
           resistance.color = "blue",
           horizontal.color = 'green',
           touch.tol.pip = 0.01, 
           max.pip.from.now = 800, # filterout trendlines with (current level-current close price）> max.pip.from.now
           start.shift=0, # start time(row number) to plot out the candlestick and trendline. default set to 0.(useful when want to show part of graph)
           ticksize
           ) {
    
    
    # Generate candlestick with support and resistance trendlines
    
    # Return: 
    #         graph: plotly object-- candlestick with support and resisitance
    #         res: table with each row representing resistance line
    #         sup: table with each row representing support line
    #         chosen.sup: support line with current level nearest to the current close price
    #         chosen.res: resistance line with current level nearest to the current close price
    
    
    dates = index(dat)
    close = as.numeric(dat[, "Close"])
    high = as.numeric(dat[, "High"])
    low = as.numeric(dat[, "Low"])
    open = as.numeric(dat[, "Open"])
    if (mean(dat, na.rm = T) <= 75) {pip = 0.0001} else {pip = 0.01}
    alert.thres = pip * alert.thres.pip
    break.thres = pip * break.thres.pip
    break.thres.2 = pip * break.thres.pip.2
    touch.tol = touch.tol.pip * pip
    
    max.pip.from.now = max.pip.from.now * pip
    df = extend.timeperiod(dat, extend.window.length,is.weekly = (ticksize == "Weekly"))
    
    extended.dates = df$Date
    if (gen.pdf) {
      print(plot.chart(df, curncy))
    }
    # Find all trendlines
    lines = find.trendlines(
      dat,
      curncy,
      extend.window.length = extend.window.length,
      days.pivots = days.pivots,
      useHighLow = useHighLow,
      show.pivots = show.pivots,
      interpeak.thres = interpeak.thres,
      break.thres = break.thres,
      break.thres.2 = break.thres.2,
      min.sup.trend.length = min.sup.trend.length,
      min.res.trend.length = min.res.trend.length,
      max.pip.from.now = max.pip.from.now,
      start.shift=start.shift
    )
    #
    resistance = lines$resistance
    support = lines$support
    horizontal = lines$all
    print(resistance)
    print(all(is.na(resistance)))
    if (all(is.na(resistance))){
      
      chosen.res = NA
    }else{
   
      resistance = resistance[order(abs(resistance[, "dist.to.now"])),, drop = F ]
      chosen.res = resistance[1, ]
    }
    if (all(is.na(support))){
      chosen.sup = NA
    }else{
      support = support[order(abs(support[, "dist.to.now"])),, drop = F ]
      chosen.sup = support[1, ]
    }
    if (all(is.na(horizontal))){
      chosen.all = NA
    }else{
      horizontal = horizontal[order(abs(horizontal[, "dist.to.now"])),, drop = F ]
      chosen.all = horizontal[1, ]
    }


    # -------------------------Create plotly candlestick----------------------
    g = plot.candlestick(df[(start.shift+1):nrow(dat),], title = curncy) # return a plotly candlestick
    g.addresis = add.lines(
      base.plot = g,
      xaxis = extended.dates,
      lines = resistance,
      line.name = "resistance",
      line.color = "blue",
      start.shift = start.shift
    )
    g.addsupport = add.lines(
      base.plot = g.addresis,
      xaxis = extended.dates,
      lines = support,
      line.name = "support",
      line.color = "green",
      start.shift = start.shift
    )
    g.addhorizontal= add.lines(
      base.plot = g.addsupport,
      xaxis = extended.dates,
      lines = horizontal,
      line.name = "horizontal",
      line.color = "red",
      start.shift = start.shift
    )
    graph = g.addhorizontal
    #--------------------Print candlestick to generate pdf reports------------------
    if (gen.pdf) {
      draw.trendlines(resistance, color = resistance.color, start.shift = start.shift)
      draw.trendlines(support , color = support.color, start.shift = start.shift)
      draw.trendlines(horizontal , color = horizontal.color, start.shift = start.shift)
      if (!all(is.na(chosen.sup))) {
        if (chosen.sup["dist.to.now"] < alert.thres)
          draw.trendlines(chosen.sup,
                          color = support.color,
                          start.shift = start.shift,
                          lwd = 3,
                          lty = "dashed")
        
      }
      if (!all(is.na(chosen.res))) {
        if (chosen.res["dist.to.now"] < alert.thres)
          draw.trendlines(chosen.res,
                          color = resistance.color,
                          start.shift = start.shift,
                          lwd = 3,
                          lty = "dashed")
        
      }
      if (!all(is.na(chosen.all))) {
        if (chosen.all["dist.to.now"] < alert.thres)
          draw.trendlines(chosen.all,
                          color = horizontal.color,
                          start.shift = start.shift,
                          lwd = 3,
                          lty = "dashed")
        
      }
    }
    #
    return(
      list(
        graph = graph,
        res = resistance,
        sup = support,
        horizontal = horizontal,
        chosen.sup = chosen.sup,
        chosen.res = chosen.res,
        chosen.all = chosen.all
      )
    )
  }

gen_chart_table<-function(ticksize, start.date, duration, tickers,generate.table=T ,days=NULL){
  # ticksize = c("Daily", "Weekly", "Hourly")
  # start.date: startdate to read data(used for non-tick data)
  # duration: duration of the data (Used only as a name for pdf)
  # tickers: tickers to be included in the pdf report and shiny app
  # generate.table: whether to generate summary table
  # days: the number of days to retrieve hourly data but in general it is the number 
  # in front of the the date units in duration. eg: if duration = "10Y", days = 10
  
  # Return:
  #       generate pdf reports with all trendline graphs
  #       generate pdf reports with summary table ( formated according to blg convention )
  #       table: summary table
  #       plotly.chart: list of plotly trendline graphs
  plotly.chart = list()
  
  result.df = matrix(NA, length(tickers), 9) # summary table
  row.names(result.df) = tickers
  colnames(result.df) = c(  "3rd Resist",
                            "2nd Resist",
                          "1st Resist",
                          "Current" ,
                          "1st Sup",
                          "2nd Sup",
                          "3rd Sup",
                          "Dist to 1st Resist(pip)",
                          "Dist to 1st Sup(pip)")
  pdf(paste0(duration,"_", ticksize,"_trendlines.pdf"), width = 21, height = 13) # generate pdf for trendlines
  
  for (curncy in tickers) {
    
    message(paste0("Generating ", curncy, "___", duration,"___",ticksize,"\n Progress: ", which(curncy==tickers),"/", length(tickers)))
    if (toupper(ticksize) %in% c("WEEKLY", "DAILY")) {
      dat = pull.ohlc(curncy,  start = start.date , tf=toupper(ticksize) , end = Sys.Date())
    }else if(ticksize == "Hourly") {
      dat = get.live.tickData(ticker = curncy, interval = 60, days=days)
    }
   
    dat = na.omit(dat) # dat is in xts with OHLC columns
   
    # set up parameters for trendline filtering
    min.trend.length = floor( nrow(dat) / 12)
    if (mean(dat,rm.na=T)>=75) {pip = 0.01} else {pip=0.0001}
    if ( (ticksize=="Weekly" & days <= 1 )| (ticksize=="Hourly" & days<=15 )){
      days.pivots = 1
    }else if((ticksize=="Daily" & days== 1) ){
      days.pivots = 2
    } else if(ticksize=="Daily" & days >= 2|  (ticksize=="Hourly" & days>=20 ) ) {
      days.pivots=10
    }else {days.pivots = 5}
    if (ticksize=="Hourly"){
      break.thres.pip = 0.01*diff(range(dat))/pip
      break.thres.pip.2 = 0.01 * diff(range(dat)) /pip
    }else {
      break.thres.pip = 50
      break.thres.pip.2 = 60
    }

    extend.window.length = floor(nrow(dat) / 5)
    result = generate.trendline.plot(
      dat,
      curncy = curncy,
      min.sup.trend.length = min.trend.length,
      min.res.trend.length = min.trend.length,
      alert.thres.pip = 30,
      break.thres.pip = break.thres.pip,
      break.thres.pip.2 = break.thres.pip.2,
      extend.window.length = extend.window.length,
      days.pivots = days.pivots,
      useHighLow = F,
      show.pivots = T,
      interpeak.thres = 1,
      gen.pdf = T,
      touch.tol.pip = 1,
      max.pip.from.now = 500,
      start.shift=0,
      ticksize = ticksize
    )
 
   
    plotly.chart[[curncy]] = result$graph
    if (generate.table) {
     
      if (all(is.na(result$sup))) {
        dist.sup = NA
        sup = NA
        sup2 = NA
        sup3 = NA
      } else{
        print("result:sup")
        print(result$sup)
        support.ordered = result$sup[order(abs(result$sup[,"dist.to.now"])),,drop=F ]
        dist.sup = as.integer(abs(support.ordered[1, "dist.to.now"]/pip))
        sup = support.ordered[1, "y.now"]
        if (nrow(support.ordered)>=2){
          sup2 =support.ordered[2, "y.now"]
        }else sup2 = NA
        if(nrow(support.ordered)>=3){
          sup3 = support.ordered[3, "y.now"]
        } else sup3 = NA
      }
      if (all(is.na(result$res))) {
        dist.res = NA
        res = NA
        res2 = NA
        res3 = NA

      } else{
        print("result: res")
        print(result$res)
        
        resistance.ordered = result$res[order(abs(result$res[,"dist.to.now"])), , drop=F ]
        dist.res = as.integer(abs(resistance.ordered[1, "dist.to.now"]/pip))

        
        res = resistance.ordered[1, "y.now"]
        if (nrow(resistance.ordered)>=2){
          res2 =resistance.ordered[2, "y.now"]
        } else {res2 = NA}
        if(nrow(resistance.ordered)>=3){
          res3 = resistance.ordered[3, "y.now"]
        } else res3 = NA
      }
      result.df[curncy,] = c( res3= blgformat(res3), res2 = blgformat(res2), res = blgformat(res), 
                             currennt = blgformat(dat[nrow(dat), "Close"]),
                             sup= blgformat(sup),sup2 = blgformat(sup2),sup3 = blgformat(sup3),
                             dist.res = format(dist.res, big.mark = ","),
                             dist.sup = format(dist.sup, big.mark = ","))
                            
                            
                 
      print(result.df[curncy,])
      }
    
  }
  dev.off()
  # Generate pdf report for summary table
  pdf(paste0(duration,"_", ticksize,"_table.pdf"), width = 12, height = 16)
  grid.table(result.df)
  dev.off()

  return(list(table = result.df, plotly.chart=plotly.chart))
}

## -----------------------------------  run -----------------------------------------------------

# Initialization
tickers = c( # for Zac
  "USDJPY BGN Curncy",  "EURUSD BGN Curncy",  "GBPUSD BGN Curncy",  "AUDUSD BGN Curncy",  "NZDUSD BGN Curncy" #,
  #"USDCAD BGN Curncy",  "USDCHF BGN Curncy",  "XAUUSD BGN Curncy",  "DXY Curncy",  "USDOLLAR Index",  "USDCNH BGN Curncy"
  #"KWN+1M BGN Curncy",  "NTN+1M BGN Curncy",  "IRN+1M BGN Curncy",  "IHN+1M BGN Curncy",  "PPN+1M BGN Curncy",  "MRN+1M BGN Curncy",
  #"USDTHB BGN Curncy",  "USDSGD BGN Curncy",  "JPYKRW BGN Curncy",  "ADXY Index",  "EURJPY BGN Curncy",
  #"GBPJPY BGN Curncy",  "AUDJPY BGN Curncy",  "NZDJPY BGN Curncy",  "CADJPY BGN Curncy",  "EURGBP BGN Curncy",
  #"EURAUD BGN Curncy",  "EURNZD BGN Curncy",  "EURCAD BGN Curncy",  "GBPAUD BGN Curncy",  "GBPNZD BGN Curncy",
  #"GBPCAD BGN Curncy",  "AUDNZD BGN Curncy",  "AUDCAD BGN Curncy",  "AUDSGD BGN Curncy",  "NZDCAD BGN Curncy",
  #"USGG2YR Index",  "USGG10YR Index",  "ES1 Index",
  #"NH1 Index",  "KOSPI Index",  "TWSE Index",  "SHCOMP Index",  "HSCEI Index",  "HSI Index",  "GDBR10 Index"
)
tickers = unique(tickers)
table.daily = rep(list(list()), length(chart.daily.duration))
chart.daily = rep(list(list()), length(chart.daily.duration))
chart.weekly = rep(list(list()), length(chart.weekly.duration))
table.weekly = rep(list(list()), length(chart.weekly.duration))
chart.hourly = rep(list(list()), length(chart.hourly.duration))
table.hourly = rep(list(list()), length(chart.hourly.duration))
names(chart.daily) = chart.daily.duration

chart.duration=list()
chart.duration[["Daily"]] = chart.daily.duration
chart.duration[["Weekly"]] = chart.weekly.duration
chart.duration[["Hourly"]] = chart.hourly.duration
chart.ticksize = c("Daily", "Weekly", "Hourly") 

doHourly = FALSE
# Generate table and chart reports for all curncy in tickers in pdf. 
# Store plotly charts and summary table for shiny app
#
# Manual: 1. Add curncy  by changing tickers in trendline_function.r
#       2. Adjust parameters in gen_chart_table function (adjust according to the price range and volatility)

for (tick in chart.ticksize){
  # if(tick == "Weekly" && wday(Sys.Date()) != 2) {
  #   break
  # } else if(tick == "Hourly" && !doHourly ) {
  #   break
  # }
  for (duration in chart.duration[[tick]]) {
    # Set start.date to retrieve data from duration(Str)
    duration.chars = strsplit(duration, split = "")[[1]]
    unit = duration.chars[length(duration.chars)]
    num = as.numeric(duration.chars[-length(duration.chars)])
    num = as.numeric(gsub(", ","",toString(num)))
    if (unit == "D") {
      start.date = Sys.Date() - days(num)
    } else if (unit == "M") {
      start.date = Sys.Date() - months(num)
    } else if (unit == "Y") {
      start.date = Sys.Date() - years(num)
    }
    # Generate charts and tables, pdf reports for all curncies
    res = gen_chart_table(ticksize=tick, start.date = start.date, days = num, duration = duration, tickers= tickers)
    
    # Store table and charts in respective list
    if (toupper(tick) == "DAILY") {
      chart.daily[[duration]] = res$plotly.chart
      table.daily[[duration]] = res$table
      } else if(toupper(tick)=="WEEKLY") {
 
      chart.weekly[[duration]] = res$plotly.chart
      table.weekly[[duration]] = res$table
      }else if (tick=="Hourly") {
      chart.hourly[[duration]] = res$plotly.chart
      table.hourly[[duration]] = res$table
      } 
  }
}


