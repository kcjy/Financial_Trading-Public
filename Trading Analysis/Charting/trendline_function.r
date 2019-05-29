

library(plotly)
library(quantmod)
require(reshape)
require(plyr)
require(tidyr)
require(dplyr)
library(openxlsx)
library(Rblpapi)
blpConnect()



tickers = c( # for Zac
  "USDJPY BGN Curncy",  "EURUSD BGN Curncy",  "GBPUSD BGN Curncy",  "AUDUSD BGN Curncy",  "NZDUSD BGN Curncy",
  "USDCAD BGN Curncy",  "USDCHF BGN Curncy",  "XAUUSD BGN Curncy",  "DXY Curncy",  "USDOLLAR Index",  "USDCNH BGN Curncy"
  #"KWN+1M BGN Curncy",  "NTN+1M BGN Curncy",  "IRN+1M BGN Curncy",  "IHN+1M BGN Curncy",  "PPN+1M BGN Curncy",  "MRN+1M BGN Curncy",
  #"USDTHB BGN Curncy",  "USDSGD BGN Curncy",  "JPYKRW BGN Curncy",  "ADXY Index",  "EURJPY BGN Curncy",
  #"GBPJPY BGN Curncy",  "AUDJPY BGN Curncy",  "NZDJPY BGN Curncy",  "CADJPY BGN Curncy",  "EURGBP BGN Curncy",
  #"EURAUD BGN Curncy",  "EURNZD BGN Curncy",  "EURCAD BGN Curncy",  "GBPAUD BGN Curncy",  "GBPNZD BGN Curncy",
  #"GBPCAD BGN Curncy",  "AUDNZD BGN Curncy",  "AUDCAD BGN Curncy",  "AUDSGD BGN Curncy",  "NZDCAD BGN Curncy",
  #"USGG2YR Index",  "USGG10YR Index",  "ES1 Index",
  #"NH1 Index",  "KOSPI Index",  "TWSE Index",  "SHCOMP Index",  "HSCEI Index",  "HSI Index",  "GDBR10 Index"
)
#----------------------Data Retrieval------------------------------------

## Retrieve daily data

get.live.tickData <-
  function(ticker,
           interval = 5,
           days = NULL,
           start = "2017-5-25 12:00",
           end = Sys.time()) {
    # ticker = curncy
    # interval = 5 # tick interval
    # d = 2 # number of days
    if (is.null(days)) {
      start = start #strptime(start, "%Y-%m-%d %H:%M")
      end = end
      # strptime(end, "%Y-%m-%d %H:%M")
    }
    else{
      start =  round(Sys.time(), units = "hours") - 60 * 60 * 24 * days
      end =  round(Sys.time(), units = "hours")
    }
    data <-
      getBars(
        ticker,
        eventType = "TRADE",
        barInterval = interval,
        startTime = start,
        endTime = end,
        returnAs = getOption("blpType", "matrix")
      )
    data = data[, 1:5]
    colnames(data) = c("Date", "Open", "High", "Low", "Close")
    data = as.xts(data[, 2:5], order.by = data$Date)
    
    return(data)
  }

pull.ohlc = function(idx,
                     start = NULL,
                     end = NULL,
                     tf = "DAILY",
                     period = NULL) {
  if (is.null(period)) {
    a <- start
    b <- iif(is.null(end), Sys.Date(), end)
  } else {
    b = Sys.Date()
    #period = 100
    a = b - period
  }
  overrides.px <- structure(tf, names = c("periodicitySelection"))
  
  data = bdh(
    securities = idx,
    fields = c("OPEN", "HIGH", "LOW", "PX_LAST"),
    start.date = a,
    end.date = b,
    options = overrides.px
  )
  
  if (length(idx) == 1) {
    colnames(data) = c("date", "Open", "High", "Low", "Close")
    data = as.xts(data[, 2:5], order.by = data$date)
    data
  } else {
    for (i in 1:length(idx)) {
      temp = data[[i]]
      colnames(temp) = c("date", "Open", "High", "Low", "Close")
      temp = as.xts(temp[, 2:5], order.by = temp$date)
      data[[i]] = temp
    }
    data
  }
  
}


#------------------ Helper functions for lines ---------------------------
# Given 2 points, return the intercept and slope

find.grad.constant = function(x) {
  x1 = x[1]
  y1 = x[2]
  x2 = x[3]
  y2 = x[4]
  m = (y2 - y1) / (x2 - x1)
  constant = y2 - m * x2
  return(c(m, constant))
}
find.constant. = function(x) {
  x1 = x[1]
  y1 = x[2]
  x2 = x[3]
  y2 = x[4]
  m = 0
  constant = y2 - 0 * x2
  return(c(m, constant))
}

ln <- function(x, m, c) {
  if (m != 0)
    m * x + c
  else
    rep(c, length(x))
}#Y=mX+c

iif <-
  function(statement, true.ret, false.ret) {
    if (statement) {
      return(true.ret)
    } else{
      return (false.ret)
    }
  }

extend.timeperiod <- function(data, extend.length = 60,is.weekly=F) {
  require(timeDate)
  # return x with extended rows filled with NA
  #data = tick.dat
  Date = index(data)
  time.interval = min(Date[2:11] - Date[1:10])
  endtime = tail(Date, 1)
  new.index = seq(
    from = endtime + time.interval,
    by = time.interval,
    length.out = extend.length * 10
  )
  if (is.weekly) trading.day.index = new.index[1:extend.length]
  else trading.day.index = new.index[isBizday(as.timeDate(new.index))][1:extend.length]
  extended.data = tail(data, extend.length)
  index(extended.data) = trading.day.index
  coredata(extended.data) = as.matrix(rep(NA, 4 * extend.length, nrow = extend.length, ncol =
                                            4))
  
  data = rbind(data, extended.data)
  Date = as.POSIXlt(index(data))
  df = data.frame(cbind(Date, as.data.frame(data)), row.names = NULL)
  return(df)
  
}

find_peaks <- function(x, # x: data series
                       m = 2,# m: neighborhood. larger m -> less peak. Each peak i, x[i+1] is the largest in the neighborhood of radius=m
                       pip.thresh = 0,# pip.thresh: filter out the peaks that are too similar to the previous peak. larger thres -> less peaks
                       interpeak.thres = 1 # minumum distance between peaks
                       ) {
 
  
  
  # return peak indices
  shape <-
    diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) # find the general peaks
  pks <-
    sapply(
      which(shape < 0),
      FUN = function(i) {
        # get peaks that is local maxima in the neighborhood of m
        z <- i - m + 1
        z <- ifelse(z > 0, z, 1)
        w <- i + m + 1
        w <- ifelse(w < length(x), w, length(x))
        
        if (all(x[c(z:i, (i + 2):w)] <= x[min(i + 1, length(x))]))
          return(i + 1)
        else
          return(numeric(0))
      }
    )
  pks <- sort(unlist(pks))
  if(length(pks)==1) return(pks)
  rm.idx = c()
  for (i in c(1:(length(pks) - 1))) {

    if (abs(pks[i + 1] - pks[i]) <= interpeak.thres)
      rm.idx = c(rm.idx, i) # for consecutive peaks with same value, return the latest peak
  }
  if (length(rm.idx) != 0)
    pks = pks[-rm.idx]
  
  
  if (pip.thresh != 0) {
    if (sign(pip.thresh) < 0)
      pip.thresh <- -pip.thresh
    return(pks[(abs((x[pks] / coredata(x[pks + 1])) - 1) * 100)  > pip.thresh]) # filter out the peaks that are too similar to the previous peak.
  }
  else
    return(pks)
}



npivotpoint <-
  function(data,
           n = 1,
           pip.swing = 0,
           useHighLow = T,
           interpeak.thres = 1)
    
  {
    # data is df Open High Low Close
    # n=#bars on each side of pivot (peaks are local maxima in the radius of n). default=1: highest for 3 days on either side
    # useHighLow: if true, use high low, else use open, close
    # pip.swing: filter out the peaks that are too similar to the previous peak. larger thres -> less peaks
    require(quantmod)
    
    if (ncol(data) > 4) {
      colnames(data) = c("Open", "High", "Low", "Close", colnames(data)[5:ncol(data)])
    }
    
    else{
      colnames(data) = c("Open", "High", "Low", "Close")
    }
    
    
    if (is.xts(data)) {
      data = as.data.frame(as.matrix(data[, 1:4]))
      row.names(data) <- index(data)
    }
    else {
      row.names(data) <- index(data)
    }
    
    if (useHighLow)
    {
      h = data$High
      l = data$Low
    } else {
      h = apply(cbind(data$Open, data$Close), 1, max)
      h = t(t(h)) # m*1 matrix
      l = apply(cbind(data$Open, data$Close), 1, min)
      l = t(t(l))
    }
    
    
    p = find_peaks(x = h,
                   m = n,
                   interpeak.thres = interpeak.thres)
    #p=p[which(p<=.max)]
    t = find_peaks(x = -l,
                   m = n,
                   interpeak.thres = interpeak.thres) # find minimal with -x
    
    #t=t[which(t<=.max)]
    highs = cbind(p, h[p])
    lows = cbind(t, l[t])
    return(list(pivot.high = highs, pivot.low = lows))
  }

is.interbreak <- function(line, # a vector indicating a trendline (m: gradiant, constant: intercept )
           data,# OHLC data
           tol, # tolerance threshold of a break
           type, # type = c("resistance", "support")
           start = "x1", # start point of the line
           end = "x2", # end point of the line
           useHighLow) {
  # Return: boolean indicating if there's a break between start and end point of the line
    x1 = as.numeric(line[start])
    x2 = as.numeric(line[end])
    
    if (abs(x1 - x2) <= 1) {
      return(NA)
    }
    
    x = c(x1:x2)
    if (useHighLow) {
      highs = data[x, 2]
      lows = data[x, 3]
    }
    else {
      highs = apply(data[x, c(1, 4)], 1, max)
      lows = apply(data[x, c(1, 4)], 1, min)
    }
    lin = ln(x, m = as.numeric(line['m']), c = as.numeric(line['constant']))
    if (type == "resistance") {
      deviate = highs - lin
      
    }
    else {
      deviate = lin - lows
    }
    is.break = any(deviate > tol)
    return(is.break)
    
  }

find.dup.pivots = function(result, col) {
  if (is.na(nrow(result))) {
    return(NA)
  }
  # return index of rows with duplicated values in result$col
  # eg: result[unlist(find.dup.pivots, "x2)] -> rows with duplicated x2's
  # result = filteredout.breaks.lines
  # col="x2"
  a = result[, col]
  ans = list()
  temp = c()
  checker = c(0)
  for (i in 1:(length(a) - 1)) {
    if (!i %in% checker) {
      p = a[i]
      for (j in (i + 1):length(a)) {
        q = a[j]
        
        if (!is.na(p) & !is.na(q))
          if (p == q)
            temp = c(temp, i, j) # index i, j have same value
      }
      temp = temp[!duplicated(temp)]
      ans = c(ans, list(temp))
      checker = c(checker, temp)
      temp = c()
    }
  }
  ans = ans[lapply(ans, is.null) == F]
  ans
}


find.similar.points = function(lines, col, tol = 0.001) {
  if (is.na(nrow(lines))) {
    return(NA)
  }
  # return index of rows with duplicated values in lines$col
  # eg: lines[unlist(find.dup.pivots, "x2)] -> rows with duplicated x2's
  
  #lines = filteredout.breaks.lines
  #col="x2"
  a = lines[, col]
  ans = list()
  temp = c()
  checker = c(0)
  for (i in 1:(length(a) - 1)) {
    if (!i %in% checker) {
      p = a[i]
      for (j in (i + 1):length(a)) {
        q = a[j]
        if (!is.na(p) & !is.na(q)) {
          if (abs(p - q) <= tol)
            temp = c(temp, i, j) # index i, j have same value
        }
      }
      temp = temp[!duplicated(temp)]
      ans = c(ans, list(temp))
      checker = c(checker, temp)
      temp = c()
    }
  }
  ans = ans[lapply(ans, is.null) == F]
  ans
}

count.touch <- function(line, pivots, tol) {
  x = pivots[, 1]
  y = pivots[, 2]
  y.line = x * line["m"] + line["constant"]
  return(sum(abs(y - y.line) <= tol))
}

rm.dup.endlines <- function(lines, endpoint = "x2") {
  ## for lines with the same end points, return the one with the the most touches and moderate slope
  if (is.na(nrow(lines)) | nrow(lines) == 1)
    return(lines)
  groupby.endpoint = find.dup.pivots(lines, endpoint)
  
  if (length(groupby.endpoint) == 0)
    return(lines)
  filtered = lines[-unlist(groupby.endpoint), ]
  for (group in groupby.endpoint) {
    lines.group = lines[unlist(group), , drop = F]
    max.touch.indx = which(lines.group[, "num.touch"] == max(lines.group[, "num.touch"]))
    selected.line = lines.group[max.touch.indx, , drop = F]
    min.grad.index = which(abs(selected.line[, "m"]) == min(abs(selected.line[, "m"])))
    selected.line = selected.line[min.grad.index, , drop = F]
    filtered = rbind(filtered, selected.line)
  }
  
  rownames(filtered) = NULL
  return(filtered)
}

# ----------------------- Trendline properties -----------------

find.all.trendlines <-
  function(data,
           extend.window.length, # number of empty bars in the end to extend the trendlines
           support.color = "red", # support color in candlestick
           resistance.color = "blue",  
           extend.trend.multiplier = 10, # extend the trendline 10 times its original length (x2-x1)
           days.pivots = 3, # range of local peaks and lows
           useHighLow = T, # whether use high low data or open close data to find peaks and breaks
           show.pivots = T, # True: add marks in the peaks
           type = "resistance", # type = c("resistance", "support")
           break.thres = 0.1, # break tolorance between x1 and x2
           break.thres.2 = 30 , # break tolerance between x2 and x3
           min.trend.length = 3, 
           interpeak.thres = 5,
           touch.tol = 0.01, # threshold of distance between a bar and line value to be count a touch
           max.pip.from.now = 100, # maximum distance of the trendline current level from current price
           start.shift = 0 # start day to plot the data. By default set to 0
           ) {
  
    days = nrow(data)
    
    pivots.list = npivotpoint(data,
                              n = days.pivots,
                              interpeak.thres = interpeak.thres,
                              useHighLow = useHighLow)
    
    if (show.pivots) {
      allowance = median(abs(data[, 1] - data[, 4])) # abs(open-close) for plotting: distance of the label above the peaks
      piplabel = function(x) {
        return(substring(gsub('[.]', '', as.character(x)), 2, 5))
      } # return number after the decimal points
      if (type == "resistance") {
        points(
          pivots.list$pivot.high[, 1]-start.shift,
          pivots.list$pivot.high[, 2] + allowance,
          pch = 25,
          bg = resistance.color,
          col = resistance.color,
          cex = 0.7
        ) # Mark the peaks
        text(
          pivots.list$pivot.high[, 1]-start.shift,
          pivots.list$pivot.high[, 2] + 2 * allowance,
          labels = piplabel(pivots.list$pivot.high[, 2]),
          col = 'black',
          cex = 0.7,
          font = 2
        )
        
      }
      else{
        points(
          pivots.list$pivot.low[, 1]-start.shift,
          pivots.list$pivot.low[, 2] - allowance,
          pch = 24,
          bg = support.color,
          col = support.color,
          cex = 0.7
        ) #Mark the lows
        text(
          pivots.list$pivot.low[, 1]-start.shift,
          pivots.list$pivot.low[, 2] - 2 * allowance,
          labels = piplabel(pivots.list$pivot.low[, 2]),
          col = 'black',
          cex = 0.7,
          font = 2
        )
        
      }
    }
    if (type == "resistance") {
      pivots = pivots.list$pivot.high
    } else if (type=='support'){
      pivots = pivots.list$pivot.low
    } else {
      pivots = do.call(rbind, pivots.list)
    }
    if (all(is.na(pivots))) {
      print("No pivots found")
      return(NA)
    }
    
    ln = nrow(pivots) # number of pivots
    
    if (ln <= 1) {
      print("Less than two pivots found. Couldn't plot trendline")
      return(NA)
    }
    totalcounts = choose(ln, 2)
    result = matrix(rep(NA, 4 * totalcounts), ncol = 4, nrow = totalcounts)
    k = 1
    for (i in 1:ln){
      if (i == ln)
        break
      for (j in (i + 1):ln)
      {
        if (j == i + 1)
        {
          result[k, ] <- c(pivots[i, ], pivots[j, ])
          k = k + 1
        } else {
          result[k, ] <- c(pivots[i, ], pivots[j, ])
          k = k + 1
        }
      }
    }
    colnames(result) = c('x1', 'y1', 'x2', 'y2')
    #fill columns of m and constant
    
    result = cbind(result, length = abs(result[, "x1"] - result[, "x2"]))
    result = result[result[, "length"] >= min.trend.length, , drop = F]
    
    print(paste0(
      "number of trendlines after deleting the near trend: ",nrow(result)
    ))
    if (nrow(result)==0){
      return(result)
    }
    #-------------------------------COMPUTE GRADIANT AND INTERCEPT---------------
    if (is.null(nrow(result)) | nrow(result) == 1) {
      # incase of only one line
      t1 = find.grad.constant(result)
    } else {
      t1 = apply(result, 1, find.grad.constant)
      }
    new.colnames = c(colnames(result), 'm', 'constant')
    
    result = cbind(result, t(t1))
    colnames(result) <- new.colnames
    extend.band = break.thres / 2
    ylim = c(min(data[, "Low"]) - extend.band, max(data[, "High"]) + extend.band)
    l = result[, "x2"] - result[, "x1"]
    result = cbind(result, x.now = rep(days, nrow(result)),y.now = days * result[, "m"] + result[, "constant"])
    
    x3 =  rep(days + extend.window.length, nrow(result))
    y3 = result[, "m"] * x3 + result[, "constant"] 
    for (i in c(1:length(y3))) {
      if (y3[i] > ylim[2]) {
        y3[i] = ylim[2]
        x3[i] = (y3[i] - result[i, "constant"]) / result[i, "m"]
      }
      else if (y3[i] < ylim[1]) {
        y3[i] = ylim[1]
        x3[i] = (y3[i] - result[i, "constant"]) / result[i, "m"]
      }
    }
    
    result = cbind(result, x3, y3)
    result = result[x3 >= (days + extend.window.length), , drop = F]
    
    
    # --------------------------------FILTER OUT LINES WITH BREAKS IN [X1,X2] -------------------
    interbreaks = unlist(alply(
        result,1,is.interbreak,data = data,start = "x1",end = "x2",tol = break.thres,
        type = type,useHighLow = useHighLow
      ))
    filteredout.breaks.lines = result[coredata(which(interbreaks == F)), , drop = F]
    print(paste0(
      "number of trendlines after deleting the trendlines with interbreaks: ",nrow(filteredout.breaks.lines)
    ))
    result = filteredout.breaks.lines
    # --------------------------------FILTER OUT LINES WITH BREAKS IN [X2,X.NOW] -------------------
    interbreaks.2 = unlist(
      alply(
        result,1,is.interbreak,data = data,start = "x2",end = "x.now",tol = break.thres.2,
        type = type,useHighLow = useHighLow
      )
    )
    
    filteredout.breaks.lines = result[coredata(which(interbreaks.2 == F)), , drop = F]
    print(paste0(
      "number of trendlines after deleting the trendlines with interbreaks: ",nrow(filteredout.breaks.lines)
    ))
    result = filteredout.breaks.lines
    # --------------------------------ADD NUMBER OF TOUCHES AND DISTANCE FROM CURRENT LEVEL-------------------
    result = cbind(result, num.touch = unlist(apply(result, 1, FUN = count.touch, pivots, touch.tol)))
    dist =   result[, "y.now"] - as.numeric(data[days, "Close"])
    result = cbind(result, dist.to.now = dist)
    if (is.null(nrow(result)) | nrow(result) <= 1) {return(result)}
    # --------------------------------FILTER:  SELECT ONE LINE FOR EACH ENDPOINT -------------------
    filterout.dup.end = rm.dup.endlines(result)
    
    print(paste0(
        "number of trendlines after deleting the trendlines with same end points: ",nrow(filterout.dup.end)
      ))
    result = filterout.dup.end
    # --------------------------------FILTER OUT LINES WITH CURRENT LEVEL TOO FAR FROM CURRENT PRICE -------------------
    filterout.offlines = result[abs(result[, "dist.to.now"]) <= max.pip.from.now, , drop = F]
    print(paste0(
        "number of trendlines after deleting (y.now-trendline.level)>max.pip: ", nrow(filterout.offlines)
      ))
    # --------------------------------FILTER: SELECT 5 LINES NEARES TO THE CURRENT LEVEL-------------------
    result = result[order(abs(result[, "dist.to.now"]))[1:min(nrow(result), 5)], , drop = F]
    
    return(result)
    
  }
# -------------------------Plot functions-------------------
plot.candlestick <- function(df, title) {
  candlestick = plot_ly(
    df,
    type = "candlestick",
    x = df$Date ,
    open = ~ Open,
    high = ~ High,
    low = ~ Low,
    close = ~ Close,
    yaxis = "y",
    increasing = list(line = list(color = "#455D7A")),
    decreasing = list(line = list(color = "#F95959")),
    name = "Price",
    height = 600,
    width = 1024
  ) %>%
    
    layout(
      showlegend = F,
      
      yaxis = list(
        title = "Price",
        domain = c(0, 0.9),
        showgrid = T
      ),
      # xaxis = list(type = "category"),
      annotations = list(
        # title of the plot
        list(
          xref = "paper",
          yref = "paper",
          x = 0,
          y = 1,
          showarrow = F,
          xanchor = "left",
          yanchor = "top",
          align = "left",
          text = paste0("<b>", title, "</b>")
        ),
        # date range of the plot
        list(
          xref = "paper",
          yref = "paper",
          x = 0.75,
          y = 1,
          showarrow = F,
          xanchor = "left",
          yanchor = "top",
          align = "left",
          text = paste(range(df$Date), collapse = " : "),
          font = list(size = 8)
        )
      ),
      plot_bgcolor = "#f2f2f2"
    )
  
  return(candlestick)
}

add.lines <- function (base.plot,
            xaxis,
            start.shift=0,
            lines,
            line.name,
            line.color) {
    if (all(is.na(lines))) {
      print("No trendline to draw")
      return(base.plot)
    }
    max.val = max(lines[, c("y1", "y.now")])
    min.val = min(lines[, c("y1", "y.now")])
    ymax = (max.val - min.val) * 0.3 + max.val
    ymin = -(max.val - min.val) * 0.3 + min.val
    plt = base.plot
    for (i in 1:nrow(lines)) {
      line = lines[i, ]
      x = line["x1"]:floor(line["x3"])
      x.pts = xaxis[x]

      y = ln(x, line['m'], line['constant'])
      if (tail(x.pts, 1) >= line["x.now"]) {
        plt = plt %>% add_lines(
          x = x.pts-start.shift,
          y = y,
          line = list(width = 1, color = line.color),
          name = line.name,
          inherit = F
        )
      }
    }
    return(plt)
  }

draw.trendlines <- function(lines,
                            color,
                            start.shift=0,
                            lwd = 1,
                            lty = 1) {

    if (all(is.na(lines))) {
      print("No trendline to draw")
      return()
    }
    print(nrow(lines))
    if ( is.null(nrow(lines))){
      x0=lines["x1"]-start.shift
      y0=lines["y1"]
      x1 = lines["x3"] - start.shift
      y1 = lines["y3"]
    } else{
    lines = as.data.frame(lines)
    for (i in c(1:nrow(lines))) {
      line = lines[i, ]
      x0 = line$x1 - start.shift
      y0 = line$y1
      if (x0<=0) {
        y0= start.shift*line$m+line$constant
        x0= 1
      }
      segments(
        x0 = x0,
        y0 = y0,
        x1 = line$x3 - start.shift,
        y1 = line$y3,
        lwd = lwd,
        lty = lty,
        col = color
      )
    }
  }
}



# ------------------------ run -------------------------------

find.trendlines <-
  function(dat,
           curncy,
           extend.trend.multiplier = 10,
           extend.window.length = 60,
           days.pivots = 4,
           useHighLow = T,
           show.pivots = T,
           interpeak.thres = 5,
           break.thres = 30,
           break.thres.2 = 100 ,
           min.res.trend.length = 50,
           min.sup.trend.length = 30,
           touch.tol = 0.01,
           max.pip.from.now = 100,
           start.shift=0) {
# Wrapper function for find.all.trendlines function
    # return:
    #   resistance and support lines
    
    print("Finding resistance lines")
    resistance = find.all.trendlines(
      dat,
      extend.trend.multiplier = extend.trend.multiplier,
      extend.window.length = extend.window.length,
      days.pivots = days.pivots,
      useHighLow = useHighLow,
      show.pivots = show.pivots,
      interpeak.thres = interpeak.thres,
      type = "resistance",
      break.thres = break.thres,
      break.thres.2 = break.thres.2,
      min.trend.length = min.res.trend.length,
      touch.tol = touch.tol,
      max.pip.from.now = max.pip.from.now,
      start.shift=start.shift
    )
    
    print("Finding support lines")
    support = find.all.trendlines(
      dat,
      extend.window.length = extend.window.length,
      extend.trend.multiplier = extend.trend.multiplier,
      days.pivots = days.pivots,
      useHighLow = useHighLow,
      show.pivots = show.pivots,
      interpeak.thres = interpeak.thres,
      type = "support",
      break.thres = break.thres,
      break.thres.2 = break.thres.2,
      min.trend.length = min.sup.trend.length,
      touch.tol = touch.tol ,
      max.pip.from.now = max.pip.from.now,
      start.shift=start.shift
    )
    
    print("Finding support lines")
    all = find.all.trendlines(
      dat,
      extend.window.length = extend.window.length,
      extend.trend.multiplier = extend.trend.multiplier,
      days.pivots = days.pivots,
      useHighLow = useHighLow,
      show.pivots = show.pivots,
      interpeak.thres = interpeak.thres,
      type = "all",
      break.thres = break.thres,
      break.thres.2 = break.thres.2,
      min.trend.length = min.sup.trend.length,
      touch.tol = touch.tol ,
      max.pip.from.now = max.pip.from.now,
      start.shift=start.shift
    )
    print("Finding horizontal lines")
    return(list(support = support, resistance = resistance, all = all))
  }

plot.chart = function(df, name = "Time Series"){
  myPars <- chart_pars()
  #myPars$mar <- c(3, 2, 0, .2) # default is c(3, 1, 0, 1)  # bottom, left, top, right
  myPars$cex <- 0.8 #' Increase font size of both x and y axis scale ticks
  mychartTheme <- chart_theme()
  mychartTheme$rylab = FALSE  #' Don't show y-axis on right side of plot to save space
  mychartTheme$format.labels = '%b %d'
  mychartTheme$col$dn.col = 'black'
  mychartTheme$col$up.col = 'white'
  mychartTheme$col$bg.col = "white"
  x = xts(df[,-1], order.by = as.POSIXct(df$Date))
  chart_Series(
    x,
    type = "candlesticks",
    pars = myPars,
    name = name,
    theme =  mychartTheme
  )
}

