
table.ticksize = c("Hourly" ,"Daily", "Weekly") #"15 Min", 
chart.ticksize = c("Daily", "Hourly",  "Weekly") #"15 Min",
chart.tick.duration = c("5D","10D", "30D") #"4D","20D",
chart.hourly.duration = chart.tick.duration
chart.daily.duration = c("1Y", "3Y","5Y")#"3M"
chart.weekly.duration = c( "3Y","5Y","10Y") #"1Y",
plot.trendline<-function(curncy, tick.size, duration){
  # Return plotly chandlestick with trendlines from input in shiny
  # data is generated in gen_chart.r
  print(tick.size)
  print(curncy)
  if (tick.size =="15 Min"){
    
    return(chart.15min$duration[[curncy]])
  }else if(tick.size=="Hourly"){
    table.duration = "30D"
    return(chart.hourly[[duration]][[curncy]])
  }
  else if(tick.size=="Daily"){
    print("plot.trenline")
    print(chart.daily[[duration]][[curncy]])

    return(chart.daily[[duration]][[curncy]])
  }
  else if(tick.size=="Weekly"){

    return(chart.weekly[[duration]][[curncy]])
  }
  
}

createTable<-function(tick.size,duration){
  # Return summary table from input in shiny
  # data is generated in gen_chart.r
   if(tick.size=="Hourly"){
    df = table.hourly[[duration]]
  }
  else if(tick.size=="Daily"){
    df = table.daily[[duration]]
   
  }
  else if(tick.size=="Weekly"){
    df = table.weekly[[duration]]
  }
  
  options(scipen = 999)
  formated.df = format(
    df,
    digits = 4,
    nsmall = 4,
    big.mark = ",",
    small.interval = 3
  )
  
  return(formated.df)
}
