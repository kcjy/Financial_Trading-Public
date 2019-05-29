
library(shiny)
library(htmltools)
library(DT)
library(dplyr)
library(rsconnect)

# Manual: 1. run gen_chart.r to create data
#       2. run runApp(host = "host ip address", port=5033) port can be any four digits
#       3. save server.r before executing runApp (Do not include runApp line in this file)

source("gen_chart.r")
#runApp(host="192.168.2.220", port=1234)

shinyServer <- function(input, output,session) {
  duration.choices <- reactive({
    if (input$chart.ticksize=="15 Min"){
      return(chart.tick.duration)
    }else if (input$chart.ticksize=="Hourly"){
      return(chart.hourly.duration)
    }
    else if (input$chart.ticksize=="Daily"){
      return(chart.daily.duration)
    }
    else {return(chart.weekly.duration)}
  })
  observe({
    updateSelectInput(session, "chart.duration",choices = duration.choices())
    })
  
  duration.choices.table <- reactive({
    if (input$table.ticksize=="Hourly"){
      return(chart.hourly.duration)
    }
    else if (input$table.ticksize=="Daily"){
      return(chart.daily.duration)
    }
    else {return(chart.weekly.duration)}
  })
  observe({
    updateSelectInput(session, "table.duration",choices = duration.choices.table())
  })
  
  output$chart <- renderPlotly({
   plot.trendline(input$chart.currency, input$chart.ticksize, input$chart.duration)
  })
  
  
  output$table <- DT::renderDataTable({
  createTable(input$table.ticksize, input$table.duration)
    }, options=list(pageLength=100 ))
    
  
  
}

