library(shiny)
library(shinyjs)
library(waveslim)
library(changepoint)
library(ggplot2)
library(RMySQL)
library(tidyr)

#Sample of sensors to play with.
sensors <- read.csv('sensors.csv')

#Function used to get query from Amazon RDS and close connection.
sqlQuery <- function(query) {
  con <-
    dbConnect(
      MySQL(),
      user = 'thepufferfish',
      password = 'usingthis0nce',
      host = 'caswp-db.cyxyjqnr1bi5.us-west-1.rds.amazonaws.com',
      port = 3306,
      dbname = 'RTDF'
    )
  res <- dbGetQuery(con, query)
  
  dbDisconnect(con)
  return(res)
}

#Function for busy indicator widget
withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <-
    sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <-
    sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000,
                   shinyjs::hide(
                     selector = doneEl,
                     anim = TRUE,
                     animType = "fade",
                     time = 0.5
                   ))
    value
  }, error = function(err) {
    errorFunc(err, buttonId)
  })
}

function(input, output, session) {
  #Data Series Selection
  output$sensor <- renderUI({
    selectInput(inputId = 'sensor',
                label = 'Sensor',
                choice = sensors[sensors$Station == input$station, 'Sensor'])
  })
  series <- eventReactive(input$submit.series, {
    query <- paste(
      'SELECT ObsTime, RawValue FROM tblTimeSeries WHERE StaSensorID = ',
      subset(sensors, Station == input$station &
               Sensor == input$sensor)$StaSensorID,
      sep = ''
    )
    series <- sqlQuery(query)
    data.frame(ObsTime = as.POSIXct(series$ObsTime, tz = 'UTC'),
               RawValue = series$RawValue)
  })
  output$daterange <- renderUI({
    input$submit.series
    min <- series()$ObsTime[1]
    max <- series()$ObsTime[nrow(series())]
    dateRangeInput(
      inputId = 'dates',
      label = NULL,
      start = min,
      end = as.Date(max),
      min = min,
      max = max
    )
  })
  ts <- reactive({
    min2 <- as.POSIXct.Date(input$dates[1])
    max2 <- as.POSIXct.Date(input$dates[2])
    ts <-
      na.omit(subset(series(), ObsTime >= min2 & ObsTime <= max2))
  })
  output$rawplot <- renderPlot({
    input$submit.series
    input$dates
    ggplot(data = ts(), aes(y = RawValue, x = ObsTime)) + geom_line(color = '#AA4499') + theme_bw() + labs(
      x = 'Date/Time',
      y = subset(sensors, Station == input$station &
                   Sensor == input$sensor)$Units
    )
  })
  
  #Wavelet Transform
  ts.wt <- eventReactive(input$submit.filter, {
    modwt(
      na.omit(ts()$RawValue),
      wf = input$filter,
      n.levels = input$levels,
      boundary = tolower(input$boundary)
    )
  })
  output$nlvls <- renderUI({
    numericInput(
      inputId = 'nlvls',
      label = 'Number of levels to display: ',
      min = 1,
      value = 1,
      max = input$levels,
      step = 1,
      width = '190px'
    )
  })
  output$lvlplots1 <- renderPlot({
    input$submit.filter
    ts.wt.df <-
      data.frame(ts.wt()[1:input$nlvls], ObsTime = na.omit(ts())$ObsTime)
    ts.wt.df <- ts.wt.df[-c(1, nrow(ts.wt.df)),]
    ggplot(data = gather(ts.wt.df, Level, value, -ObsTime),
           aes(y = value, x = ObsTime)) + geom_line(color = '#AA4499') +
      facet_grid(Level ~ ., scales = 'free_y') + theme_bw() +
      labs(x = "Date/Time", y = '')
  })
  output$lvlplots2 <- renderUI({
    input$submit.filter
    plotheight <- input$nlvls * 250
    plotOutput("lvlplots1", height = plotheight)
  })
  
  #Changepoint Detection
  cpt <- eventReactive(input$submit.cpts, {
    if (input$cptmethod == 'SegNeigh'){
      cpts(
      cpt.var(
        ts.wt()$d1,
        method = input$cptmethod,
        Q = input$ncpts,
        penalty = 'Manual',
        pen.value = input$pen
      )
    )
      } else{
      if (input$cptmethod == 'PELT'){
        cpts(
          cpt.var(
            ts.wt()$d1,
            method = input$cptmethod,
            minseglen = input$minseg,
            penalty = 'Manual',
            pen.value = input$pen
          )
        )
      } else {
        cpts(
          cpt.var(
            ts.wt()$d1,
            method = input$cptmethod,
            Q = input$ncpts,
            penalty = 'Manual',
            pen.value = input$pen
          )
        )
      }
      
    }
  })
  output$locs <- renderTable({
    input$submit.cpts
    as.character(ts()$ObsTime[cpt()])
  })
  output$cptseries <- renderPlot({
    input$submit.cpts
    ggplot(data = ts(), aes(y = RawValue, x = ObsTime)) + geom_line(color = '#AA4499') + theme_bw() + labs(
      x = 'Date/Time',
      y = subset(sensors, Station == input$station &
                   Sensor == input$sensor)$Units
    ) + geom_vline(xintercept = as.numeric(ts()$ObsTime[cpt()]))
  })
  output$lvlcpts <- renderPlot({
    input$submit.cpts
    ts.wt.df <-
      data.frame(ts.wt()[1:input$nlvls], ObsTime = na.omit(ts())$ObsTime)
    ts.wt.df <- ts.wt.df[-c(1, nrow(ts.wt.df)),]
    ggplot(data = gather(ts.wt.df, Level, value, -ObsTime),
           aes(y = value, x = ObsTime)) + geom_line(color = '#AA4499') +
      facet_grid(Level ~ ., scales = 'free_y') + theme_bw() +
      labs(x = "Date/Time", y = '') + geom_vline(xintercept = as.numeric(ts()$ObsTime[cpt()]))
  })
}
