library(shiny)
library(shinyjs)
library(shinythemes)

#Sample of sensors to play with.
sensors <- read.csv('sensors.csv')

#Function for UI side of busy indicator widget.
withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    `data-for-btn` = id,
    button,
    span(class = "btn-loading-container",
         hidden(
           img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
           icon("check", class = "btn-done-indicator")
         )),
    hidden(div(class = "btn-err",
               div(
                 icon("exclamation-circle"),
                 tags$b("Error: "),
                 span(class = "btn-err-msg")
               )))
  )
}

shinyUI(
  navbarPage(
    "Wavelet Changepoint Detection",
    inverse = TRUE,
    tabPanel(
      "Data Selection",
      h1("Instructions:"),
      h5("1. Select data series and hit submit."),
      h5("2. Choose desired date range underneath series plot."),
      h5("3. Perform wavelet transform."),
      h5("4. Search for changepoints."),
      hr(),
      sidebarPanel(
        selectInput(
          inputId = 'station',
          label = 'Station',
          choices = unique(sensors$Station)
        ),
        uiOutput('sensor'),
        actionButton(inputId = 'submit.series', label = 'Select'),
        br(),
        br()
      ),
      mainPanel(plotOutput('rawplot'),
                uiOutput('daterange', align = 'center'))
    ),
    tabPanel(
      "Wavelet Transform",
      sidebarPanel(
        selectInput(
          inputId = 'filter',
          label = 'Filter',
          choices = c('haar', 'd4', 'la8')
        ),
        sliderInput(
          inputId = 'levels',
          label = 'Levels',
          min = 1,
          max = 8,
          value = 4,
          step = 1
        ),
        selectInput(
          inputId = 'boundary',
          label = 'Boundary',
          choices = c('periodic', 'reflection')
        ),
        actionButton(inputId = 'submit.filter', label = 'Filter')
      ),
      mainPanel(uiOutput('nlvls'),
                plotOutput('lvlplots1'))
    ),
    tabPanel(
      "Changepoint Detection",
      p(strong("Notes:")),
      p("SegNeigh does not take a minimum sement length argument and is computationally slow."),
      p("PELT does not take maximum changepoint argument."),
      hr(),
      sidebarPanel(
        selectInput(
          inputId = 'cptmethod',
          label = 'Method',
          choices = c('BinSeg', 'SegNeigh', 'PELT')
        ),
        numericInput(
          inputId = 'ncpts',
          label = 'Maximum Number',
          value = 1,
          min = 1
        ),
        numericInput(
          inputId = 'pen',
          label = 'Penalty Value',
          value = 200,
          min = 0
        ),
        numericInput(
          inputId = 'minseg',
          label = 'Minimum Segment Length',
          value = 50,
          min = 2,
          step = 1
        ),
        actionButton(inputId = 'submit.cpts', label = 'Find'),
        br(),
        h5('Location(s)'),
        tableOutput('locs')
      ),
      mainPanel(tabsetPanel(
        tabPanel("Original Series",
                 plotOutput('cptseries')),
        tabPanel("Wavelet Details",
                 plotOutput('lvlcpts'))
      ))
    )
  )
)
