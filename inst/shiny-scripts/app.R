library(shiny)
library(plotly)
library(magrittr)
library(jsonlite)
library(data.table)
library(promises)
library(future)
library(shinyalert)
library(shinybusy)
plan(multisession)

# Import files for each page
source ('uploadPage.R')
source ('analysisPage.R')

# Change options for this session
opts <- options()
options(shiny.maxRequestSize=250*1024^2) # max upload of 250 MB
on.exit(options(opts))

ui <- fluidPage(
  conditionalPanel(condition='output.patientDfUploaded == false', 
                   uploadPageUI(uploadedFileId="patientDf")),
  conditionalPanel(condition='output.patientDfUploaded == true', analysisPageUI),
  
  # Bottom text
  br(),
  p("© BHK Lab 2021-2022", align='center')
)

server <- function(input, output) {
  
  # Logic to render different pages
  output$patientDfUploaded <- reactive({FALSE})
  outputOptions(output, 'patientDfUploaded', suspendWhenHidden=FALSE)
  
  observeEvent(input$patientDf, {
    req(input$patientDf)
    output$patientDfUploaded <- reactive({TRUE})
  })
  
  # Initialize pages
  analysisPageInitiatize(input, output)
}

shinyApp(ui, server)

# [END]
