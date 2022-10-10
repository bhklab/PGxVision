library(shiny)
library(shinyjs)
library(plotly)
library(magrittr)
library(jsonlite)
library(data.table)
library(promises)
library(future)
library(shinyalert)
library(shinybusy)
library(msigdbr)
library(GSVA)
library(DT)
plan(multisession)

# Import files for each page
source ('uploadPage.R')
source ('analysisPage.R')
source ('drugTreatmentPage.R')
source ('gseaReportPage.R')

# Change options for this session
opts <- options()
options(shiny.maxRequestSize=250*1024^2) # max upload of 250 MB
on.exit(options(opts))

# Navigation function
navigate <- function(newPage, output) {
  output$app <- renderUI({
    switch(newPage,
           home={return(uploadPageUI)},
           analysis={return(analysisPageUI)},
           treatment={return(drugTreatmentPageUI)},
           ssGSEA={return(gseaReportPageUI)},
    )})
}

ui <- fluidPage(
  # Import app level CSS
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css")),
  
  useShinyjs(),
  uiOutput('app'),
  
  # Footer
  br(),
  p("© BHK Lab 2021-2022", align='center')
)

server <- function(input, output, session) {
  
  globalRV <- reactiveValues(
    patientDf=NULL,
    referenceDf=NULL,
    ssGseaResults=NULL,
    ssGseaMetadata=NULL,
  )
  
  # Logic to render different pages
  navigate('home', output)
  
  # Initialize pages
  uploadPageInitialize(input, output, navigate, globalRV)
  analysisPageInitiatize(input, output, navigate, globalRV)
  drugTreatmentPageInitiatize(input, output, navigate, globalRV)
  gseaReportPageInitiatize(input, output, navigate, globalRV)
}

shinyApp(ui, server)

# [END]