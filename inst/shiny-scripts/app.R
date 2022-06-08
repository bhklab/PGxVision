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
           treatment={return(drugTreatmentPageUI)}) 
  })
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
  # Logic to render different pages
  navigate('home', output)
  
  # Initialize pages
  uploadPageInitialize(input, output, navigate)
  analysisPageInitiatize(input, output, navigate)
  drugTreatmentPageInitiatize(input, output, navigate)
}

shinyApp(ui, server)

# [END]
