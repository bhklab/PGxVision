### IMPORT COMPONENTS ###
source('components/container.R')

### INITIALIZE VARIABLES ###

sampleDataDir <- "extdata/sample_data/"
pdxFile <- system.file(paste0(sampleDataDir, "sampleTreatmentResponse.csv"),
                       package="PGxVision")
brcaPdxePaxlitaxelResponse <- read.csv(pdxFile)

# Function to initialize everything in this page
drugTreatmentPageInitiatize <- function(input, output, navigate) {
  drugTreatmentPageRV <- drugTreatmentPageCreateRV()
  drugTreatmentPageObservers(input, drugTreatmentPageRV, output, navigate)
  drugTreatmentPageOutputUI(input, drugTreatmentPageRV, output)
}

### INPUT ###

# Return all tab input rows 
drugTreatmentPageUI = container(
  # Set headers
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/analysisPage.css")),
  
  div(class='header',
      div(id='home', 
          class='flex', 
          img(style='display: inline-block;', src='./icons/home.png', height='16px', width='16px'),
          span(style='color: white; font-weight: bold;', "Home"),
      ),
      h2(style='color: white', "Drug Response")
  ),
  br(),br(),
  div(style='display: flex; justify-content: center',
      div(class='card shadow', style='width: 100%', 
          fileInput("drugSensFile", "Upload Drug Sensitivity CSV:",
                    accept = c("text/csv", ".csv"), buttonLabel="Browse files"),
          div(class='flex', style='justify-content: space-between;',
              uiOutput("wfXSelect"),
              uiOutput("wfYSelect"),
              uiOutput("wfColSelect"),
          ),
          br(),
          plotOutput("waterfallPlot"),
          uiOutput("wfLabels"),
      ),
  ),
)

### REACTIVE VALUES AND OBSERVERS ###

drugTreatmentPageCreateRV <- function() {
  return( reactiveValues(sensitivityDf = brcaPdxePaxlitaxelResponse))
}

# Return all reactive variable observers
drugTreatmentPageObservers <- function(input, rv, output, navigate) {
  # Observe home button click
  observe({
    onclick("home", navigate('home', output))
  })
  
  # Update waterfallDf based on file upload
  observeEvent(input$drugSensFile, {
    req(input$drugSensFile)
    rv$sensitivityDf <- read.csv(input$drugSensFile$datapath)
  })
}

### OUTPUT ###

drugTreatmentPageOutputUI <- function(input, rv, output) {
  # Create waterfall plot dropdowns based on file upload
  output$wfXSelect <- renderUI({
    columns <- colnames(rv$sensitivityDf)
    columnClasses <- sapply(rv$sensitivityDf, class)
    discreteColumns <- columns[columnClasses == "character"]
    selectInput("wfX", "x-Axis", discreteColumns, selectize = F, width='230px')
  })
  
  output$wfYSelect <- renderUI({
    columns <- colnames(rv$sensitivityDf)
    columnClasses <- sapply(rv$sensitivityDf, class)
    numericColumns <- columns[columnClasses == "numeric"]
    selectInput("wfY", "y-Axis", numericColumns, selectize = F, width='230px')
  })
  
  output$wfColSelect <- renderUI({
    columns <- colnames(rv$sensitivityDf)
    columnClasses <- sapply(rv$sensitivityDf, class)
    numericColumns <- columns[columnClasses == "numeric"]
    selectInput("wfCol", "Color", numericColumns, selectize = F, width='230px')
  })
  
  # Create text input elements for custom labeling
  output$wfLabels <- renderUI({
    div(
      h5('Plot Labels'),
      div(class='flex', style='justify-content: space-between;',
          textInput("wfTitle", "Title", value = "", width='230px'),
          textInput("wfXLabel", "x-Axis Label", value = input$wfX, width='230px'),
          textInput("wfYLabel", "y-Axis Label", value = input$wfY, width='230px'),
      ),
    )
  })
  
  # Update plots based on selected columns
  output$waterfallPlot <- renderPlot({
    if (typeof(input$wfX) == "character" &&
        typeof(input$wfY) == "character" &&
        typeof(input$wfCol) == "character") {
      PGxVision::buildWaterfallPlot(
        rv$sensitivityDf, xAxisCol=input$wfX, drugSensitivityCol=input$wfY,
        colorCol=input$wfCol, xLabel=input$wfXLabel, yLabel=input$wfYLabel,
        title=input$wfTitle)
    }
  })
}