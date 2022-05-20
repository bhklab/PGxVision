### IMPORT COMPONENTS ###
source('components/tip.R')
source('components/characteristic.R')

source('univariateTab.R')
source('multivariateTab.R')

# Function to initialize everything in this page
analysisPageInitiatize <- function(input, output) {
  analysisPageRV <- analysisPageCreateRV()
  analysisPageObservers(input, analysisPageRV)
  analysisPageOutputUI(input, analysisPageRV, output)
  
  # Initialize sub-tabs
  univariateTabInitialize(input, output)
  multivariateTabInitialize(input, output)
}

### INPUT ###

# Return page UI
analysisPageUI <- fluidPage(
  div(style='display: flex; justify-content: center',
      div(style='width: 1000px',
          div(style="background-color: #0c2461; width: 100%; padding: 32px 16px;
              margin-top: 16px; border-radius: 8px 8px 0px 0px",
              uiOutput('sampleName'),
              br(),
          ),
          div(class='shadow', style='background-color: #1e3799; border-radius: 0px 0px 8px 8px;
              padding: 8px',
              uiOutput('sampleCharacteristics'),
          ),
          
          br(),
          h5("Drug Analysis", align='left'),
          
          div(class='card shadow', style='width: 100%',
              tabsetPanel(type='tabs',
                          tabPanel('Univariate', univariateTabUI),
                          tabPanel('Multivariate', multivariateTabUI)))))
)

### REACTIVE VALUES AND OBSERVERS ###

analysisPageObservers <- function(input, rv) {
  
}

analysisPageCreateRV <- function() {
  return( reactiveValues() )
}

### OUTPUT ###

#Return all output objects
analysisPageOutputUI <- function (input, rv, output) {
  output$sampleName <- renderUI({
    return(div(
      h5(align='center', style='color: white', 'SAMPLE'),
      h2(align='center', style='color: white; font-weight: 900', input$patientDf$name))
    )
  })
  
  output$sampleCharacteristics <- renderUI({
    return(div(class='flex',
               createCharacteristic("47", "Random Interesting Fact"),
               createCharacteristic("23", "Random Crazy Fact"),
               createCharacteristic("536", "Really Insane Fact"),
    ))
  })
}