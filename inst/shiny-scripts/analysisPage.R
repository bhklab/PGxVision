### IMPORT COMPONENTS ###
source('components/tip.R')
source('components/characteristic.R')
source('components/container.R')

### IMPORT PAGES AND UI ###
source('univariateTab.R')
source('multivariateTab.R')
source('biomarkerTab.R')

# Function to initialize everything in this page
analysisPageInitiatize <- function(input, output, navigate) {
  analysisPageRV <- analysisPageCreateRV()
  analysisPageObservers(input, analysisPageRV, output, navigate)
  analysisPageOutputUI(input, analysisPageRV, output)
  
  # Initialize sub-tabs
  univariateTabInitialize(input, output)
  multivariateTabInitialize(input, output)
  biomarkerTabInitialize(input, output)
}

### INPUT ###

# Return page UI
analysisPageUI <- container(
  # Set headers
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/analysisPage.css")),
  
  # Header
  div(class='flex headerTop',
      div(style='width: 40%; align-self: start', 
          div(id='home', 
              class='flex', 
              img(style='display: inline-block;', src='./icons/home.png', height='16px', width='16px'),
              span(style='color: white; font-weight: bold;', "Home"),
          ),
          uiOutput('sampleName'),
          br(),
      ),
      div(style='width: 60%; border-left: 2px solid rgba(255,255,255,0.5);',
          uiOutput('ssgsea'),
      )
  ),
  div(class='headerBottom shadow', 
      uiOutput('sampleCharacteristics'),
      uiOutput('')),
  
  br(),
  
  # Drug Analysis
  h5("Drug Analysis", align='left'),
  div(class='card shadow', style='width: 100%',
      tabsetPanel(type='tabs',
                  tabPanel('Univariate', univariateTabUI),
                  tabPanel('Multivariate', multivariateTabUI))),
  
  br(), br(), br(), 
  
  # Biomarker Analysis
  h5("Biomarker Analysis", align='left'),
  div(class='card shadow', style='width: 100%', 
      biomarkerTabUI,
  )
)

### REACTIVE VALUES AND OBSERVERS ###

analysisPageObservers <- function(input, rv, output, navigate) {
  observe({
    onclick("home", navigate('home', output))
  })
}

analysisPageCreateRV <- function() { return(reactiveValues()) }

### OUTPUT ###

#Return all output objects
analysisPageOutputUI <- function (input, rv, output) {
  output$sampleName <- renderUI({
    return(div(
      h5(align='center', style='color: white; margin-bottom: 0px', 'SAMPLE'),
      div(class='flex', style='margin-right: 16px',
          h2(align='center', class='sampleName', style='overflow-wrap: break-word;',
             input$patientDf$name))
    ))
  })
  
  output$ssgsea <- renderUI({
    df <- data.frame(results)
    topTen <- df[order(abs(df[,1])), , drop=F][1:10, , drop=F]
    
    returnUIRow <- function(row, index) {
      pathway <- rownames(row) 
      estimate <- formatC(as.numeric(row), format='e', digits=4)
      return(div( style='margin-bottom: 4px;',
        span(style='color: #0c2461; border-right: 2px solid #0c2461;
             font-weight: bold; padding-right: 3px', as.character(index)),
        span(pathway, style='overflow-wrap: break-word; color: #0c2461'),
        span(' '),
        span(style='color: white; background-color: #0c2461; margin-left: 8px;
             padding: 1px 6px; border-radius: 8px; font-weight: bold;',estimate)
      ))
    }
    
    return(
      div(
        h5(align='left', style='color: white; margin-left: 12px', 'ENRICHED GENE SETS'),
        div(style='margin: 8px 12px; padding: 8px; background-color: white; 
          border-radius: 8px; color: white;',
            Map(returnUIRow, split(topTen, seq_len(nrow(topTen))), 1:10),
            p(style='text-decoration: underline; color: #0c2461; font-weight: bold; 
      border-radius: 8px; padding: 3px 8px;', align='right', "Learn more")
        )
      )
    )
  })
  
  output$sampleCharacteristics <- renderUI({
    return(
      div(class='flex',
          createCharacteristic("47", "Random Interesting Fact", '#0c2461', '#0c2461'),
          createCharacteristic("23", "Random Crazy Fact", '#0c2461', '#0c2461'),
          createCharacteristic("536", "Really Insane Fact", '#0c2461', '#0c2461'),
      ))
  })
}