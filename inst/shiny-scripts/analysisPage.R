### IMPORT COMPONENTS ###
source('components/tip.R')
source('components/characteristic.R')
source('components/container.R')
source('components/gseaRow.R')

### IMPORT PAGES AND UI ###
source('univariateTab.R')
source('multivariateTab.R')
source('biomarkerTab.R')

### INITIALIZE VARIABLE ###

# Function to initialize everything in this page
analysisPageInitiatize <- function(input, output, navigate, globalRV) {
  analysisPageRV <- analysisPageCreateRV()
  analysisPageObservers(input, analysisPageRV, output, navigate, globalRV)
  analysisPageOutputUI(input, analysisPageRV, output, navigate, globalRV)
  
  # Initialize sub-tabs
  univariateTabInitialize(input, output, globalRV)
  multivariateTabInitialize(input, output)
  biomarkerTabInitialize(input, output)
}

geneSetsDir <- "extdata/gsets/"

geneSetCategories <- c(
  'H Hallmark gene sets',
  'C1 Positional gene sets',
  'C2 Curated gene sets',
  'C3 Regulatory target gene sets',
  'C4 Computational gene sets',
  'C5 Ontology gene sets',
  'C6 Oncogenic signature gene sets',
  'C7 Immunologic signature gene sets',
  'C8 Cell type signature gene sets'
)

### INPUT ###

# Return page UI
analysisPageUI <- container(
  # Set headers
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/analysisPage.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css")),
  
  # Header
  div(class='header',
      div(id='home', 
          class='flex headerButton', 
          img(style='display: inline-block;', src='./icons/home.png', height='16px', width='16px'),
          span(style='color: white; font-weight: bold;', "Home"),
      ),
      uiOutput('sampleName'),
      br(), 
      div(class='geneSetHeader', style='display:flex',
          h5(align='left', style='color: white; display: inline-block;', 'TOP 5 ENRICHED GENE SETS FOR UPLOADED SAMPLE'),
          selectInput('geneSetCategory', label=NULL, geneSetCategories),
      ),
      uiOutput('ssgsea'),
  ),
  
  br(),
  
  # Drug Analysis
  h5("Drug Analysis", align='left'),
  div(class='card shadow', style='width: 100%',
      tabsetPanel(type='tabs',
                  tabPanel('Univariate', univariateTabUI),
                  tabPanel('Multivariate', multivariateTabUI))),
  
  br(), br(),  
  
  # Biomarker Analysis
  h5("Biomarker Analysis", align='left'),
  div(class='card shadow', style='width: 100%', 
      biomarkerTabUI,
  )
)

### REACTIVE VALUES AND OBSERVERS ###

analysisPageObservers <- function(input, rv, output, navigate, globalRV) {
  observe({ onclick("home", {
    globalRV$referenceDf <- NULL
    navigate('home', output)
  })
  })
  observe({ onclick("learnMore", navigate('ssGSEA', output)) })
  
  observeEvent(input$geneSetCategory, {
    rv$geneSetCategory <- input$geneSetCategory             
  })
}

analysisPageCreateRV <- function() { return(reactiveValues(
  geneSetCategory=geneSetCategories[7],
)) }

### OUTPUT ###

#Return all output objects
analysisPageOutputUI <- function (input, rv, output, navigate, globalRV) {
  output$sampleName <- renderUI({
    return(div(
      h5(align='center', style='color: white; margin-bottom: 0px', 'SAMPLE'),
      div(class='flex', style='margin-right: 16px',
          h2(align='center', class='sampleName', style='overflow-wrap: break-word;',
             input$patientDf$name))
    ))
  })
  
  output$ssgsea <- renderUI({
    geneSetJSONFile <- system.file(paste0(geneSetsDir, rv$geneSetCategory, '.json'), package="PGxVision")
    gseaResults <- PGxVision::performSSGSEA(globalRV$patientDf, geneSetJSONFile)
    View(gseaResults)
    print(typeof(gseaResults))
    gseaResultsDf <- data.frame(gseaResults$results)
    gseaResultsDf <- gseaResultsDf[order(abs(as.numeric(gseaResultsDf[,1])), decreasing=T), , drop=F]
    globalRV$ssGseaMetadata <- gseaResults$descriptions
    globalRV$ssGseaResults <- gseaResultsDf[gseaResultsDf[,1] > 0.4,]
    topFive <- gseaResultsDf[1:5, , drop=F]
    
    returnUIRow <- function(row, index) {
      pathway <- rownames(row) 
      estimate <- formatC(as.numeric(row[1]), format='e', digits=4)
      genes <- row[2]
      title <- gseaResults[['descriptions']][[pathway]]
      return(gseaRow(title, index, pathway, estimate, genes))
    }
    
    return(
      div(class='gseaPane',
          Map(returnUIRow, split(topFive, seq_len(nrow(topFive))), 1:5),
          p(id='learnMore', align='right', "Learn more")
      )
    )
  })
}