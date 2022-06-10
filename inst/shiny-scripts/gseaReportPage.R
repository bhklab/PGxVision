### IMPORT COMPONENTS ###
source('components/container.R')
source('components/gseaRowDetailed.R')

### INITIALIZE VARIABLES ###

# Function to initialize everything in this page
gseaReportPageInitiatize <- function(input, output, navigate, globalRV) {
  gseaReportPageRV <- gseaReportPageCreateRV()
  gseaReportPageObservers(input, gseaReportPageRV, output, navigate, globalRV)
  gseaReportPageOutputUI(input, gseaReportPageRV, output, globalRV)
}

### INPUT ###

# Return all tab input rows 
gseaReportPageUI = container(
  # Set headers
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/analysisPage.css")),
  
  div(class='header',
      div(id='back', 
          class='flex headerButton', 
          img(style='display: inline-block;', src='./icons/back.png', height='16px', width='16px'),
          span(style='color: white; font-weight: bold;', "Back"),
      ),
      h2(style='color: white', "GSEA Report"),
      p(style='color: white;', 'Enrichment score on a scale from 0-1. 1 means that all genes in the set are encriched. 
        0 means no genes in the set are enriched.')
  ),
  br(),br(),
  div(style='display: flex; justify-content: center',
      div(class='card shadow', style='width: 100%', 
          # TODO: Figure out what goes here
          uiOutput('ssGseaTable')
      ),
  ),
)

### REACTIVE VALUES AND OBSERVERS ###

gseaReportPageCreateRV <- function() {
  return( reactiveValues())
}

# Return all reactive variable observers
gseaReportPageObservers <- function(input, rv, output, navigate, globalRV) {
  # Observe home button click
  observe({ onclick("back", {
    globalRV$ssGseaResults <- NULL
    output$ssGseaTable <- renderUI({updateGSEAPlot(input, rv, output, globalRV)})
    navigate('analysis', output)})})
}

### OUTPUT ###

updateGSEAPlot <- function(input, rv, output, globalRV) {
  if (identical(globalRV$ssGseaResults, NULL)) {
    return(p("Loading"))
  }
  
  gseaResultsDf <- globalRV$ssGseaResults 
  
  returnUIRow <- function(row, index) {
    pathway <- rownames(row) 
    estimate <- formatC(as.numeric(row[1]), format='e', digits=4)
    genes <- row[2]
    description <- globalRV$ssGseaMetadata[[pathway]]
    return(gseaRowDetailed(description, index, pathway, estimate, genes))
  }
  
  return(
    div(
      Map(returnUIRow, split(gseaResultsDf, seq_len(nrow(gseaResultsDf))), 1:nrow(gseaResultsDf))
    )
  )
}

gseaReportPageOutputUI <- function(input, rv, output, globalRV) {
  output$ssGseaTable <- renderUI({updateGSEAPlot(input, rv, output, globalRV)})
}