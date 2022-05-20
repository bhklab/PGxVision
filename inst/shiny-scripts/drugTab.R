### INITIALIZE VARIABLES ###

### INPUT ###

# Helper functions
drugBiomarkerFileUploadBox <- box(
  width=12,
  column(
    width=6,
    fileInput("patientFileDrugTab", "Upload Patient Molecular Profile CSV:",
              accept=c("text/csv", ".csv"), buttonLabel="Browse files"
    )
  ),
  column(
    width=6,
    fileInput("referencePopulationFileDrugTab",
              "Upload Reference Population CSV:",
              accept=c("text/csv", ".csv"), buttonLabel="Browse files"
    )
  )
)

pharmacodbBiomarkersTable <- box(
  width=12,
  column(width=8, align="center", dataTableOutput("pdbBiomarkers"))
)

instructionsMessage <- box(
  width=13, 
  textOutput('instructions'),
  actionButton("funButton", "Show Message"),
)

# Return all tab input rows 
drugTabInputUI = tabItem(
  tabName= "druganalysis",
  h2("Drug Analysis"),
  fluidRow(drugBiomarkerFileUploadBox),
  #add_busy_spinner(spin = "fading-circle"),
  instructionsMessage,
  fluidRow(pharmacodbBiomarkersTable),
  
  
)

### REACTIVE VALUES AND OBSERVERS ###

# object containing all relevant reactive values for the clinical tab
drugTabCreateRV <- function() {
  return( reactiveValues(patientDf = NULL,
                         referenceDf = NULL,
                         pdbBiomarkersDf=tryCatch(
                           PGxVision::fetchPharmacoDbBiomarkers(),
                           error=function(e) fread(file.path(
                             system.file("extdata", package="PGxVision"),
                             "sample_data",
                             "pharmacodb_biomarker_df.csv"
                           )))))
}

# Return all reactive variable observers
drugTabObservers <- function(input, rv) {
  observeEvent(input$patientFileDrugTab, {
    req(input$patientFileDrugTab)
    df_ <- data.table::fread(input$patientFileDrugTab$datapath)
    # convert data table to data frame with genes as row names
    rv$patientDf <- data.frame(df_[, -1], row.names=df_[[1]]) 
  })
  
  observeEvent(input$referencePopulationFileDrugTab, {
    req(input$referencePopulationFileDrugTab)
    df_ <- data.table::fread(input$referencePopulationFileDrugTab$datapath)
    rv$referenceDf <- data.frame(df_[, -1], row.names=df_[[1]])
  })
  
  observeEvent(input$funButton, {
    shinyalert("Title of Modal", "Body of modal", type= "error")
  })
}

### OUTPUT ###

#Return all output objects
drugTabOutputUI <- function (input, rv, output) {
  output$instructions <- renderText({
    if(identical(rv$patientDf, NULL)) { return("Please upload your patient's gene expression file.")}
    else if (identical(rv$referenceDf, NULL)) { return("Please upload your reference population gene expression file.")}
    else {
      return("View your results below")
      #show_spinner()
    }
  })
  
  output$pdbBiomarkers <- renderDataTable({
    pdf <- rv$patientDf
    rdf <- rv$referenceDf
    
    # Return empty table if files not uploaded 
    if (identical(pdf, NULL) | identical(rdf, NULL)) {
      return(data.frame(Biomarkers=character()))
    }
    
    biodf <- data.frame(rv$pdbBiomarkersDf)
     future_promise( {
      df_ <- PGxVision::retrieveDiffExpressedGenes(pdf, rdf, biodf) 
      names(df_)[names(df_) == 'estimate'] <- 'correlation'
      names(df_)[names(df_) == 'percentile'] <- 'sample expression percentile'
      df_[order(df_$pvalue, -abs(df_$correlation)), c('compound_name', 'correlation', 'pvalue', 'gene_symbol', 
                                                      'sample expression percentile', 'inchikey', 'pubchem', 'chembl_id', 'tissue')]
    
      }) 
  })
}