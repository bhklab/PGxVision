### IMPORT COMPONENTS ###
source('components/tip.R')

# Function to initialize everything in this page
univariateTabInitialize <- function(input, output) {
  univariateTabRV <- univariateTabCreateRV()
  univariateTabObservers(input, univariateTabRV)
  univariateTabOutputUI(input, univariateTabRV, output)
}


### INPUT ###

# Return page UI
univariateTabUI <- div(
  br(),
  p("Please upload your reference population file"),
  fileInput("referencePopulationDf",
            "",
            accept=c("text/csv", ".csv"), buttonLabel="Browse files"),
  createTip("Tip", "File format must be .csv"),
  br(),
  dataTableOutput("pdbBiomarkers"),
)


univariateTabCreateRV <- function() {
  return( reactiveValues(
    patientDf=NULL,
    referenceDf=NULL,
    pdbBiomarkersDf=tryCatch(
      PGxVision::fetchPharmacoDbBiomarkers(),
      error=function(e) fread(file.path(
        system.file("extdata", package="PGxVision"),
        "sample_data",
        "pharmacodb_biomarker_df.csv"
      )))
  ))
}


# Return all reactive variable observers
univariateTabObservers <- function(input, rv) {
  observeEvent(input$referencePopulationDf, {
    req(input$referencePopulationDf)
    df1 <- data.table::fread(input$referencePopulationDf$datapath)
    rv$referenceDf <- data.frame(df1[, -1], row.names=df1[[1]])
    df2 <- data.table::fread(input$patientDf$datapath)
    rv$patientDf <- data.frame(df2[, -1], row.names=df2[[1]])
  })
}

#Return all output objects
univariateTabOutputUI <- function (input, rv, output) {
  output$pdbBiomarkers <- renderDataTable({
    pdf <- rv$patientDf
    rdf <- rv$referenceDf
    
    # Return empty table if files not uploaded 
    if (identical(pdf, NULL) | identical(rdf, NULL)) {
      return(data.frame(Biomarkers=character()))
    }
    
    biodf <- data.frame(rv$pdbBiomarkersDf)
    future_promise({
      df_ <- PGxVision::retrieveDiffExpressedGenes(pdf, rdf, biodf) 
      names(df_)[names(df_) == 'estimate'] <- 'correlation'
      names(df_)[names(df_) == 'percentile'] <- 'sample expression percentile'
      df_[order(df_$pvalue, -abs(df_$correlation)), c('compound_name', 'correlation', 'pvalue', 'gene_symbol', 
                                                      'sample expression percentile', 'pubchem', 'tissue')]
    }) 
  })
}