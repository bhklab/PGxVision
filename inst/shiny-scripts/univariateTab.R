### IMPORT COMPONENTS ###
source('components/tip.R')

# Function to initialize everything in this page
univariateTabInitialize <- function(input, output, globalRV) {
  univariateTabRV <- univariateTabCreateRV()
  univariateTabObservers(input, univariateTabRV)
  univariateTabOutputUI(input, univariateTabRV, output, globalRV)
}


### INPUT ###

# Return page UI
univariateTabUI <- div(
  br(),
  div(class='flex', style='align-items: stretch',
      div(style='width: 35%', 
        p("Please upload your reference population file"),
        fileInput("referencePopulationDf",
                  "",
                  accept=c("text/csv", ".csv"), buttonLabel="Browse files"),
        createTip("Tip", "File format must be .csv"),
      ),
      div(style='width: 65%; border-left: 1px solid gray; padding: 8px;', 
        uiOutput('selectedRow'),
      )
  ),
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
      ))), 
    univarResultsDf=NULL,
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
univariateTabOutputUI <- function (input, rv, output, globalRV) {
  output$selectedRow <- renderUI({
    if (identical(rv$univarResultsDf, NULL)) { return(div()) }
    else {
      index <- input$pdbBiomarkers_rows_selected
      if (identical(index, NULL)) { return(p("Select a row to learn more."))}
      
      # Find reevant pathways 
      gene <- rv$univarResultsDf[index,]$gene_symbol
      relevantPathways <- globalRV$ssGseaResults[grepl(pattern=gene, globalRV$ssGseaResults$genes), ]
      
      return(
        div(
          
          p(index),
          p(gene),
          p(globalRV$geneSetCategory),
          lapply(rownames(relevantPathways), \(x) { return (p(x))})
        ))
    }})
  
  output$pdbBiomarkers <- DT::renderDataTable({
    pdf <- rv$patientDf
    rdf <- rv$referenceDf
    biodf <- data.frame(rv$pdbBiomarkersDf)
    
    # Return empty table if files not uploaded 
    if (identical(pdf, NULL) | identical(rdf, NULL)) {
      return(data.frame(Biomarkers=character()))
    }
    
    df_ <- PGxVision::retrieveDiffExpressedGenes(pdf, rdf, biodf)
    
    # Add emojis to show direction of correlation
    appendSmileys <- function(x) {
      if (x$estimate > 0) { return ("✅ Sensitive")} 
      else if (x$estimate < 0) {return ("❌ Resistant")}
      else {return(" ")}}
    correlation_direction <- apply(df_, MARGIN = 1, appendSmileys)
    df_ <- cbind(df_, correlation_direction)
    
    # Make the numbers round to 4 decimals
    df_$estimate <- round(as.numeric(df_$estimate), 4)
    df_$pvalue <- sprintf("%.4e", as.numeric(df_$pvalue)) 
    df_$percentile <- round(as.numeric(df_$percentile), 4)
    
    # Order the dataframe and add more human column names
    df_ <- df_[order(df_$pvalue, -abs(df_$estimate)), c('compound_name', 'gene_symbol', 'estimate', 'correlation_direction', 
                                                        'pvalue', 'percentile', 'pubchem', 'tissue')]
    rv$univarResultsDf <- df_
    colnames(df_) <- c('Compound Name', 'Gene', 'Correlation', 'Correlation Direction', 'P-value', 
                       'Sample Expression Percentile', 'PubChem ID', 'Tissue Type')
    return (df_)
    
  }, selection='single')
}