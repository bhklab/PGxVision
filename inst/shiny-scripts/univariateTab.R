### IMPORT COMPONENTS ###
source('components/tip.R')

# Function to initialize everything in this page
univariateTabInitialize <- function(input, output, globalRV) {
  univariateTabRV <- univariateTabCreateRV()
  univariateTabObservers(input, univariateTabRV, globalRV)
  univariateTabOutputUI(input, univariateTabRV, output, globalRV)
}


### INPUT ###

# Return page UI
univariateTabUI <- div(
  br(),
  div(class='flex', style='align-items: stretch',
      div(style='width: 33%',
          # File input for reference population matrix (.csv)
          fileInput("referencePopulationDf",
                    "",
                    accept=c("text/csv", ".csv"), buttonLabel="Browse files"),
          createTip("Tip", "File format must be .csv"),
      ),
      # This is the info pane for the biomarkers displayed in this tab
      div(style='width: 67%; background-color: #ecf0f1; border-radius: 8px; padding: 8px;',
          uiOutput('selectedRow'),
      )
  ),
  br(),

  # Data table containing biomarkers 
  dataTableOutput("pdbBiomarkers"),
)


univariateTabCreateRV <- function() {
  return( reactiveValues(
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
univariateTabObservers <- function(input, rv, globalRV) {
  observeEvent(input$referencePopulationDf, {
    req(input$referencePopulationDf)
    df1 <- data.table::fread(input$referencePopulationDf$datapath)
    globalRV$referenceDf <- data.frame(df1[, -1], row.names=df1[[1]])
  })
}

#Return all output objects
univariateTabOutputUI <- function (input, rv, output, globalRV) {
  output$selectedRow <- renderUI({
    # Show a message if nothing is uploaded
    if (identical(rv$univarResultsDf, NULL)) { return(p("Please upload your reference population file.")) }
    
    # Otherwise, find the results
    else {
      index <- input$pdbBiomarkers_rows_selected
      
      # No row selected
      if (identical(index, NULL)) { return(p("Select a row to learn more."))}
      # Find relevant pathways in the ssGSEA results to display
      gene <- rv$univarResultsDf[index,]$gene_symbol
      relevantPathways <- globalRV$ssGseaResults[grepl(pattern=gene, globalRV$ssGseaResults$genes), ]
      
      return(
        div(style='display: flex;',
            div(style='width: 38%; overflow: wrap;padding-right: 4px; ',
              div(span(style='font-weight: bold', "Gene"), span(gene)),
              div(span(style='font-weight: bold', "Gene Set Category"), span(globalRV$geneSetCategory)),
              div(span(style='font-weight: bold', 'Enriched Pathways')),
              div(style='display: flex; flex-wrap: wrap; overflow: wrap; ',
                  lapply(rownames(relevantPathways), \(x) {
                    return (
                      div(style='background-color: white; padding: 1px 8px; width: 100%; overflow: wrap;
                      border-radius: 15px; margin: 2px 0px 2px 0px; display: inline-block; 
                          word-wrap: break-word;',
                          span(style='word-wrap: break-word;',x)
                      ))})
              ),
            ),
            div(style='width: 60%; height: 200px; border-radius: 8px; overflow: hidden;
                background-color: white;',
                renderPlot(PGxVision::densityPlotBiomarkerPercentile(
                  gene, # the gene of interest (string)
                  globalRV$patientDf[gene, ], # the patient's expression for the gene chosen (float)
                  globalRV$referenceDf), height=200)
            )
        ))
    }})

  # Display the results table from the univariate analysis
  output$pdbBiomarkers <- DT::renderDataTable({
    pdf <- globalRV$patientDf
    rdf <- globalRV$referenceDf
    biodf <- data.frame(rv$pdbBiomarkersDf)
    # Return empty table if files not uploaded
    if (identical(pdf, NULL) | identical(rdf, NULL)) {
      return(data.frame(Biomarkers=character()))
    }

    df_ <- PGxVision::retrieveDiffExpressedGenes(pdf, rdf, biodf)

    # Add emojis to show direction of correlation
    appendSmileys <- function(x) {
      positive_corr <- x$estimate > 0
      percentile_high <- x$percentile > 0.9
      percentile_low <- x$percentile < 0.1
      if ((positive_corr && percentile_high) || (!positive_corr && percentile_low)) {
        return("✅ Sensitive")
      } else if ((!positive_corr && percentile_high) || (positive_corr && percentile_low)) {
        return("❌ Resistant")
      } else {
        return("❔Neither")
      }
    }
    correlation_direction <- apply(df_, MARGIN = 1, appendSmileys)
    df_ <- cbind(df_, correlation_direction)
    # Make the numbers round to 3 decimals
    df_$estimate <- round(as.numeric(df_$estimate), 3)
    df_$pvalue <- sprintf("%.2e", as.numeric(df_$pvalue))
    df_$percentile <- round(as.numeric(df_$percentile), 3)
    df_$score <- round(df_$score, 3)
    
    # Order the dataframe and add more human column names
    df_ <- df_[order(abs(df_$score), decreasing=TRUE),
               c('compound_name', 'gene_symbol', 'correlation_direction', 'score',
                 'estimate', 'pvalue', 'percentile', 'pubchem', 'tissue')]
    
    # Order the dataframe and add more human column names
    df_ <- df_[order(abs(df_$score), decreasing=TRUE),
      c('compound_name', 'gene_symbol', 'correlation_direction', 'score',
        'estimate', 'pvalue', 'percentile', 'pubchem', 'tissue')]
    rv$univarResultsDf <- df_
    colnames(df_) <- c(
      'Compound Name', 'Gene', 'Prediction',  'Prediction Score', 'Correlation',
      'Corr. P-value', 'Sample Expression Percentile', 'PubChem ID', 'Tissue Type')
    return (df_)

  }, selection='single')
}