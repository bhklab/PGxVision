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
      div(style='width: 33%',
          # p("Please upload your reference population file"),
          fileInput("referencePopulationDf",
                    "",
                    accept=c("text/csv", ".csv"), buttonLabel="Browse files"),
          createTip("Tip", "File format must be .csv"),
      ),
      div(style='width: 67%; background-color: #ecf0f1; border-radius: 8px; padding: 8px;',
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
    if (identical(rv$univarResultsDf, NULL)) { return(p("Please upload your reference population file.")) }
    else {
      index <- input$pdbBiomarkers_rows_selected
      if (identical(index, NULL)) { return(p("Select a row to learn more."))}

      # Find reevant pathways
      gene <- rv$univarResultsDf[index,]$gene_symbol
      relevantPathways <- globalRV$ssGseaResults[grepl(pattern=gene, globalRV$ssGseaResults$genes), ]

      return(
        div(style='display: flex;',
            div(style='width: 40%;',
              div(span(style='font-weight: bold', "Gene"), span(gene)),
              div(span(style='font-weight: bold', "Gene Set Category"), span(globalRV$geneSetCategory)),
              div(span(style='font-weight: bold', 'Enriched Pathways')),
              div(style='display: flex; flex-wrap: wrap;',
                  lapply(rownames(relevantPathways), \(x) {
                    return (
                      div(style='background-color: white; padding: 1px 8px; word-wrap: break-word;
                      border-radius: 15px; margin: 2px 4px 2px 0px; display: inline-block',
                          span(x)
                      ))})
              ),
            ),
            div(style='width: 60%; height: 200px; border-radius: 8px; overflow: hidden;
                background-color: white;',
                renderPlot(PGxVision::densityPlotBiomarkerPercentile(
                  gene, # the gene of interest (string)
                  rv$patientDf[gene, ], # the patient's expression for the gene chosen (float)
                  rv$referenceDf), height=200)
            )
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
    rv$univarResultsDf <- df_
    colnames(df_) <- c(
      'Compound Name', 'Gene', 'Prediction',  'Prediction Score', 'Correlation',
      'Corr. P-value', 'Sample Expression Percentile', 'PubChem ID', 'Tissue Type')
    return (df_)

  }, selection='single')
}