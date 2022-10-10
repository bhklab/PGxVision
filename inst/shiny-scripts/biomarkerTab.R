### IMPORT COMPONENTS ###
source('components/tip.R')

### INITIALIZE VARIABLES ###

sampleDataDir <- "extdata/sample_data/"
biomarkerFile <- system.file(paste0(sampleDataDir, "sampleBiomarkers.csv"),
                             package="PGxVision")
Biomarkers <- read.csv(biomarkerFile)
genomeFile <- system.file(paste0(sampleDataDir, "sampleGenome.csv"),
                          package="PGxVision")
GRCh38p13Assembly <- read.csv(genomeFile)
pdxFile <- system.file(paste0(sampleDataDir, "sampleTreatmentResponse.csv"),
                       package="PGxVision")
brcaPdxePaxlitaxelResponse <- read.csv(pdxFile)

gsTypes <- unique(msigdbr::msigdbr_collections()$gs_subcat)
blankGene <- data.table::data.table(gene="", abs_gene_seq_start="", chr="",
                                    pvalue="", estimate="", fdr="")
blankGeneSet <- data.table::data.table(gs_id="", gs_name="", gs_exact_source="",
                                       gs_url="", gs_description="")

### INITIALIZE ###

# Function to initialize everything in this page
biomarkerTabInitialize <- function(input, output) {
  biomarkerTabRV <- biomarkerTabCreateRV()
  biomarkerTabObservers(input, biomarkerTabRV)
  biomarkerTabOutputUI(input, biomarkerTabRV, output)
}

### INPUT ###

# Helper functions

# Return all tab input rows 
biomarkerTabUI <- div(
  h5(align='center', style='font-weight: bold', 'Filter Biomarkers'),
  br(), 
  div(class='flex',style='justify-content: space-between;',
      uiOutput("tissueSelect"), 
      uiOutput("compoundSelect"),
      uiOutput("mDataTypeSelect"),
      sliderInput("pValCutoff", "P-Value Cutoff", min=0, max=1, value=0.05, width='230px'),
  ),
  div(class='flex',style='justify-content: space-between;',
      createTip("Note", "The PharmacoDB biomarker set and GRCh38.p13 genome will be used automatically"),
      
  ),
  #hr(), 
  #h5(align='center', style='font-weight: bold', 'Plot Properties'),
  br(),
  
  # TODO: something funny happens when pval >= 0.39 ?? consider
  # reducing range of slider
  div(style='display: flex', 
      div(class='flex', style='flex-direction: column-reverse',
          plotlyOutput("manhattanPlot", height='300px', width='700px'),
          plotlyOutput("volcanoPlot", height='300px', width='700px'),
      ),
      div(style='background-color: #f1f2f6; border-radius: 0px 0px 8px 8px; margin-left: 8px',
          p(style='background-color: #0c2461; color: white; font-family: source sans pro;
         padding: 8px 16px; border-radius: 8px 8px 0px 0px; font-size: 24px; font-weight: bold;
        margin-bottom: 0px', "Biomarker Info"),
          div(style='padding: 16px',
              uiOutput("geneInfoBox"),
          ),
      ),
  ),
  
  hr(), 
  h5(align='center', style='font-weight: bold', 'Gene Set Analysis'),
  br(),
  
  
  div(class='flex', style='justify-content: space-between; align-items: end;',
      uiOutput("geneSelect"),
      selectInput("gsType", "Gene Set Type",
                  c("Select a gene set type..." = "", gsTypes), selected = "", width='230px'),
      selectInput("simAlgo", "Similarity Algorithm", c("overlap"), width='230px'),
      sliderInput("simCutoff", "Similarity Cutoff",
                  min = 0, max = 1, value = 0.05, width='230px'),
  ),
  actionButton("runGsAnalysis", "Run Gene Set Analysis!"),
  
  uiOutput("plotError"),
  shinybusy::use_busy_spinner(spin = "fading-circle"),
  
  div(style='display: flex; width: 100%;', 
      div(style='min-width: 700px',
          visNetwork::visNetworkOutput("networkPlot", width='700px'),
      ),
      div(style='background-color: #f1f2f6; border-radius: 0px 0px 8px 8px; 
          margin-left: 8px; flex-grow:1; min-width: 0',
          p(style='background-color: #0c2461; color: white; font-family: source sans pro;
         padding: 8px 16px; border-radius: 8px 8px 0px 0px; font-size: 24px; font-weight: bold;
        margin-bottom: 0px', "Gene Set Info"),
          div(style='padding: 16px',
              uiOutput("gsInfo")
          ),
      ),
  ),
)

### REACTIVE VALUES AND OBSERVERS ###

# object containing all relevant reactive values for the biomarker tab
biomarkerTabCreateRV <- function() {
  return( reactiveValues(biomarkerDf = Biomarkers,
                         chromosomeDf = GRCh38p13Assembly,
                         plottedBiomrkrs = NULL,
                         selectedGene = blankGene,
                         geneSets = NULL,
                         gsSimilarityDf = NULL,
                         error = NULL))
}

# Return all reactive variable observers
biomarkerTabObservers <- function(input, rv) {
  # Update biomarkerDf and chromosomeDf based on file uploads
  #observeEvent(input$biomarkerFile, {
  #  req(input$biomarkerFile)
  #  rv$biomarkerDf <- read.csv(input$biomarkerFile$datapath)
  #})
  
  #observeEvent(input$genomeFile, {
  #  req(input$genomeFile)
  #  rv$chromosomeDf <- read.csv(input$genomeFile$datapath)
  #})
  
  # Update & rerender network plot only when the runGsAnalysis button is pressed
  observeEvent(input$runGsAnalysis, {
    rv$gsSimilarityDf <- NULL
    rv$geneSets <- NULL
    tryCatch({
      # Gene set analysis can take a while, so show a spinner in the meantime
      shinybusy::show_spinner()
      
      gsAnalysis <- PGxVision::geneSetAnalysis(
        input$gene, input$gsType, input$simAlgo)
      rv$gsSimilarityDf <- gsAnalysis$similarityDf
      rv$geneSets <- gsAnalysis$geneSets
      rv$error <- NULL
      
      shinybusy::hide_spinner()
    },
    error=function(e) {
      hide_spinner()
      rv$error <- e$message
    })
  })
  
  # Update selected gene when users click on plots
  observe({
    req(rv$plottedBiomrkrs)
    
    if (!is.null(event_data("plotly_click", source = "manhattan"))) {
      d <- event_data("plotly_click", source = "manhattan")
      rv$selectedGene <- rv$plottedBiomrkrs[abs_gene_seq_start == d$x, ]
    }
  })
  
  observe({
    req(rv$plottedBiomrkrs)
    
    if (!is.null(event_data("plotly_click", source = "volcano"))) {
      d <- event_data("plotly_click", source = "volcano")
      rv$selectedGene <- rv$plottedBiomrkrs[estimate == d$x, ]
    }
  })
  
  #TODO: I want the equivalent data points on the other plot to highlight
}

### OUPUT ###

biomarkerTabOutputUI <- function(input, rv, output) {
  # Get all gene, tissues, compounds, and mDataTypes from biomarkerDf
  # and update dropdown elements accordingly
  output$geneSelect <- renderUI({
    geneChoices <- unique(rv$biomarkerDf$gene) #FIXME: unsafe
    selectInput("gene", "Gene", c("Select a gene..." = "", geneChoices),
                selected = "", width='230px')
  })
  
  output$tissueSelect <- renderUI({
    tissueChoices <- unique(rv$biomarkerDf$tissue) #FIXME: unsafe
    selectInput("tissue", "Tissue", c("Select a tissue..." = "", tissueChoices),
                selected = "", width='230px')
  })
  
  output$compoundSelect <- renderUI({
    compoundChoices <- unique(rv$biomarkerDf$compound) #FIXME: unsafe
    selectInput("compound", "Compound/Drug",
                c("Select a compound..." = "", compoundChoices), 
                selected = "", width='230px')
  })
  
  output$mDataTypeSelect <- renderUI({
    mDataTypeChoices <- unique(rv$biomarkerDf$mDataType) #FIXME: unsafe
    selectInput("mDataType", "Molecular Data Type",
                c("Select a molecular data type..." = "", mDataTypeChoices),
                selected = "", width='230px')
  })
  
  # Update plots based on tissue, compound, mDataType selections
  output$manhattanPlot <- renderPlotly({
    req(rv$biomarkerDf)
    
    # Wait until everything renders and inputs are not NULL
    if (typeof(input$tissue) == "character" &&
        typeof(input$compound) == "character" &&
        typeof(input$mDataType) == "character") {
      result <- suppressWarnings(
        PGxVision::buildManhattanPlot(
          rv$biomarkerDf, rv$chromosomeDf, input$tissue,
          input$compound, input$mDataType, input$pValCutoff))
      rv$plottedBiomrkrs <- result$dt
      ggplotly(result$plot, source = "manhattan") %>%
        # Modify aesthetics because ggplotly overrides aes from ggplot2
        # NOTE: bottom padding doesn't work; idk why
        # TODO: cite https://plotly-r.com/improving-ggplotly.html
        layout(title=list(font=list(size = 16), pad = list(b = 25))) %>%
        style(line = list(width = 1, color = "black", dash = "dot"), traces = 1)
    }
  })
  
  output$volcanoPlot <- renderPlotly({
    req(rv$biomarkerDf)
    
    # Wait until everything renders and inputs are not NULL
    if (typeof(input$tissue) == "character" &&
        typeof(input$compound) == "character" &&
        typeof(input$mDataType) == "character") {
      p <- suppressWarnings(
        PGxVision::buildVolcanoPlot(
          rv$biomarkerDf, input$tissue, input$compound,
          input$mDataType, pValCutoff = input$pValCutoff)$plot)
      ggplotly(p, source = "volcano") %>%
        # Modify aesthetics because ggplotly overrides aes from ggplot2
        # TODO: cite https://plotly-r.com/improving-ggplotly.html
        layout(title = list(font = list(size = 16))) %>%
        style(line = list(width = 1, color = "black", dash = "dot"), traces = 1)
    }
  })
  
  # Update biomarker info box in response to new selected gene
  output$geneInfoBox <- renderUI({
    req(rv$selectedGene)
    div(
      
      div(style='font-family: source sans pro', tags$b("Gene: "), rv$selectedGene[1, gene]),
      div(style='font-family: source sans pro', tags$b("Genome Position: "), rv$selectedGene[1, abs_gene_seq_start]),
      div(style='font-family: source sans pro', tags$b("Chromosome: "), rv$selectedGene[1, chr]),
      div(style='font-family: source sans pro', tags$b("Estimate: "), rv$selectedGene[1, estimate]),
      div(style='font-family: source sans pro', tags$b("P-Value: "), rv$selectedGene[1, pvalue]),
      div(style='font-family: source sans pro', tags$b("FDR: "), rv$selectedGene[1, fdr]),
      
      p("Click on any point to see more information about the gene."), br(),
    )
  })
  
  output$plotError <- renderUI({
    req(rv$error)
    p(rv$error)
  })
  
  output$networkPlot <- visNetwork::renderVisNetwork({
    req(rv$geneSets, rv$gsSimilarityDf)
    
    p <- PGxVision::buildNetworkPlot(rv$gsSimilarityDf, input$simCutoff) %>%
      # Add JS hook to react to node selection
      # Js code taken from xclotet:
      # xclotet. (2016). Get selected Node data from visNetwork graph without
      # actionButton. StackOverflow.
      # https://stackoverflow.com/questions/41018899/get-selected-node-data-from-visnetwork-graph-without-actionbutton/41020222
      visNetwork::visEvents(select = "function(nodes) {
        Shiny.onInputChange('currentNodeId', nodes.nodes);}")
    p
  })
  
  # React to node selection
  output$gsInfo <- renderUI({
    if (length(input$currentNodeId) > 0) {
      gsInfo <- rv$geneSets[gs_id == input$currentNodeId,]
      link <- p("Learn more at", a(gsInfo$gs_url, href=gsInfo$gs_url))
    } else {
      gsInfo <- blankGeneSet
      link <- ""
    }
    
    div(div(tags$b("ID: "), gsInfo$gs_id),
        div(tags$b("Name: "), p(style='word-break: break-all;', gsInfo$gs_name)),
        div(tags$b("Source: "), p(gsInfo$gs_exact_source)),
        div(tags$b("Description: ")),
        p(gsInfo$gs_description),
        link
    )
  })
}