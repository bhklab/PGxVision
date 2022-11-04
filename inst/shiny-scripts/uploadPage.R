### INITIALIZE VARIABLES ###
source('components/tip.R')

# Function to initialize everything in this page
uploadPageInitialize <- function(input, output, navigate, globalRV) {
  uploadPageRV <- uploadPageCreateRV()
  uploadPageObservers(input, uploadPageRV, output, navigate, globalRV)
}

### INPUT ###

# Helper functions

# Return page UI
uploadPageUI <- fluidPage(
  
  # Set headers
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/uploadPage.css")),
  
  add_busy_bar(color = "#1e3799"), # To show busy indicator
  
  # Title
  h1("PGxVision", align='center'),
  br(),
  
  # Upload file form
  div(class="flex", 
      div(class='card shadow', align='center',
          h4("Upload patient gene expression file"),
          fileInput('patientDf', label="",
                    accept=c("text/csv", ".csv"), buttonLabel="Browse files"),
          createTip("Tip", "File format must be .csv"),
      )),
  
  # Other utilities
  br(),
  h5("Other tools", align='center'),
  div(class='flex', 
      # Drug treatment tab button
      div(id='drugTreatment', class='otherToolButton flex', style='flex-direction: column;', 
          br(),
          img(src='./icons/pills.png', height='64px', width='64px'),
          br(),
          p("Drug Treatment", align='center')),
      
      # Other button
      # TODO: make this button do something
      div(class='otherToolButton flex', style='flex-direction: column;', br(),
          img(src='./icons/bar-chart.png', height='64px', width='64px'),
          br(),
          p("Other Analysis", align='center')),
  ),
)

### REACTIVE VALUES AND OBSERVERS ###

uploadPageCreateRV <- function() {
  # No reactive values needed so far for the upload page
  return( reactiveValues())
}

# Return all reactive variable observers
uploadPageObservers <- function(input, rv, output, navigate, globalRV) {
  
  # Observe when user uploads a CSV file (patient's gene expression matrix)
  observeEvent(input$patientDf, {
    req(input$patientDf) # Validate input
    df_ <- data.table::fread(input$patientDf$datapath) 
    
    # Store the data frame in the global (application-level) reactive values
    
    globalRV$patientDf <- data.frame(df_[, -1], row.names=df_[[1]]) 
    
    # Navigate to the analysis tab
    navigate('analysis', output)
  })
  
  # Observe when the drug treatment button is clicked
  observe({
    onclick("drugTreatment", navigate('treatment', output))
  })
}

### OUTPUT ###