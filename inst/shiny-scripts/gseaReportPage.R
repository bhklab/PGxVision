### IMPORT COMPONENTS ###
source('components/container.R')

### INITIALIZE VARIABLES ###

# Function to initialize everything in this page
gseaReportPageInitiatize <- function(input, output, navigate) {
  gseaReportPageRV <- gseaReportPageCreateRV()
  gseaReportPageObservers(input, gseaReportPageRV, output, navigate)
  gseaReportPageOutputUI(input, gseaReportPageRV, output)
}

### INPUT ###

# Return all tab input rows 
gseaReportPageUI = container(
  # Set headers
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/analysisPage.css")),
  
  div(class='header',
      div(id='home', 
          class='flex', 
          img(style='display: inline-block;', src='./icons/home.png', height='16px', width='16px'),
          span(style='color: white; font-weight: bold;', "Home"),
      ),
      h2(style='color: white', "GSEA Report")
  ),
  br(),br(),
  div(style='display: flex; justify-content: center',
      div(class='card shadow', style='width: 100%', 
          # TODO: Figure out what goes here
      ),
  ),
)

### REACTIVE VALUES AND OBSERVERS ###

gseaReportPageCreateRV <- function() {
  return( reactiveValues())
}

# Return all reactive variable observers
gseaReportPageObservers <- function(input, rv, output, navigate) {
  # Observe home button click
  observe({
    onclick("home", navigate('home', output))
  })
}

### OUTPUT ###

gseaReportPageOutputUI <- function(input, rv, output) {

}