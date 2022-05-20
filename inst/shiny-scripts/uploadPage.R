### INITIALIZE VARIABLES ###
source('components/tip.R')

### INPUT ###

# Helper functions

# Return page UI
uploadPageUI <- function (uploadedFileId) {
  
  return (fluidPage(
    
    # Set headers
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "app.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "uploadPage.css")),
    
    add_busy_bar(color = "#1e3799"), # To show busy indicator
    
    h1("PGxVision", align='center'),
    br(),
    
    # Upload file form
    div(class="flex", 
        div(class='card shadow', align='center',
            h4("Upload patient gene expression file"),
            fileInput(uploadedFileId, label="",
                      accept=c("text/csv", ".csv"), buttonLabel="Browse files"),
            createTip("Tip", "File format must be .csv"),
            #createTip("Error", "Please upload a .csv file", "#e55039", "#fbe3df"),
        )),
    
    # Other utilities
    br(),
    h5("Other tools", align='center'),
    div(class='flex', 
        div(class='otherToolButton flex', style='flex-direction: column;', br(),
            img(src='./icons/pills.png', height='64px', width='64px'),
            br(),
            p("Drug Treatment", align='center')),
        
        div(class='otherToolButton flex', style='flex-direction: column;', br(),
            img(src='./icons/bar-chart.png', height='64px', width='64px'),
            br(),
            p("Other Analysis", align='center')),
    ),
  ))
}

