gseaRowDetailed <- function(title, index, pathway, estimate, genes) {
  return(
    div(style='margin-bottom: 4px;',
        div(style='margin-bottom: 2px;',
          span(style='color: black; border-radius: 4px;    
             font-weight: bold; padding: 1px 8px 1px 4px', as.character(index)),
          span(pathway, style='overflow-wrap: break-word; color: black; font-weight: bold;'),
          span( style='color: black; background-color: lightgray; margin-left: 8px;
             padding: 1px 6px; border-radius: 8px; font-weight: bold;',estimate),
        ),
        div(
          p(style='margin-bottom: 4px;', title),
          p(style='color: gray; font-weight: 100; 
            font-size: 9pt; font-style: italic;', genes),
        ),
        
    )
  )
}