gseaRow <- function(title, index, pathway, estimate, genes) {
  return(
    div(title=title, style='margin-bottom: 4px; white-space: nowrap; overflow: hidden;',
         span(style='color: white; border-radius: 4px;    
             font-weight: bold; padding: 1px 8px 1px 4px', as.character(index)),
         span(pathway, style='overflow-wrap: break-word; color: white; font-weight: bold;'),
         span(' '),
         span(style='color: white; background-color: #0c2461; margin-left: 8px;
             padding: 1px 6px; border-radius: 8px; font-weight: bold;',estimate),
         span(style='color: rgba(255,255,255,0.7); font-weight: 100; margin-left: 8px; 
              font-style: italic;', genes),
         )
  )
}