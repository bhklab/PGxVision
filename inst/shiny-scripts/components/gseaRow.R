gseaRow <- function(index, pathway, estimate) {
  return(
    div( style='margin-bottom: 4px;',
         span(style='color: white; border-radius: 4px;
             font-weight: bold; padding: 1px 8px 1px 4px', as.character(index)),
         span(pathway, style='overflow-wrap: break-word; color: white;'),
         span(' '),
         span(style='color: white; background-color: #0c2461; margin-left: 8px;
             padding: 1px 6px; border-radius: 8px; font-weight: bold;',estimate))
  )
}