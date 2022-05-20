createTip <- function(title, tip, primaryColor='#1e3799', secondaryColor='#f0f3fa') {
  return(
    div(style='display: flex; justify-content: left; margin: 4px 4px 4px 0px',
        span(title, style=paste0('padding: 2px 8px 2px 6px; color: white; font-weight: bold;
          border-radius: 4px 0px 0px 4px; background-color: ', primaryColor)),
        span(tip, style=sprintf('background-color: %s; border: 1px solid %s;
          border-radius: 0px 4px 4px 0px; padding: 1px 8px 1px 8px; color: %s', secondaryColor, primaryColor, primaryColor)))
  )
}