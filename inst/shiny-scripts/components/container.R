container <- function(...) {
  return(
    div(style='display: flex; justify-content: center',
        div(style='width: 1000px',
            ...))
  )
}