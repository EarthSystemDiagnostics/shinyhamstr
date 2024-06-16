
#' Launch a shinyhamstr shiny app
#'
#' @return NULL
#' @export
shinyhamstr <- function(){
  shiny::shinyApp(ui, server)
}


