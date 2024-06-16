
#' Launch a shinyhamstr shiny app
#'
#' @return NULL
#' @export
shinyhamstr <- function(){

  options(shiny.host = "0.0.0.0")
  options(shiny.port = 9292)
  options(shiny.maxRequestSize=300*1024^2)

  shiny::shinyApp(ui, server)
}


