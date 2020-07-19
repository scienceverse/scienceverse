#' Launch Shiny App
#'
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'
scivrs_app <- function(...) {
  shiny::runApp(appDir = system.file("app", package = "scienceverse"), ...)
}
