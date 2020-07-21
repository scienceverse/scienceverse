#' Launch Shiny App
#'
#' Create a meta-study file interactively in a shiny app that runs locally in RStudio or your web browser (recommended). It does not connect to the web at all, so your data are completely private.
#'
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'
#' @examples
#' \dontrun{ scivrs_app() }
#'
scivrs_app <- function(...) {
  shiny::runApp(appDir = system.file("app", package = "scienceverse"), ...)
}
