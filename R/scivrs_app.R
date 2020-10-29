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
  pckgs <- c("shiny", "shinydashboard", "shinyjs",
             "shiny.i18n", "DT", "dplyr", "tidyr")
  names(pckgs) <- pckgs
  req_pckgs <- sapply(pckgs, requireNamespace, quietly = TRUE)

  if (all(req_pckgs)) {
    shiny::runApp(appDir = system.file("app", package = "scienceverse"), ...)
  } else {
    warning("You need to install the following packages to run the app: ",
            paste(names(req_pckgs[!req_pckgs]), collapse = ", "))
  }
}
