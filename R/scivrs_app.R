#' Launch Shiny App
#'
#' Create a meta-study file interactively in a shiny app that runs locally in RStudio or your web browser (recommended). It does not connect to the web at all, so your data are completely private.
#'
#' @param study optional study to load
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'
#' @returns A study object created or edited by the app
#'
#' @examples
#' \dontrun{ s <- scivrs_app() }
#'
scivrs_app <- function(study = NULL, ...) {
  # check study
  if (!is.null(study) && !"scivrs_study" %in% class(study)) {
    stop("The argument study must be a study object created by scienceverse, or NULL to create it entirely in the app.")
  }

  # check required packages
  pckgs <- c("shiny", "shinydashboard", "shinyjs",
             "shiny.i18n", "DT", "dplyr", "tidyr")
  names(pckgs) <- pckgs
  req_pckgs <- sapply(pckgs, requireNamespace, quietly = TRUE)

  if (all(req_pckgs)) {
    .GlobalEnv$.app.study. <- study
    on.exit(rm(.app.study., envir=.GlobalEnv))

    # # get all local scivrs study objects
    # all <- mget(ls(), envir = globalenv())
    # ss <- lapply(all, class) |> sapply(\(x) "scivrs_study" %in% x)
    # sss <- all[ss]

    shiny::runApp(appDir = system.file("app", package = "scienceverse"), ...)
  } else {
    warning("You need to install the following packages to run the app: ",
            paste(names(req_pckgs[!req_pckgs]), collapse = ", "))
  }
}
