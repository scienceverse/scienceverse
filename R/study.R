#' Create a study object
#'
#' Create a new study object or load a study from the JSON meta-study file
#'
#' @param name The name of the study or a file path to a JSON meta-study file
#' @param ... further arguments to add
#' @return A study object with class scivrs_study
#' @examples
#'
#' s <- study("Demo Study")
#' study_to_json(s)
#'
#' @export
#'
study <- function(name = "Demo Study", ...) {
  if (isTRUE(grep("\\.json$", name) == 1)) {
    study <- study_from_json(name)

    # overwrite info
    dots <- list(...)
    dotnames <- names(dots)
    for (n in dotnames) {
      study$info[n] <- dots[n]
    }
  } else {
    # make empty study object
    study <- c(
      list(name = name),
      list(
        info = list(...),
        authors = list(),
        hypotheses = list(),
        methods = list(),
        data = list(),
        analyses = list()
      )
    )

    class(study) <- c("scivrs_study", "list")
    # create a new environment for this study
    attr(study, "env") <- new.env()
  }

  invisible(study)
}

