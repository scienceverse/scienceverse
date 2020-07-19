#' Run analysis
#'
#' Run the analyses on the data
#'
#' @param study A study list object with class scivrs_study
#' @return A study object with class scivrs_study
#' @export
#' @examples
#'
#' s <- study() %>%
#'   add_hypothesis("H1", "Petal width and length will be positively correlated.") %>%
#'   add_analysis("A1", cor.test(dat$Petal.Width, dat$Petal.Length)) %>%
#'   add_criterion("sig", "p.value", "<", 0.05) %>%
#'   add_criterion("pos", "estimate", ">", 0) %>%
#'   add_eval("corroboration",
#'            "Petal width is significantly and positively correlated to length",
#'            "sig & pos") %>%
#'   add_eval("falsification",
#'            "Petal width is significantly and negatively correlated to length",
#'            "sig & !pos") %>%
#'   add_data("dat", iris) %>%
#'   study_analyse()
#' study_to_json(s)
#'
study_analyse <- function(study) {
  analysis_n <- length(study$analyses)
  if (analysis_n == 0) {
    if (scienceverse_options("verbose")) {
      message("No analyses have been specified")
    }
    return(invisible(study))
  }

  # get study environment
  env <- attr(study, "env")

  # load data
  for (d in study$data) {
    assign(d$id, d$data, envir = env)
  }

  # run analyses ----
  for (i in 1:analysis_n) {
    func <- paste0("analysis_", study$analyses[[i]]$id)

    # check the analysis function exists
    if (!methods::existsFunction(func, where = env)) {
      stop("The function for analysis ",
           study$analyses[[i]]$id,
           " is not defined")
    }

    # save results, convert to list, and make class list
    # (jsonlite doesn't deal well with non-list classes like htest , etc)
    study$analyses[[i]]$results <- do.call(func, list(), envir = env) %>%
      as.list()
    class(study$analyses[[i]]$results) <- "list"
  }

  # evaluate hypotheses based on the results
  study <- study_eval(study)

  invisible(study)
}


#' @rdname study_analyse
#' @export
study_analyze <- study_analyse
