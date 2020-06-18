#' Get Result Value
#'
#' @param study A study list object with class scivrs_study
#' @param result The name of the result value to retrieve (omit to return all results)
#' @param analysis_id The id for the relevant analysis (index or character) defaults to the first analysis in the list
#'
#' @return value of names result or list of results
#' @export
#'
#' @examples
#' s <- study() %>%
#'   add_hypothesis("H1", "Petals are wider than long") %>%
#'   add_analysis("A1", t.test(iris$Petal.Width, iris$Petal.Height)) %>%
#'   add_criterion("p", "p.value", "<", .05) %>%
#'   study_analyse()
#'
#'  get_result(s, "p.value", "A1")
#'
get_result <- function(study, result = NULL, analysis_id = 1) {
  idx <- get_idx(study, analysis_id, "analyses")

  if (idx > length(study$analyses)) {
    stop("Analysis ", analysis_id, " does not exist.")
  } else if (is.null(study$analyses[[idx]]$results)) {
    stop("The analysis does not have results yet. Try running study_analyse first.")
  }

  if (is.null(result)) {
    # return all results
    return(study$analyses[[idx]]$results)
  } else if (is.null(study$analyses[[idx]]$results[[result]])) {
    # named result not found
    resnames <- names(study$analyses[[idx]]$results) %>%
      paste(collapse = ", ")
    stop("The result ", result, " is not found. Possible results are: ", resnames)
  } else {
    return(study$analyses[[idx]]$results[[result]])
  }
}


