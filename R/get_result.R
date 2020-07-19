#' Get Result Value
#'
#' @param study A study list object with class scivrs_study
#' @param result The name of the result value to retrieve (omit to return all results)
#' @param analysis_id The id for the relevant analysis (index or character) defaults to the first analysis in the list
#' @param digits Number of digits to round to (defaults to the value for options("digits")
#' @param return Return the value, character version, or an html linked version
#' @param analysis_link Name of the analysis file to link to if linking to html
#'
#' @return value of names result or list of results or html version
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
get_result <- function(study, result = NULL, analysis_id = 1,
                       digits = getOption("digits", 3),
                       return = c("value", "char", "html"),
                       analysis_link = "") {
  return <- match.arg(return)

  a_idx <- get_idx(study, analysis_id, "analyses")

  if (a_idx > length(study$analyses)) {
    stop("Analysis ", analysis_id, " does not exist.")
  } else if (is.null(study$analyses[[a_idx]]$results)) {
    warning("The analysis does not have results yet. Try running study_analyse first.")
    results <- list()
    class(results) <- c("scivrs_results", "list")
    return(results)
  }

  results <- study$analyses[[a_idx]]$results
  # round the results
  results <- round_char(results, digits, return != "value")

  if (is.null(result)) {
    # return all results
    class(results) <- c("scivrs_results", "list")
    return(results)
  } else {
    # returns built-in functions if they have the same name :(
    # e <- list2env(results)
    # res <- tryCatch(eval(parse(text = result), envir = e),
    #                 error = function(err) {
    #                   warning(result, " was not found")
    #                   NULL
    #                 })
    # return(res)

    # get value, handle indices in result
    splitres <- stringr::str_split(result, "(\\$|\\[+'?\"?|'?\"?\\]+)")
    res <- splitres[[1]][1]
    if (length(splitres[[1]]) == 1) {
      # no index
      idx <- NULL
    } else {
      idx <- splitres[[1]][2]

      # convert to numeric if it is
      suppressWarnings(n_idx <- as.numeric(idx))
      if (!is.na(n_idx)) idx <- n_idx
    }

    if (res %in% names(results)) {
      if (is.null(idx)) {
        value <- results[[res]]
      } else if (is.numeric(idx) & length(results[[res]]) < idx) {
        warning(res, " does not have a numeric index ", idx)
        return(NULL)
      } else if (is.character(idx) & !idx %in% names(results[[res]])) {
        warning(res, " does not have a named index ", idx)
        return(NULL)
      } else {
        value <- results[[res]][[idx]]
      }
    } else {
      # named result not found
      resnames <- names(results) %>% paste(collapse = ", ")
      warning("The result ", result, " is not found. Possible results are: ", resnames)
      return(NULL)
    }

    if (return != "html") return(value)

    # convert to html output
    html <- sprintf("<a href='%s#analysis_%d' title='Analysis %d Result %s'>%s</a>",
                    analysis_link, a_idx, a_idx, result, value[[1]])
    return(html)

  }
}


#' Get result value as HTML
#'
#' @param result The name of the result value to retrieve (omit to return all results)
#' @param analysis_id The id for the relevant analysis (index or character) defaults to the first analysis in the list
#' @param digits Number of digits to round to (defaults to the value for options("digits")
#'
#' @return html
#' @export
#'
get_html <- function(result = NULL, analysis_id = 1,
         digits = getOption("digits", 3)) {
  # get a study object from the global environment
  env <- .GlobalEnv
  classes <- eapply(env, class)

  for (obj in names(classes)) {
    if ("scivrs_study" %in% classes[[obj]]) {
      study <- parse(text = obj) %>% eval(env)
      idx <- get_idx(study, analysis_id, "analyses")
      if (idx <= length(study$analyses)) break
    }
  }

  # get analysis link
  analysis_link <- scienceverse_options("analysis_link")
  if (is.null(analysis_link)) analysis_link <- ""

  get_result(study, result, analysis_id, digits, return = "html", analysis_link)
}
