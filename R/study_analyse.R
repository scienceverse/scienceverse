#' Run analysis
#'
#' Run the analyses on the data
#'
#' @param study A study list object with class reg_study
#' @return A study object with class reg_study
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
#' study_json(s)
#'
study_analyse <- function(study) {
  analysis_n <- length(study$analyses)
  if (analysis_n == 0) {
    message("No analyses have been specified")
    return(invisible(study))
  }

  # load data
  for (d in study$data) {
    assign(d$id, d$data, envir = .GlobalEnv)
  }

  # run analyses ----
  for (i in 1:analysis_n) {
    func <- study$analyses[[i]]$func

    # check the analysis function exists
    if (!methods::existsFunction(func)) {
      stop("The function for analysis ",
           study$analyses[[i]]$id,
           " is not defined")
    }

    # save results, convert to list, and make class list
    # (jsonlite doesn't deal well with non-list classes like htest , etc)
    study$analyses[[i]]$results <- do.call(func, list()) %>%
      as.list()
    class(study$analyses[[i]]$results) <- "list"
  }

  # evaluate each hypothesis ----
  hypothesis_n <- length(study$hypotheses)
  for (i in 1:hypothesis_n) {
    h <- study$hypotheses[[i]]
    # evaluate each criterion ----
    criteria_n <- length(h$criteria)
    if (criteria_n == 0) {
      message("Hypothesis ", h$id, " has no criteria")
    } else {
      criteria <- vector()
      analysis_ids <- sapply(study$analyses, function(x) {x$id})
      for (j in 1:criteria_n) {
        criterion <- h$criteria[[j]]
        analysis <- match(criterion$analysis_id, analysis_ids)

        # get value, handle indices in result
        splitres <- stringr::str_split(criterion$result, "(\\[|\\])")
        res <- splitres[[1]][1]
        idx <- as.integer(splitres[[1]][2])
        idx <- ifelse(isTRUE(idx > 0), idx, 1)
        value <- study$analyses[[analysis]]$results[[res]][idx]

        if (criterion$operator == "<") {
          conclusion <- value < criterion$comparator
        } else if (criterion$operator == ">") {
          conclusion <- value > criterion$comparator
        } else if (criterion$operator == "=") {
          conclusion <- value == criterion$comparator
        } else if (criterion$operator == "!=") {
          conclusion <- value != criterion$comparator
        } else {
          conclusion <- NA
        }
        criteria[criterion$id] <- conclusion
        study$hypotheses[[i]]$criteria[[j]]$conclusion <- conclusion

        message("Hypothesis ", h$id, ", Criterion ", criterion$id, ": ",
                criterion$result, " ", criterion$operator,
                " ", criterion$comparator, " is ", conclusion,
                " (", criterion$result, " = ", round_char(value, 2), ")")
      }

      # evaluate hypothesis ----
      replacement <- as.character(criteria)
      names(replacement) <- names(criteria)

      corrob <- h$corroboration$evaluation %>%
        gsub("\\s+", "", .) %>%
        strsplit("(?<=[\\W+])", perl = TRUE) %>%
        magrittr::extract2(1) %>%
        stringr::str_replace_all(replacement) %>%
        paste(collapse = "") %>%
        parse(text = .) %>%
        eval(envir = .GlobalEnv)

      falsify <- h$falsification$evaluation %>%
        gsub("\\s+", "", .) %>%
        strsplit("(?<=[\\W+])", perl = TRUE) %>%
        magrittr::extract2(1) %>%
        stringr::str_replace_all(replacement) %>%
        paste(collapse = "") %>%
        parse(text = .) %>%
        eval(envir = .GlobalEnv)

      if (corrob & !falsify) {
        study$hypotheses[[i]]$conclusion = "corroborate"
      } else if (!corrob & falsify) {
        study$hypotheses[[i]]$conclusion = "falsify"
      } else {
        study$hypotheses[[i]]$conclusion = "inconclusive"
      }

      message("Hypothesis ", h$id, ", Evaluation: ", study$hypotheses[[i]]$conclusion)
    }
  }

  invisible(study)
}


#' @rdname study_analyse
#' @export
study_analyze <- study_analyse
