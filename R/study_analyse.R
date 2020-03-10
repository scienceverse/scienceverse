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
    if (scienceverse_options("verbose")) {
      message("No analyses have been specified")
    }
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
      if (scienceverse_options("verbose")) {
        message("Hypothesis ", h$id, " has no criteria")
      }
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

        if (scienceverse_options("verbose")) {
          message("Hypothesis ", h$id, ", Criterion ", criterion$id, ": ",
                  criterion$result, " ", criterion$operator,
                  " ", criterion$comparator, " is ", conclusion,
                  " (", criterion$result, " = ", round_char(value, 2), ")")
        }
      }

      # evaluate hypothesis ----
      replacement <- as.character(criteria)
      names(replacement) <- names(criteria)

      if (is.null(h$corroboration$evaluation)) {
        warning("Hypothesis ", h$id, " has no evaluation criteria for corroboration")
        corrob <- FALSE
      } else {
        tryCatch({
          corrob_pieces <- h$corroboration$evaluation %>%
            gsub("(\\(|\\)|\\||\\!|&)", " \\1 ", .) %>%
            strsplit("\\s+", perl = TRUE) %>%
            magrittr::extract2(1)
          potential_criteria <- grep("^[a-zA-Z0-9_]+$", corrob_pieces)
          for (pc in potential_criteria) {
            crit <- corrob_pieces[pc]
            if (crit %in% names(replacement)) {
              corrob_pieces[pc] <- replacement[[crit]]
            } else {
              stop(crit, " is not a defined criterion")
            }
          }

          corrob <- corrob_pieces %>%
            paste(collapse = "") %>%
            parse(text = .) %>%
            eval(envir = .GlobalEnv)
        }, error = function(e) {
          warning("Hypothesis ", h$id, " has an error in the evaluation criteria for corroboration: ", h$corroboration$evaluation)
          corrob <<- FALSE
        })
      }
      if (is.null(h$falsification$evaluation)) {
        warning("Hypothesis ", h$id, " has no evaluation criteria for falsification")
        falsify <- FALSE
      } else {
        tryCatch({
          falsify_pieces <- h$falsification$evaluation %>%
            gsub("(\\(|\\)|\\||\\!|&)", " \\1 ", .) %>%
            strsplit("\\s+", perl = TRUE) %>%
            magrittr::extract2(1)
          potential_criteria <- grep("^[a-zA-Z0-9_]+$", falsify_pieces)
          for (pc in potential_criteria) {
            crit <- falsify_pieces[pc]
            if (crit %in% names(replacement)) {
              falsify_pieces[pc] <- replacement[[crit]]
            } else {
              stop(crit, " is not a defined criterion")
            }
          }

          falsify <- falsify_pieces %>%
            paste(collapse = "") %>%
            parse(text = .) %>%
            eval(envir = .GlobalEnv)
        }, error = function(e) {
          warning("Hypothesis ", h$id, " has an error in the evaluation criteria for falsification: ", h$falsification$evaluation)
          falsify <<- FALSE
        })
      }

      study$hypotheses[[i]]$corroboration[["result"]] <- corrob
      study$hypotheses[[i]]$falsification[["result"]] <- falsify

      if (corrob & !isTRUE(falsify)) {
        study$hypotheses[[i]]$conclusion = "corroborate"
      } else if (!isTRUE(corrob) & falsify) {
        study$hypotheses[[i]]$conclusion = "falsify"
      } else {
        study$hypotheses[[i]]$conclusion = "inconclusive"
      }

      if (scienceverse_options("verbose")) {
        message("Hypothesis ", h$id, ":\n",
                "    Corroborate: ", corrob, "\n",
                "    Falsify: ", falsify, "\n",
                "    Conclusion: ", study$hypotheses[[i]]$conclusion)
      }
    }
  }

  invisible(study)
}


#' @rdname study_analyse
#' @export
study_analyze <- study_analyse
