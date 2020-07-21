#' Evaluate study results
#'
#' @param study A study list object with class scivrs_study
#' @return A study object with class scivrs_study
#' @export
#'
study_eval <- function(study) {
  # evaluate each hypothesis ----
  hypothesis_n <- length(study$hypotheses)

  if (hypothesis_n == 0) {
    if (scienceverse_options("verbose")) {
      message("The study has no hypotheses.")
    }
    return(invisible(study))
  }

  analysis_ids <- sapply(study$analyses, function(x) {x$id})

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
      for (j in 1:criteria_n) {
        criterion <- h$criteria[[j]]
        analysis <- match(criterion$analysis_id, analysis_ids)
        results <- study$analyses[[analysis]]$results

        # get result and comparator values from results
        value <- get_res_value(criterion$result, results)
        comp_value <- get_res_value(criterion$comparator, results)

        if (criterion$operator == "<") {
          conclusion <- value < comp_value
        } else if (criterion$operator == ">") {
          conclusion <- value > comp_value
        } else if (criterion$operator == "=") {
          conclusion <- value == comp_value
        } else if (criterion$operator == "!=") {
          conclusion <- value != comp_value
        } else {
          conclusion <- NA
        }
        criteria[criterion$id] <- conclusion
        study$hypotheses[[i]]$criteria[[j]]$conclusion <- conclusion
      }

      # evaluate hypothesis ----
      replacement <- as.character(criteria)
      names(replacement) <- names(criteria)

      if (is.null(h$corroboration$evaluation)) {
        if (scienceverse_options("verbose")) {
          message("Hypothesis ", h$id, " has no evaluation criteria for corroboration")
        }
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
        if (scienceverse_options("verbose")) {
          message("Hypothesis ", h$id, " has no evaluation criteria for falsification")
        }
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
    }
  }

  if (scienceverse_options("verbose")) {
    message(eval_summary(study))
  }


  invisible(study)
}


#' Evaluation summary
#'
#' @param study A study list object with class scivrs_study
#'
#' @return Summary text
#' @export
#'
eval_summary <- function(study) {
  # handle nulls in sprintf
  no_null <- function(x, repl="") { ifelse(is.null(x), repl, x) }

  eval_summary <- ""

  for (h in study$hypotheses) {
    eval_summary <- sprintf("%sHypothesis %s: %s\n\n",
                            eval_summary, h$id, h$description)

    for (criterion in h$criteria) {
      results <- suppressWarnings(
        get_result(study, analysis_id = criterion$analysis_id,
                            digits = 3, return = "char")
      )

      value <- get_res_value(criterion$result, results)
      v2 <- get_res_value(criterion$comparator, results)
      comp_res <- ""
      if (v2 != criterion$comparator) {
        comp_res <- sprintf("\n* %s = %s", criterion$comparator, v2)
      }
      eval_summary <- sprintf("%sCriterion %s:\n* %s %s %s is %s\n* %s = %s%s\n\n",
                              eval_summary,
                              criterion$id,
                              criterion$result,
                              criterion$operator,
                              criterion$comparator,
                              no_null(criterion$conclusion, "unknown"),
                              no_null(criterion$result),
                              no_null(value),
                              no_null(comp_res)
      )
    }

    eval_summary <- sprintf(
      "%sConclusion: %s\n* Corroborate (%s): %s\n* Falsify (%s): %s\n\n",
      eval_summary,
      no_null(h$conclusion, "You may need to run `study_analyse()`"),
      no_null(h$corroboration$evaluation, "*no criteria*"),
      no_null(h$corroboration$result),
      no_null(h$falsification$evaluation, "*no criteria*"),
      no_null(h$falsification$result)
    )
  }

  trimws(eval_summary)
}
