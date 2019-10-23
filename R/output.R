#' Output custom code
#'
#' Output custom code specified in an analysis component
#'
#' @param study A study list object with class reg_study
#' @param analysis_id The id or index for the analysis code to output (defaults to index 1)
#'
#' @return string of the function definition
#' @export
#'
#' @examples
#'
#' custom <- function() { 1:10 }
#' s <- study() %>%
#'   add_analysis("custom") %>%
#'   output_custom_code()
output_custom_code <- function(study, analysis_id = 1) {
  analysis <- study$analyses[[analysis_id]]

  if (is.function(analysis$code)) {
    analysis$code <- analysis$code %>%
      jsonlite::toJSON() %>%
      jsonlite::fromJSON() %>%
      as.list()
  }

  if (is.list(analysis$code)) {
    paste(
      analysis$func, "<-",
      paste(analysis$code, collapse= "\n")
    )
  } else if (is.null(analysis$code)) {
    analysis$func
  }
}



#' Output hypotheses
#'
#' Output hypotheses specified in the json file
#'
#' @param study A study list object with class reg_study
#' @return The study object
#'
#' @export

output_hypotheses <- function(study) {
  cat("## Hypotheses\n\n")

  for (i in 1:length(study$hypotheses)) {

    cat("### Hypothesis ", i,
        ": ", study$hypotheses[[i]]$id, "\n\n",
        study$hypotheses[[i]]$description, "\n\n", sep = "")

    criteria <- study$hypotheses[[i]]$criteria

    for (j in 1:length(criteria)) {
      cat("* Criterion", j, "is confirmed if analysis",
          criteria[[j]]$analysis_id, "yields",
          criteria[[j]]$result,
          criteria[[j]]$operator,
          criteria[[j]]$comparator,
          "  \n"
      )
    }

    cat("\n")

    # explain evaluation
    eval <- study$hypotheses[[i]]$evaluation
    if (eval %in% c("&", "and")) {
      cat("If all criteria are met, this hypothesis is supported.")
    } else if (eval %in% c("|", "or")) {
      cat("If any criteria are met, this hypothesis is supported.")
    } else {
      cat(eval)
    }

    cat("\n\n\n")
  }

  invisible(study)
}


#' Output results
#'
#' Output results specified in the json file
#'
#' @param study A study list object with class reg_study
#' @param digits integer indicating the number of decimal places.
#' @return The study object
#'
#' @export

output_results <- function(study, digits = 3) {
  cat("## Results\n\n")
  for (i in 1:length(study$hypotheses)) {

    cat("### Hypothesis ", i, "\n\n", study$hypotheses[[i]]$desc, "\n\n", sep = "")

    criteria <- study$hypotheses[[i]]$criteria

    for (j in 1:length(criteria)) {
      analysis_ids <- sapply(study$analyses, function(x) {x$id})
      analysis <- match(criteria[[j]]$analysis_id, analysis_ids)
      result <- study$analyses[[analysis]]$results[[criteria[[j]]$result]]

      cat("* Criterion ", j, " was ",
          criteria[[j]]$result, " ",
          criteria[[j]]$operator, " ",
          criteria[[j]]$comparator,
          " in analysis ", criteria[[j]]$analysis_id, ".  \n    The result was ",
          criteria[[j]]$result, " = ", round_char(result, digits),
          "  \n",
          sep = ""
      )
    }

    cat("\n**Conclusion**: ")
    eval <- study$hypotheses[[i]]$evaluation
    conclusion <- study$hypotheses[[i]]$conclusion
    if (eval %in% c("&", "and")) {
      if (conclusion) {
        cat("All criteria were met, this hypothesis was supported.")
      } else {
        cat("All criteria were not met, this hypothesis was not supported.")
      }
    } else if (eval %in% c("|", "or")) {
      if (conclusion) {
        cat("At least one criterion was met, this hypothesis was supported.")
      } else {
        cat("No criteria were met, this hypothesis was not supported.")
      }
    } else {
      cat("The evaluation criteria could not be automatically evaluated.")
    }

    cat("\n\n")
  }

  invisible(study)
}


#' Output analyses
#'
#' Output analysis plan specified in the json file
#'
#' @param study A study list object with class reg_study
#' @return The study object
#'
#' @export

output_analyses <- function(study) {
  cat("## Analyses\n\n")

  for (i in 1:length(study$analyses)) {
    cat("### Analysis ", i, ": ", study$analyses[[i]]$id, "\n\n")

    func <- study$analyses[[i]]$func
    params <- study$analyses[[i]]$params

    keys <- names(params)
    vals <- unlist(params) %>% unname()
    x <- c()
    for (j in 1:length(keys)) {
      x[j] <- paste0(keys[j], " = ", vals[j])
    }

    cat("We will run `",
        func, "(", paste0(x, collapse = ", "), ")`\n\n",
        sep = "")

    output_custom_code(study, i) %>%
      paste("<code><pre>", ., "</pre></code>\n\n") %>%
      cat()
  }

  invisible(study)
}
