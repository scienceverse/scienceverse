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
#'   add_analysis("custom", t.test(rnorm(100))) %>%
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

    cat("#### Criteria\n\n")
    criteria <- study$hypotheses[[i]]$criteria

    for (j in 1:length(criteria)) {
      cat("* `", criteria[[j]]$id, "` is confirmed if analysis `",
          criteria[[j]]$analysis_id, "` yields `",
          criteria[[j]]$result,
          criteria[[j]]$operator,
          criteria[[j]]$comparator,
          "`  \n",
          sep = ""
      )
    }

    cat("\n")

    # explain evaluation
    cat("#### Evaluation\n\n")
    cat("##### Corroboration\n\n")
    cat(study$hypotheses[[i]]$corroboration$description, "\n\n")
    cat("```\n", study$hypotheses[[i]]$corroboration$evaluation, "\n```\n\n")
    cat("##### Falsification\n\n")
    cat(study$hypotheses[[i]]$falsification$description, "\n\n")
    cat("```\n", study$hypotheses[[i]]$falsification$evaluation, "\n```\n\n")
    cat("All other patterns of results are inconclusive.")

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
    h <- study$hypotheses[[i]]

    cat("### Hypothesis ", i, ": ", h$id, "\n\n", h$desc, "\n\n", sep = "")

    criteria <- h$criteria
    analysis_ids <- sapply(study$analyses, function(x) {x$id})

    for (j in 1:length(criteria)) {
      criterion <- h$criteria[[j]]
      analysis <- match(criterion$analysis_id, analysis_ids)

      # get value, handle indices in result
      splitres <- stringr::str_split(criterion$result, "(\\[|\\])")
      res <- splitres[[1]][1]
      idx <- as.integer(splitres[[1]][2])
      idx <- ifelse(isTRUE(idx > 0), idx, 1)
      value <- study$analyses[[analysis]]$results[[res]][idx]

      conc_color <- ifelse(criterion$conclusion, "green", "red")

      cat("* `", criteria[[j]]$id, "` is confirmed if analysis [",
          criteria[[j]]$analysis_id, "](#", criteria[[j]]$analysis_id,
          ") yields `",
          criteria[[j]]$result,
          criteria[[j]]$operator,
          criteria[[j]]$comparator,
          "`    \nThe result was ",
          criterion$result, " = ", round_char(value, digits),
          " (<span style=\"color:", conc_color, "\">",
          criterion$conclusion, "</span>)  \n",
          sep = ""
      )
    }
    cat("\n\n")

    # explain evaluation
    cc <- h$corroboration
    ff <- h$falsification
    conc_color <- ifelse(cc$result, "green", "red")
    cat("#### Corroboration (<span style=\"color:", conc_color, "\">", cc$result, "</span>)\n\n")
    cat(cc$description, "\n\n")
    cat("```\n", cc$evaluation, "\n```\n\n")

    conc_color <- ifelse(ff$result, "green", "red")
    cat("#### Falsification (<span style=\"color:", conc_color, "\">", ff$result, "</span>)\n\n")
    cat(ff$description, "\n\n")
    cat("```\n", ff$evaluation, "\n```\n\n")

    # conclusion
    if (h$conclusion == "corroborate") {
      cat("<span style=\"color: green\">")
      cat("**All criteria were met for corroboration.**")
    } else if (h$conclusion == "falsify") {
      cat("<span style=\"color: red\">")
      cat("**All criteria were met for falsification.**")
    } else {
      cat("<span style=\"color: blue\">")
      cat("**Neither the corroboration nor falsification criteria were met.**")
      cat("</span>")
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
    cat("### Analysis ", i, ": ", study$analyses[[i]]$id,
        " {#", study$analyses[[i]]$id, "}\n\n", sep = "")

    output_custom_code(study, i) %>%
      paste("<code><pre>", ., "</pre></code>\n\n") %>%
      cat()
  }

  invisible(study)
}
