#' Add Evaluation
#'
#' Add evalution criterion to a hypothesis in a study object
#'
#' @param study A study list object with class scivrs_study
#' @param type "corroboration" or "falsification"
#' @param description A verbal description of the conditions for corroborating the hypothesis
#' @param evaluation A logical representation of these conditions using the criteria IDs, parentheses, &, | and ! (e.g., "(c1 & c2) | (c3 & !c4)")
#' @param hypothesis_id The id for the hypothesis (index or character) if NULL, assigns to the last hypothesis in the list
#' @return A study object with class scivrs_study
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
#'            "sig & !pos")
#' study_to_json(s)
#'
#' @export
#'
add_eval <- function(study, type, description, evaluation,
                     hypothesis_id = NULL) {
  # get ids and indices
  hypothesis <- get_id_idx(study, hypothesis_id, "hypotheses")

  # check all referenced criteria exist
  criteria_refs <- gsub("\\s", "", evaluation) %>%
    strsplit("(\\(|\\)|\\||\\&|\\!)+")
  # remove empty refs - ugh
  criteria_refs <- criteria_refs[[1]][criteria_refs[[1]]!=""]

  criteria_ids <- c()
  for (criterion in study$hypotheses[[hypothesis$idx]]$criteria) {
    criteria_ids <- c(criteria_ids, criterion$id)
  }

  criteria_missing <- setdiff(criteria_refs, criteria_ids)
  if (length(criteria_missing) > 0) {
    if (scienceverse_options("verbose")) {
      warning("Criteria ", paste(criteria_missing, collapse = ", "),
              " have not been defined yet.")
    }
  }

  # add evaluation to hypothesis
  study$hypotheses[[hypothesis$idx]][[type]] <- list(
    description = description,
    evaluation = evaluation
  )

  invisible(study)
}
