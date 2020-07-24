#' Add an evaluation
#'
#' Add an evaluation criterion to a hypothesis in a study object
#'
#' @param study A study list object with class scivrs_study
#' @param type "corroboration" or "falsification" (or "c"/"f")
#' @param evaluation A logical representation of these conditions using the criteria IDs, parentheses, &, | and ! (e.g., "(c1 & c2) | (c3 & !c4)")
#' @param description A verbal description of the conditions for corroborating the hypothesis
#' @param hypothesis_id The id for the hypothesis (index or character) if NULL, assigns to the last hypothesis in the list
#' @return A study object with class scivrs_study
#' @examples
#'
#' s <- study() %>%
#'   add_hypothesis("H1", "Petal width and length will be positively correlated.") %>%
#'   add_analysis("A1", cor.test(dat$Petal.Width, dat$Petal.Length)) %>%
#'   add_criterion("sig", "p.value", "<", 0.05) %>%
#'   add_criterion("pos", "estimate", ">", 0) %>%
#'   add_eval("corroboration", "sig & pos",
#'            "Petal width is significantly and positively correlated to length"
#'            ) %>%
#'   add_eval("falsification", "sig & !pos",
#'            "Petal width is significantly and negatively correlated to length"
#'            )
#' s
#' @export
#'
add_eval <- function(study, type, evaluation, description= "",
                     hypothesis_id = NULL) {
  # get ids and indices
  hypothesis <- get_id_idx(study, hypothesis_id, "hypotheses")

  # check all referenced criteria exist
  criteria_refs <- evaluation %>%
    strsplit("(\\(|\\)|\\||\\&|\\!|\\s)+")
  # remove empty refs - ugh
  criteria_refs <- criteria_refs[[1]][criteria_refs[[1]]!=""]

  criteria_ids <- c()
  for (criterion in study$hypotheses[[hypothesis$idx]]$criteria) {
    criteria_ids <- c(criteria_ids, criterion$id)
  }

  criteria_missing <- setdiff(criteria_refs, criteria_ids)
  if (length(criteria_missing) > 0) {
    if (scienceverse_options("verbose")) {
      warning("Criteria `", paste(criteria_missing, collapse = "`, `"),
              "` have not been defined yet.")
    }
  }

  # check type
  type_f <- substr(type, 1, 1) %>% tolower()
  if (!(type_f %in% c("c", "f"))) {
    stop("The type must be one of 'corroboration' or 'falsification' (or 'c'/'f')")
  }
  checked_type <- ifelse(type_f == "c", "corroboration", "falsification")

  # add evaluation to hypothesis
  study$hypotheses[[hypothesis$idx]][[checked_type]] <- list(
    description = description,
    evaluation = evaluation
  )

  invisible(study)
}
