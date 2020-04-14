#' Add Criterion
#'
#' Add a criterion to a hypothesis in a study object
#'
#' @param study A study list object with class reg_study
#' @param id A unique name to refere to the criterion with in the evaluation
#' @param result The name of the item in the analysis results list to compare
#' @param operator The operator for comparison c("<", "=", ">", "!=")
#' @param comparator The value to compare
#' @param hypothesis_id The id for the hypothesis (index or character) if NULL, assigns to the last hypothesis in the list
#' @param analysis_id The id for the relevant analysis (index or character) if NULL, assigns to the last analysis in the list
#'
#' @return A study object with class reg_study
#' @examples
#'
#' s <- study() %>%
#'   add_hypothesis("H1", "Petal width and length will be positively correlated.") %>%
#'   add_analysis("A1", cor.test(dat$Petal.Width, dat$Petal.Length)) %>%
#'   add_criterion("sig", "p.value", "<", 0.05) %>%
#'   add_criterion("pos", "estimate", ">", 0)
#' study_to_json(s)
#'
#' @export
#'
add_criterion <- function(study, id, result, operator, comparator,
                          hypothesis_id = NULL,
                          analysis_id = NULL) {
  # get ids and indices
  hypothesis <- get_id_idx(study, hypothesis_id, "hypotheses")
  analysis <- get_id_idx(study, analysis_id, "analyses")

  # set up criterion structure
  criterion <- list(
    id = fix_id(id),
    analysis_id = analysis$id,
    result = result,
    operator = operator,
    comparator = comparator
  )

  class(criterion) <- c("reg_study_hypothesis_criterion", "list")

  # add criterion to hypothesis
  crit_idx <- length(study$hypotheses[[hypothesis$idx]]$criteria) + 1
  study$hypotheses[[hypothesis$idx]]$criteria[[crit_idx]] <- criterion

  invisible(study)
}
