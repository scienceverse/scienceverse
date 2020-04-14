#' Add Hypothesis
#'
#' Add a hypothesis to a study object
#'
#' @param study A study list object with class reg_study
#' @param id The id for this hypothesis (index or character) if a hypothesis with this id already exists, it will overwrite it
#' @param description The text description of the hypothesis
#' @return A study object with class reg_study
#' @examples
#'
#' s <- study() %>%
#'   add_hypothesis("H1", "Petal width and length will be positively correlated.")
#' study_to_json(s)
#'
#' @export
#'
add_hypothesis <- function(study, id = NULL,
                           description = "Describe your hypothesis") {
  idx <- get_idx(study, id, "hypotheses")
  id <- ifelse(is.null(id), idx , fix_id(id))

  hypothesis <- list(
    id = id,
    description = description,
    criteria = list(),
    corroboration = list(),
    falsification = list()
  )

  class(hypothesis) <- c("reg_study_hypothesis", "list")

  study$hypotheses[[idx]] <- hypothesis

  invisible(study)
}
