#' Add a hypothesis
#'
#' Add a hypothesis to a study object
#'
#' @param study A study list object with class scivrs_study
#' @param id The id for this hypothesis (index or character) if a hypothesis with this id already exists, it will overwrite it
#' @param description The text description of the hypothesis
#' @param ... further arguments to add
#'
#' @return A study object with class scivrs_study
#' @examples
#'
#' s <- study() %>%
#'   add_hypothesis("H1", "Petal width and length will be positively correlated.")
#' study_to_json(s)
#'
#' @export
#'
add_hypothesis <- function(study, id = NULL,
                           description = "Describe your hypothesis", ...) {
  idx <- get_idx(study, id, "hypotheses")
  id <- ifelse(is.null(id), idx , fix_id(id))

  hypothesis <- list(
    id = id,
    description = description,
    criteria = list(),
    corroboration = list(),
    falsification = list()
  )

  hypothesis <- c(hypothesis, list(...))

  class(hypothesis) <- c("scivrs_hypothesis", "list")

  study$hypotheses[[idx]] <- hypothesis

  invisible(study)
}



#' Update a hypothesis
#'
#' @param study A study list object with class scivrs_study
#' @param id The id for this hypothesis (index or character)
#' @param description The text description of the hypothesis
#' @param new_id A new (character) ID for this hypothesis
#'
#' @return A study object with class scivrs_study
#' @export
#'
#' @examples
#' s <- study() %>% add_hypothesis("H1", "My hypothesis")
#' s <- update_hypothesis(s, 1, "Better description", "H1a")
#' s
update_hypothesis <- function(study, id = 1, description = NULL, new_id = NULL) {
  idx <- get_idx(study, id, "hypotheses")

  if (idx > length(study$hypotheses)) stop("The study does not have a hypothesis with the ID ", id)

  if (!is.null(new_id))
    study$hypotheses[[idx]]$id <- fix_id(new_id)

  if (!is.null(description))
    study$hypotheses[[idx]]$description <- description

  invisible(study)
}
