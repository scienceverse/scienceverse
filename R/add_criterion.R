#' Add a criterion
#'
#' Add a criterion to a hypothesis in a study object
#'
#' @param study A study list object with class scivrs_study
#' @param id A unique name to refer to the criterion with in the evaluation
#' @param result The name of the item in the analysis results list to compare
#' @param operator The operator for comparison c("<", "=", ">", "!=")
#' @param comparator The value to compare
#' @param analysis_id The id for the relevant analysis (index or character) if NULL, assigns to the last analysis in the list
#' @param hypothesis_id The id for the hypothesis (index or character) if NULL, assigns to the last hypothesis in the list
#'
#' @return A study object with class scivrs_study
#' @examples
#'
#' s <- study() %>%
#'   add_hypothesis("H1", "Petal width and length will be positively correlated.") %>%
#'   add_analysis("A1", cor.test(dat$Petal.Width, dat$Petal.Length)) %>%
#'   add_criterion("sig", "p.value", "<", 0.05) %>%
#'   add_criterion("pos", "estimate", ">", 0)
#'
#' @export
#'
add_criterion <- function(study, id, result, operator, comparator,
                          analysis_id = NULL, hypothesis_id = NULL) {
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

  class(criterion) <- c("scivrs_hypothesis_criterion", "list")

  # add criterion to hypothesis
  crit_ids <- sapply(study$hypotheses[[hypothesis$idx]]$criteria, `[[`, "id")
  if (criterion$id %in% crit_ids) {
    crit_idx <- which(criterion$id == crit_ids)
  } else {
    crit_idx <- length(crit_ids) + 1
  }
  study$hypotheses[[hypothesis$idx]]$criteria[[crit_idx]] <- criterion

  invisible(study)
}

#' Update a criterion
#'
#' @param study A study list object with class scivrs_study
#' @param id A unique name to refer to the criterion with in the evaluation
#' @param result The name of the item in the analysis results list to compare
#' @param operator The operator for comparison c("<", "=", ">", "!=")
#' @param comparator The value to compare
#' @param analysis_id The id for the relevant analysis (index or character) if NULL, assigns to the last analysis in the list
#' @param hypothesis_id The id for the hypothesis (index or character) if NULL, assigns to the last hypothesis in the list
#' @param new_id A new (character) ID for this criterion
#'
#' @return A study object with class scivrs_study
#' @export
#'
#' @examples
#' s <- study() %>%
#'   add_hypothesis("H1") %>%
#'   add_analysis("A1", t.test(rnorm(100))) %>%
#'   add_criterion("p", "p.value", ">", 0.05) %>%
#'   study_analyse()
#'
#' s <- update_criterion(s, "p", operator = "<",
#'                       analysis_id = "A1"
#'                       hypothesis_id = "H1" ) %>%
#'      study_analyse()
#'
update_criterion <- function(study, id,
                             result = NULL,
                             operator = NULL,
                             comparator = NULL,
                             analysis_id = NULL,
                             hypothesis_id = NULL,
                             new_id = NULL) {
  hypothesis <- get_id_idx(study, hypothesis_id, "hypotheses")
  analysis <- get_id_idx(study, analysis_id, "analyses")
  if (analysis$idx > length(study$analyses)) stop("The study does not have an analysis with the ID ", analysis_id)
  if (hypothesis$idx > length(study$hypotheses)) stop("The study does not have a hypothesis with the ID ", hypothesis_id)
  criteria <- study$hypotheses[[hypothesis$idx]]$criteria

  crit_ids <- sapply(criteria, `[[`, "id")

  if (!id %in% crit_ids) stop("The study does not have a criterion with the ID ", id)

  idx <- which(id == crit_ids)

  if (!is.null(new_id))
    study$hypotheses[[hypothesis$idx]]$criteria[[idx]]$id <- fix_id(new_id)

  if (!is.null(result))
    study$hypotheses[[hypothesis$idx]]$criteria[[idx]]$result <- result

  if (!is.null(operator))
    study$hypotheses[[hypothesis$idx]]$criteria[[idx]]$operator <- operator

  if (!is.null(comparator))
    study$hypotheses[[hypothesis$idx]]$criteria[[idx]]$comparator <- comparator

  invisible(study)
}
