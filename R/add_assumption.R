#' Add Assumption
#'
#' Add an auxillary assumption
#'
#' @param study A study list object with class scivrs_study
#' @param id The id for this assumption (index or character) if an assumption with this id already exists, it will overwrite it
#' @param description The text description of the assumption
#' @param value Whether the assumption is TRUE, FALSE or NULL (unknown)
#' @param hypothesis_id A vector of hypothesis IDs that rely on this assumption
#' @return A study object with class scivrs_study
#' @examples
#'
#' s <- study() %>%
#'   add_hypothesis("H1", "Speed and distance will be positively correlated") %>%
#'   add_assumption("normal_dist", "Distance will be normally distributed", "H1")
#'
#' @export
#'
add_assumption <- function(study, id = NULL, description = "",
                           value = NULL, hypothesis_id = NULL) {
    add_hypothesis(study, id, description)
}
