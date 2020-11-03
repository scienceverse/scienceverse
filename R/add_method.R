#' Add a method
#'
#' Add a method to a study object
#'
#' @param study A study list object with class scivrs_study
#' @param id The id for this method (index or character) if a method with this id already exists, it will overwrite it
#' @param description The text description of the method
#' @param ... further arguments to add
#'
#' @return A study object with class scivrs_study
#' @examples
#'
#' s <- study() %>%
#'   add_method("M1", "Description...")
#' study_to_json(s)
#'
#' @export
#'
add_method <- function(study, id = NULL,
                       description = "Describe your method", ...) {
  idx <- get_idx(study, id, "methods")
  id <- ifelse(is.null(id), idx , fix_id(id))

  method <- list(
    id = id,
    description = description
  )

  method <- c(method, list(...))

  class(method) <- c("scivrs_method", "list")

  study$methods[[idx]] <- method

  invisible(study)
}



#' Update a method
#'
#' @param study A study list object with class scivrs_study
#' @param id The id for this method (index or character)
#' @param description The text description of the method
#' @param new_id A new (character) ID for this method
#'
#' @return A study object with class scivrs_study
#' @export
#'
#' @examples
#' s <- study() %>% add_method("M1", "My method")
#' s <- update_method(s, 1, "Better description", "M1a")
#' s
update_method <- function(study, id = 1, description = NULL, new_id = NULL) {
  idx <- get_idx(study, id, "methods")

  if (idx > length(study$methods)) stop("The study does not have a method with the ID ", id)

  if (!is.null(new_id))
    study$methods[[idx]]$id <- fix_id(new_id)

  if (!is.null(description))
    study$methods[[idx]]$description <- description

  invisible(study)
}
