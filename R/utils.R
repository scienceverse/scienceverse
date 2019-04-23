#' Get index from id
#'
#' Get the index from id for an item in the hypotheses, analyses, or data sections of a study object
#'
#' @param study A study list object with class reg_study
#' @param id The id for the section (index or character) if NULL, assigns to the last item in the list
#' @param section The section to search, c("hypotheses", "analyses", "data")
#' @return A numeric index
#'
get_idx <- function(study, id = NULL, section = "hypotheses") {
  n <- length(study[[section]])
  idx <- n + 1
  if (length(grep("^\\d+$", id))) { # is an integer
    if (n >= id) idx <- as.numeric(id)
  } else if (is.character(id) & n > 0) {
    for (i in 1:n) {
      if (study[[section]][[i]]$id == id) idx <- i
    }
  }

  return(idx)
}


#' Get index and id
#'
#' Get the index and id from an item in the hypotheses, analyses, or data sections of a study object
#'
#' @param study A study list object with class reg_study
#' @param id The id for the section (index or character) if NULL, assigns to the last item in the list
#' @param section The section to search, c("hypotheses", "analyses", "data")
#' @return A list of id and idx
#'
get_id_idx <- function(study, id = NULL, section = "hypotheses") {
  if (length(study[[section]]) == 0) {
    if (length(grep("^\\d+$", id))) id <- 1
    warning("No ", section, " items exist. ",
            "Creating a default item with id = ",
            ifelse(is.null(id), "NULL", id))

    if (section == "hypotheses") study <- add_hypothesis(study, id=id)
    if (section == "analyses") study <- add_analysis(study, id=id)
    if (section == "data") study <- add_data(study, id=id)
  }
  n <- length(study[[section]])
  idx <- n # default to the last one

  if (is.null(id)) {
    # set to last item
    last_id <- study[[section]][[n]]$id
    id <- ifelse(is.null(last_id), n, last_id)
  } else if (length(grep("^\\d+$", id))) {
    if (id > n) {
      warning("No ", section, " item with index = ", id,
              " exists. Creating a default item at index = ",
              n+1)

      if (section == "hypotheses") study <- add_hypothesis(study, id=id)
      if (section == "analyses") study <- add_analysis(study, id=id)
      if (section == "data") study <- add_data(study, id=id)

      idx <- length(study[[section]])
    } else {
      idx <- as.numeric(id)
      id <- study[[section]][[idx]]$id
    }
  } else if (is.character(id)) {
    # find existing item with this id
    item_exists <- FALSE
    for (i in 1:n) {
      if (study[[section]][[i]]$id == id) idx <- i
      item_exists <- TRUE
    }
    if (!item_exists) {
      warning("No ", section, " item with index = ", id,
              " exists. Creating a default item with at index = ",
              n+1)

      if (section == "hypotheses") study <- add_hypothesis(study, id=id)
      if (section == "analyses") study <- add_analysis(study, id=id)
      if (section == "data") study <- add_data(study, id=id)

      idx <- length(study[[section]])
    }
  }

  list(id = id, idx = idx)
}

#' Character-safe rounding
#'
#' Round a vector if it is numeric, but return the original vector if it is character.
#'
#' @param x	a character vector.
#' @param digits integer indicating the number of decimal places.
#' @param ...	arguments to be passed to methods.
#' @return The character vector or the rounded version if numeric.
#'
round_char <- function(x, digits = 0, ...) {
  num_x <- suppressWarnings(as.numeric(x))
  if (is.na(num_x)) return(x)
  round(num_x, digits, ...)
}

