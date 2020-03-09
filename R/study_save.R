#' Save study
#'
#' Save the study framework to a JSON file
#'
#' @param study A study list object with class reg_study
#' @param filename The name to save the file
#' @param data_values Whether to include data values in the JSON file (defaults to TRUE)
#' @return A study object with class reg_study
#'
#' @export
#'
study_save <- function(study,
                       filename = "study.json",
                       data_values = TRUE) {

  if (!length(grep("\\.json$", filename))) {
    # add .json extension if not already specified
    filename <- paste0(filename, ".json")
  }

  study_json(study, data_values) %>%
    writeLines(filename)

  invisible(study)
}
