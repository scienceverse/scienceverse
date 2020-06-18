#' Generate a Report
#'
#' Generate a study report (deprecated, use study_save)
#'
#' @param study A study list object with class scivrs_study
#' @param template The type of report c("prereg", "postreg") or a path to a custom template
#' @param filename The file path to save to
#' @return A study object with class scivrs_study
#'
#' @export
#'
study_report <- function(study, template = "prereg",
                         filename = "study.html") {
  study_save(study, format = template, filename = filename)
}
