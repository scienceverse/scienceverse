#' Generate a Report
#'
#' Generate a study report
#'
#' @param study A study list object with class reg_study
#' @param template The type of report c("prereg", "postreg") or a path to a custom template
#' @param filename The file path to save to
#' @return A study object with class reg_study
#' @examples
#'
#' \dontrun{
#' study() %>%
#'   add_hypothesis("H1", "Petal width and length will be positively correlated.") %>%
#'   add_analysis("A1", cor.test(dat$Petal.Width, dat$Petal.Length)) %>%
#'   add_criterion("sig", "p.value", "<", 0.05) %>%
#'   add_criterion("pos", "estimate", ">", 0) %>%
#'   add_eval("corroboration",
#'            "Petal width is significantly and positively correlated to length",
#'            "sig & pos") %>%
#'   add_eval("falsification",
#'            "Petal width is significantly and negatively correlated to length",
#'            "sig & !pos") %>%
#'   add_data("dat", iris) %>%
#'   study_analyse() %>%
#'   study_report(template = "postreg")
#' }
#'
#' @export
#'
study_report <- function(study, template = "prereg",
                         filename = "study.html") {
  if (!length(grep("\\.html$", filename))) {
    # add .html extension if not already specified
    filename <- paste0(filename, ".html")
  }
  if (substr(filename, 1, 1) != "/") {
    filename <- paste0(getwd(), "/", filename)
  }
  message("Saving to ", filename)

  if (template == "prereg") {
    template <- system.file("rmarkdown", "prereg.Rmd", package = "scienceverse")
  } else if (template == "postreg") {
    template <- system.file("rmarkdown", "postreg.Rmd", package = "scienceverse")
  }
  options(knitr.duplicate.label = 'allow')
  rmarkdown::render(template,
                    output_file = filename,
                    quiet = TRUE,
                    envir = new.env(),
                    encoding = "UTF-8")
  invisible(study)
}
