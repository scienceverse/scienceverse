#' Save study
#'
#' Save the study framework to a JSON file
#'
#' @param study A study list object with class scivrs_study
#' @param filename The name to save the file if NULL, will save as the study name
#' @param format Save as a machine-readable "json" file, a human-readable "prereg" document (no results) or a "postreg" document (includes results)
#' @param data_values Whether to include data values in the JSON file (defaults to TRUE)
#' @return A study object with class scivrs_study
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' study("iris") %>%
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
#'   study_save(format = "postreg")
#' }
#'
study_save <- function(study,
                       filename = NULL,
                       format = c("json", "prereg", "postreg"),
                       data_values = TRUE) {
  format <- match.arg(format)

  if (!"scivrs_study" %in% class(study)) {
    if (is.list(study)) {
      studies <- lapply(study, study_save, format = format, data_values = data_values)
      invisible(studies)
    } else {
      stop("The study argument needs to be a scivrs_study object or a list of them.")
    }
  }

  if (is.null(filename)) filename <- gsub("\\s+", "_", tolower(study$name))

  if (format == "json") {
    if (!length(grep("\\.json$", filename))) {
      # add .json extension if not already specified
      filename <- paste0(filename, ".json")
    }
    if (substr(filename, 1, 1) != "/") {
      filename <- paste0(getwd(), "/", filename)
    }
    message("Saving to ", filename)

    study_to_json(study, data_values) %>%
      writeLines(filename)
  } else {
    # save as report

    if (!length(grep("\\.html$", filename))) {
      # add .html extension if not already specified
      filename <- paste0(filename, ".html")
    }
    if (substr(filename, 1, 1) != "/") {
      filename <- paste0(getwd(), "/", filename)
    }
    message("Saving to ", filename)

    if (format == "prereg") {
      template <- system.file("rmarkdown", "prereg.Rmd", package = "scienceverse")
    } else if (format == "postreg") {
      template <- system.file("rmarkdown", "postreg.Rmd", package = "scienceverse")
    }
    options(knitr.duplicate.label = 'allow')
    rmarkdown::render(template,
                      output_file = filename,
                      intermediates_dir = tempdir(),
                      quiet = TRUE,
                      envir = new.env(),
                      encoding = "UTF-8")
  }

  invisible(study)
}

