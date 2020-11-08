#' Make a script with all analyses
#'
#' @param study A study list object with class scivrs_study
#' @param path Path to write script to (outputs text if NULL)
#' @param data_path Where to load data from external files (you may need to creates data files with `make_data or edit the script for your data paths); loads from the script if set to NULL
#' @param data_format The data format to save with (defaults to tsv)
#' @param show_codebook Include codebook for each dataset in comments
#' @param use_rmarkdown Creates an Rmarkdown file if TRUE, a .R file if FALSE
#' @param header_lvl The starting header level for the section (defaults to 2)
#' @param header Whether the rmarkdown version should have a header
#'
#' @return Text of the script if path is NULL
#' @export
#'
#' @examples
#' s <- study() %>%
#'   add_data("my_cars", mtcars) %>%
#'   add_analysis("A1", cor.test(my_cars$mpg, my_cars$wt)) %>%
#'   study_analyse()
#'
#' # get Rmd text output
#' make_script(s) %>% cat()
#' # get R text output
#' make_script(s, use_rmarkdown = FALSE) %>% cat()
#'
make_script <- function(study, path = NULL,
                        data_path = "data",
                        data_format = "tsv",
                        show_codebook = TRUE,
                        use_rmarkdown = TRUE,
                        header_lvl = 2,
                        header = TRUE) {

  h <- rep("#", header_lvl) %>% paste(collapse = "")

  # check if filename is .R
  if (isTRUE(grep("\\.(R|r)$", path) == 1)) use_rmarkdown = FALSE

  # data ----
  dat <- ""
  if (length(study$data) > 0) {
    if (!is.null(data_path)) {
      # write data
      make_data(study, data_path, data_format)
    }


    dat <- paste0("\n\n", h, " Data")
    for (d in study$data) {
      # . codebook ----
      cb <- ""
      if (show_codebook & "codebook" %in% names(d)) {
        clps <- ifelse(use_rmarkdown, "\n", "\n# ")
        cb <- cb_vars(d$codebook) %>%
          strsplit("\n") %>% `[[`(1) %>%
          paste(collapse = clps) %>%
          paste0(clps, ., "\n\n")
      }

      if (is.null(data_path)) {
        x <- utils::capture.output(utils::write.csv(d$data, row.names = FALSE)) %>%
          paste(collapse = "\n")
        data <- sprintf("%s <- read.csv(text='%s')", d$id, x)
      } else {
        sep <- ifelse(data_format == "tsv", "\\t", ",")
        data <- sprintf("%s <- read.csv('%s/%s_data.%s', sep='%s')",
                        d$id, data_path, d$id, data_format, sep)
      }

      fmt <- ifelse(use_rmarkdown,
                    "%s\n\n%s# %s\n\n%s\n```{r}\n%s\n```",
                    "%s\n\n%s# %s\n\n%s%s")
      dat <- sprintf(fmt, dat, h, d$id, cb, data)
    }
    dat <- sprintf("%s\n\n", dat)
  }


  # analyses ----
  ana <- ""
  if (length(study$analyses) > 0) {
    for (a in study$analyses) {
      code <- output_custom_code(study, a$id, "")
      idx <- get_idx(study, a$id, "analyses")
      if(use_rmarkdown) {
        fmt <- "%s%s Analysis %d: %s {#analysis_%d}\n\n```{r}\n%s\n```\n\n"
        ana <- sprintf(fmt, ana, h, idx, a$id, idx, code)
      } else {
        fmt <- "%s%s Analysis %d: %s\n\n%s\n\n"
        ana <- sprintf(fmt, ana, h, idx, a$id, code)
      }

      # results ----
      if ("results" %in% names(a)) {
        prefix <- ifelse(use_rmarkdown, "", "# ")
        quote <- ifelse(use_rmarkdown, "`", "")
        res <- nested_list(a$results, prefix, quote)
        ana <- sprintf("%s%s# Stored Results\n\n%s\n\n",
                       ana, h, res)
      }
    }
  }

  # preamble ----
  date <- format(Sys.Date(), "%d/%m/%Y")
  authors <- sapply(study$authors, function(x) paste(x$name$given, x$name$surname)) %>%
    paste(collapse = ", ")
  if (use_rmarkdown) {
    preamble <- sprintf('---\ntitle: "%s"\nauthor: "%s"\ndate: "%s"\noutput:\n  html_document:\n    toc: true\n    toc_float: true\n---',
                        study$name, authors, date )
  } else {
    preamble <- sprintf("# Code for %s\n# Authors: %s\n# Created %s",
                        study$name, authors, date)
  }

  if (!header) preamble <- ""
  txt <- sprintf("%s\n\n%s\n\n%s", preamble, dat,  ana)

  if (is.null(path)) return(txt)

  # write txt to file
  suffix <- ifelse(use_rmarkdown, ".Rmd", ".R")
  path <- sub("(\\.(R|Rmd))?$", suffix, path, ignore.case = TRUE)
  write(txt, path)
}



#' List codebook variables
#'
#' @param cb The codebook
#'
#' @return named list of all the variable descriptions
#'
cb_vars <- function(cb) {
  vars <- list()
  for (v in cb$variableMeasured) {
    desc <- ifelse(v$name == v$description,
                   "", paste(":", v$description))
    extras <- ""

    # has levels ----
    if (length(v$levels) > 0) {
      if (is.null(names(v$levels))) {
        lvls <- v$levels
      } else if (all(names(v$levels) == v$levels)) {
        lvls <- v$levels
      } else {
        lvls <- paste0(names(v$levels), ": ", v$levels)
      }

      extras <- sprintf(
        "\n  * Levels\n    * %s\n  * Ordered: %s",
        paste(lvls, collapse = "\n    * "),
        ifelse(is.null(v$levelsOrdered), FALSE, v$levelsOrdered)
      )
    }

    vars[v$name] = sprintf(
      "* %s (%s)%s%s",
      v$name, v$dataType, desc, extras
    )
  }

  paste(vars, collapse = "\n")
}
