#' Add Data
#'
#' Add a dataset to a study object
#'
#' @param study A study list object with class scivrs_study
#' @param id The id for this dataset (index or character) if a dataset with this id already exists, it will overwrite it
#' @param data The dataset as a data.frame, codebook object, path to a data file, or path to a codebook file
#' @param vardesc Optional variable properties (see `faux::codebook`)
#' @param design A faux design specification
#' @param ... Further dataset properties (see `faux::codebook`)
#'
#' @return A study object with class scivrs_study
#' @examples
#'
#' s <- study() %>% add_data("dat", iris)
#' study_to_json(s)
#'
#' @export
#'
add_data <- function(study, id, data = NULL,
                     vardesc = list(), design = NULL, ...) {
  id <- fix_id(id)
  d <- list(id = id)

  if (is.character(data)) {
    if (!file.exists(data)) {
      warning("The file ", data, " does not exist.")
      return(invisible(study))
    }

    accepted_ext <- c("csv", "xls", "xlsx", "txt", "tsv", "sav")

    filename <- data
    ext <- strsplit(basename(filename), split="\\.")[[1]][-1]
    if (ext == "json") {
      json <- jsonlite::read_json(filename)
      d <- c(d, json)
    } else if (ext %in% accepted_ext) {
      data <- rio::import(filename)
    } else {
      warning("The ", ext, " format is not supported.\nPlease add data in one of the following formats: ", paste(accepted_ext, collapse = ", "))
    }
  }

  vm <- list()
  if (is.data.frame(data)) {
    # create codebook

    if (all(names(data)[1:2] == c("rep", "data"))) {
      subdata <- data$data[[1]]
      attr(subdata, "design") <- attr(data, "design")
      cb <- faux::codebook(data = subdata, name = id,
                           vardesc = vardesc, ...,
                           return = "list")
    } else {
      cb <- faux::codebook(data = data, name = id,
                   vardesc = vardesc, ...,
                   return = "list")
    }
    d$codebook <- cb
    d$data <- data
  }

  if (!is.null(design)) d$design <- design

  class(d) <- c("scivrs_data", "list")

  idx <- get_idx(study, id, "data")

  study$data[[idx]] <- d

  invisible(study)
}
