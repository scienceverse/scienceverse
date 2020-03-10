#' Add Data
#'
#' Add a dataset to a study object
#'
#' @param study A study list object with class reg_study
#' @param id The id for this dataset (index or character) if a dataset with this id already exists, it will overwrite it
#' @param data The dataset as a data.frame, codebook object, path to a data file, or path to a codebook file
#' @param coldesc Optional named list of column descriptions
#'
#' @return A study object with class reg_study
#' @examples
#'
#' s <- study() %>% add_data("dat", iris)
#' study_json(s)
#'
#' @export
#'
add_data <- function(study, id, data = NULL, coldesc = NULL) {
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
    cb <- codebook(data = data, coldesc = coldesc, as_json = FALSE)
    d <- c(list(id = id), cb)
    d$data <- data
  }

  class(d) <- c("reg_study_data", "list")

  idx <- get_idx(study, id, "data")

  study$data[[idx]] <- d

  invisible(study)
}
