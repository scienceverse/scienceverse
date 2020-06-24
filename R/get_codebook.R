#' Get Codebook
#'
#' @param study A study list object with class scivrs_study
#' @param data_id The id for the dataset (index or character)
#' @param as_json Return output in JSON format
#'
#' @return a list with class codebook
#' @export
#'
#' @examples
#' s <- study() %>% add_data("test", data.frame(x = 1:10))
#' cb <- get_codebook(s, "test")
#'
get_codebook <- function(study, data_id = 1, as_json = FALSE) {
  # check the data ID exists
  idx <- get_idx(study, data_id, "data")
  if (idx > length(study$data)) stop("The study does not have dataset ", data_id)

  d <- study$data[[idx]]

  if (is.null(d)) {
    stop("The study does not have dataset ", data_id)
  } else if (is.null(d$codebook)) {
    if (is.null(d$data)) {
      stop("Dataset ", data_id, " has neither a codebook nor data to make one")
    }
    message("Dataset ", data_id, " does not have a codebook; one is being created")
    cb <- faux::codebook(d$data, name = d$id, return = "list")
  } else {
    cb <- d$codebook
  }

  if (as_json) {
    cb <- cb %>%
      jsonlite::toJSON(auto_unbox = TRUE) %>%
      jsonlite::prettify(4)
  }

  return(cb)
}
