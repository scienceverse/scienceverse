#' Get data from study
#'
#' @param study A study list object with class reg_study
#' @param id The id for the dataset (index or character)
#'
#' @return A data frame
#' @export
#'
#' @examples
#'
#' s <- study() %>% add_data("test", data.frame(x = 1:10))
#' dat <- get_data(s, "test")
#'dev
get_data <- function(study, id = 1) {
  # check the data ID exists
  idx <- get_idx(study, id, "data")

  if (is.null(study$data[[idx]])) {
    stop("The study does not have dataset ", id)
  } else if (is.null(study$data[[idx]]$data)) {
    stop("Dataset ", id, " does not have any attached data")
  } else {
    return(study$data[[idx]]$data)
  }

}
