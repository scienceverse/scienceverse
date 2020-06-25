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
    cb <- codebook(d$data, name = d$id, return = "list")
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


#' Create PsychDS Codebook from Data
#'
#' @param data The data frame to generate a codebook for
#' @param name The name of this dataset (if NULL, will be the same as `data`)
#' @param vardesc Optional variable properties in the format of a named list of vectors (can be named or unnamed and in the same order as the data) from the options description, privacy, type, propertyID, minValue, maxValue, levels, ordered, na, naValues, alternateName, unitCode
#' @param ... Further dataset properties (e.g., description, license, author, citation, funder, url, doi/sameAs, keywords, temporalCoverage, spatialCoverage, datePublished, dateCreated)
#' @param schemaVersion defaults to "Psych-DS 0.1.0"
#' @param return Whether the output should be in JSON format (json), a list (list) or the reformatted data with the codebook as an attribute (data)
#' @param interactive Whether the function should prompt the user to describe columns and factor levels
#'
#' @return a list or json-formatted codebook, or reformatted data withthe codebook as an attribute
#' @export
#'
#' @examples
#'
#' vardesc = list(
#'   description = c(speed = "Speed",
#'                   dist = "Stopping distance" ),
#'   type = c("float", "float"),
#'   unitText = c(speed = "mph", dist = "ft")
#' )
#' codebook(cars, vardesc = vardesc)
#' codebook(cars, vardesc = vardesc, return = "list")
#'
codebook <- function(data, name = NULL, vardesc = list(), ...,
                     schemaVersion = "Psych-DS 0.1.0",
                     return = "json", interactive = FALSE) {

  # set faux verbose options to same as scivrs opts
  faux_verbose <- faux::faux_options("verbose")
  faux::faux_options("verbose" = scienceverse_options("verbose"))
  on.exit({faux::faux_options("verbose" = faux_verbose)})

  if (is.null(name)) {
    name <- utils::capture.output(match.call()$data)
  }

  faux::codebook(data = data, name = name, vardesc = vardesc,
                 schemaVersion = schemaVersion, return = return,
                 interactive = interactive, ...)
}
