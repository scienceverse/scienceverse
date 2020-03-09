#' Create PsychDS Codebook from Data
#'
#' @param data The data frame to generate a codebook for
#' @param coldesc Optional named list of column descriptions
#' @param as_json Whether the output should be a list or JSON format
#'
#' @return a list or json-formatted codebook
#' @export
#'
#' @examples
#'
#' codebook(iris)
#'
codebook <- function(data, coldesc = NULL, as_json = TRUE) {
  vm <- list()
  colnames <- names(data)
  if (is.null(coldesc)) {
    # set column descriptions to the same as column names
    coldesc <- colnames
    names(coldesc) <- colnames
  }

  for (i in 1:ncol(data)) {
    col <- colnames[i]

    if (is.na(coldesc[col])) {
      # column not defined in coldesc
      coldesc[col] <- col
    }

    vm[[i]] <- list(
      type = "PropertyValue",
      unitText = colnames[i],
      name = coldesc[[col]]
    )

    vm[[i]]$missingValues <- is.na(data[[i]]) %>% sum()

    if (is.numeric(data[[i]])) {
      vm[[i]]$minValue <- min(data[[i]], na.rm = TRUE)
      vm[[i]]$maxValue <- max(data[[i]], na.rm = TRUE)
      vm[[i]]$meanValue <- mean(data[[i]], na.rm = TRUE)
      vm[[i]]$sdValue <- stats::sd(data[[i]], na.rm = TRUE)
    } else if (is.factor(data[[i]])) {
      lvls <- levels(data[[i]])
      names(lvls) <- lvls

      vm[[i]]$levels <- lvls
    }
  }

  schema <- list(
    "@type" = "Dataset",
    schemaVersion = "Psych-DS 0.1.0",
    variableMeasured = vm
  )

  if (isTRUE(as_json)) {
    schema <- schema %>%
      jsonlite::toJSON(auto_unbox = TRUE) %>%
      jsonlite::prettify(4)
  }

  return(schema)
}
