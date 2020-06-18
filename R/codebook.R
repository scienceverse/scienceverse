#' Create PsychDS Codebook from Data
#'
#' @param data The data frame to generate a codebook for
#' @param name The name of this dataset (if NULL, will be the same as `data`)
#' @param vardesc Optional variable properties in the format of a named list of vectors (can be named or unnamed and in the same order as the data) from the options description, privacy, type, propertyID, minValue, maxValue, levels, ordered, na, naValues, alternateName, unitCode
#' @param ... Further dataset properties (e.g., description, license, author, citation, funder, url, doi/sameAs, keywords, temporalCoverage, spatialCoverage, datePublished, dateCreated)
#' @param schemaVersion defaults to "Psych-DS 0.1.0"
#' @param as_json Whether the output should be a list or JSON format
#'
#' @return a list or json-formatted codebook
#' @export
#'
#' @examples
#'
#' vardesc = list(
#'   description = c("Length of the sepal",
#'                   "Width of the sepal",
#'                   "Length of the petal",
#'                   "Width of the petal",
#'                   "The flower species"),
#'   type = c("float", "float", "float", "float", "string")
#' )
#' codebook(iris, vardesc = vardesc)
#'
codebook <- function(data, name = NULL, vardesc = NULL, ...,
                     schemaVersion = "Psych-DS 0.1.0", as_json = TRUE) {
  # use PsychDS format from https://docs.google.com/document/d/1u8o5jnWk0Iqp_J06PTu5NjBfVsdoPbBhstht6W0fFp0/edit

  if (is.null(name)) {
    name <- utils::capture.output(match.call()$data)
  }

  schema <- list(
    "@context" = "https://schema.org/",
    "@type" = "Dataset",
    name = name,
    schemaVersion = schemaVersion
  )

  # check datadesc
  datadesc <- list(...)

  # fix doi and relabel as sameAs
  if (!is.null(datadesc$doi)) {
    doi <- tolower(datadesc$doi) %>%
      gsub("\\s", "", .) %>%
      sub("^doi\\:", "", .) %>%
      sub("^https://doi.org/", "", .)
    datadesc$doi <- NULL
    datadesc$sameAs <- paste0("https://doi.org/", doi)
  }

  possible_vals <- c("license", "author", "citation", "funder", "url", "sameAs", "keywords", "temporalCoverage", "spatialCoverage", "datePublished", "dateCreated")
  non_standard <- setdiff(names(datadesc), possible_vals)
  if (length(non_standard) > 0) {
    warning("The following dataset properties are not standard: ",
            paste(non_standard, collapse = ", "))
  }

  # add data properties
  schema <- c(schema, datadesc)

  # check vardesc
  possible_vals <- c("description", "privacy", "type",
                     "propertyID", "minValue", "maxValue",
                     "levels", "ordered", "na", "naValues",
                     "alternateName", "unitCode")

  non_standard <- setdiff(names(vardesc), possible_vals)
  if (length(non_standard) > 0) {
    warning("The following variable properties are not standard: ",
            paste(non_standard, collapse = ", "))
  }

  # TODO: add more validation

  # make variableMeasured
  vm <- list()
  colnames <- names(data)

  for (i in 1:ncol(data)) {
    col <- colnames[i]

    # @type and name
    vm[[i]] <- list(
      `@type` = "PropertyValue",
      name = col,
      description = col # default to be replaced from vardesc
    )

    # set variable attributes from vardesc
    for (vd in names(vardesc)) {
      vals <- vardesc[[vd]]

      if (!is.null(names(vals))) {
        # set from named (if available)
        if (col %in% names(vals)) vm[[i]][vd] <- vals[col]
      } else if (length(vals) == ncol(data)) {
        # set from position
        vm[[i]][vd] <- vals[i]
      } else {
        warning("Couldn't set ", vd, " for ", col)
      }
    }

    # get levels for factors if not specified
    if (is.factor(data[[i]])) {
      if (is.null(vm[[i]]$type)) vm[[i]]$type <- "factor"

      if (is.null(vm[[i]]$levels)) {
        lvls <- levels(data[[i]])
        names(lvls) <- lvls
        vm[[i]]$levels <- lvls
      }

      if (is.null(vm[[i]]$ordered)) {
        vm[[i]]$ordered <- is.ordered(data[[i]])
      }
    } else if (is.character(data[[i]])) {
      if (is.null(vm[[i]]$type)) vm[[i]]$type <- "string"
    } else if (is.integer(data[[i]])) {
      if (is.null(vm[[i]]$type)) vm[[i]]$type <- "int"
    } else if (is.numeric(data[[i]])) {
      if (is.null(vm[[i]]$type)) vm[[i]]$type <- "float"
    } else if (is.logical(data[[i]])) {
      if (is.null(vm[[i]]$type)) vm[[i]]$type <- "bool"
    }
  }

  schema$variableMeasured <- vm

  if (isTRUE(as_json)) {
    schema <- schema %>%
      jsonlite::toJSON(auto_unbox = TRUE) %>%
      jsonlite::prettify(4)
  } else {
    class(schema) <- c("scivrs_codebook", "list")
  }

  return(schema)
}
