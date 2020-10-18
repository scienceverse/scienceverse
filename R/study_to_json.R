#' Study Object to JSON string
#'
#' Convert a study object to a JSON string
#'
#' @param study A study list object with class scivrs_study
#' @param data_values Whether to include data values in the JSON file (defaults to TRUE)
#' @return A prettified JSON string
#'
#' @examples
#'
#' study() %>% study_to_json()
#'
#' @export
#'
study_to_json <- function (study, data_values = TRUE) {
  n_data <- length(study$data)
  if (n_data > 0) {
    for (i in 1:n_data) {
      # remove data frame
      if (data_values) {
        # convert data frame to smaller format
        df <- study$data[[i]]$data
        study$data[[i]]$data <- as.list(df)
      } else {
        study$data[[i]]$data <- NULL
      }

      # n_vars <- length(study$data[[i]]$variableMeasured)
      # if (!data_values && n_vars > 0) {
      #   for (j in 1:n_vars) {
      #     study$data[[i]]$variableMeasured[[j]]$values <- NULL
      #   }
      # }
    }
  }

  n_analyses <- length(study$analyses)
  if (n_analyses > 0) {
    for (i in 1:n_analyses) {
      study$analyses[[i]]$func <- NULL
      #
      # code <- study$analyses[[i]]$code %>%
      #   jsonlite::toJSON() %>%
      #   jsonlite::fromJSON()
      #
      # mincode <- code[3:(length(code)-1)]
      #
      # study$analyses[[i]]$code <- mincode
    }
  }

  study %>%
    jsonlite::toJSON(auto_unbox = TRUE) %>%
    jsonlite::prettify(4) %>%
    # collapse empty brackets
    gsub("\\[\\s*\\]", "\\[\\]", .) %>%
    gsub("\\{\\s*\\}", "\\{\\}", .) %>%
    # collapse short string arrays
    #gsub('"(.{0,5})",\\s*"(.{0,5})"', '"\\1", "\\2"', ., perl = TRUE) %>%
    # collapse numeric arrays
    gsub('(?<=\\d)\\s*,\\s*(?=(\\-?\\.?\\d))', ', ', ., perl = TRUE) %>%
    gsub("\\[\\s+(?=(\\-?\\.?\\d))", "\\[", ., perl = TRUE) %>%
    gsub("(?<=\\d)\\s+\\]", "\\]", ., perl = TRUE)
}

