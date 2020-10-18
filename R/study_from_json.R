#' Get study from json file
#'
#' @param filename The name of the scienceverse-formatted json file
#'
#' @return A study object with class scivrs_study
#'
study_from_json <- function(filename) {
  if (!file.exists(filename)) {
    stop("The file ", filename, " does not exist.")
  }

  #load from json file
  study <- jsonlite::read_json(filename)

  # create study environment
  env <- new.env()
  attr(study, "env") <- env

  # set up custom analysis code ----
  n_analyses <- length(study$analyses)
  if (n_analyses > 0) {
    for (i in 1:n_analyses) {
      code <- study$analyses[[i]]$code %>% unlist()
      study$analyses[[i]]$code <- code
      func <- paste0("analysis_", study$analyses[[i]]$id)
      if (!is.null(code) && length(code)) {
        make_func(func, code, env)
      }
      # check the function exists
      if (!exists(func, envir = env)) {
        stop("The function ", func, " in analysis ", i, " is not defined")
      } else if (parse(text=paste0("env$",func)) %>% eval() %>% is.function() == FALSE) {
        stop("The function ", func, " in analysis ", i, " is not a function")
      } else {
        # load the function as code
        study$analyses[[i]]$func <- methods::getFunction(func, where = env)
      }
    }
  }

  # set up dataframes from data
  n_data <- length(study$data)
  if (n_data > 0) {
    for (i in 1:n_data) {
      if (!is.null(study$data[[i]]$design)) {
        class(study$data[[i]]$design) <- c("design", "list")
      }

      if (!is.null(study$data[[i]]$codebook)) {
        class(study$data[[i]]$codebook) <- c("psychds_codebook", "list")
        vm <- study$data[[i]]$codebook$variableMeasured
        lvls <- sapply(vm, function(x) { x$levels })
        names(lvls) <- sapply(vm, function(x) { x$name })

        coltypes <- sapply(vm, function(x) { x$dataType })
        names(coltypes) <- names(lvls)
      } else {
        lvls <- NULL
        coltypes <- NULL
      }

      if (!is.null(study$data[[i]]$data)) {
        d <- study$data[[i]]$data

        nrows <- length(d[[1]])
        df <- data.frame(row.names = 1:nrows)
        for (col in names(d)) {
          df[col] <- unlist(d[col])

          if (!is.null(coltypes[[col]])) {
            # set data types TODO: handle warnings
            type <- coltypes[[col]]
            if (type == "int") {
              df[col] <- as.integer(df[[col]])
            } else if (type == "float") {
              df[col] <- as.double(df[[col]])
            } else if (type == "string") {
              df[col] <- as.character(df[[col]])
            } else if (type == "factor") {
              df[col] <- as.factor(df[[col]])
            } else if (type == "bool") {
              df[col] <- as.logical(df[[col]])
            }
          }

          # set levels for factors
          if (!is.null(lvls[[col]])) {
            df[col] <- factor(df[[col]], lvls[[col]])
          }
        }

        study$data[[i]]$data <- df
      }
    }
  }

  class(study) <- c("scivrs_study", "list")

  invisible(study)
}
