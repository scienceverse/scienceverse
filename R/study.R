#' Study
#'
#' Create or load a study object
#'
#' @param name The name of the study or a file path to a json file
#' @param ... further arguments to add
#' @return A study object with class reg_study
#' @examples
#'
#' s <- study("Demo Study")
#' study_json(s)
#'
#' @export
#'
study <- function(name = "Demo Study", ...) {
  if (grep("\\.json$", name) && file.exists(name)) {
    #load from json file
    study <- jsonlite::read_json(name)

    # set up custom analysis code ----
    n_analyses <- length(study$analyses)
    if (n_analyses > 0) {
      for (i in 1:n_analyses) {
        code <- study$analyses[[i]]$code
        func <- study$analyses[[i]]$func
        if (!is.null(code) && length(code)) {
          # handle custom code from string
          c <- paste(func, "<-", paste(code, collapse = "\n"))
          eval(parse(text = c), envir = .GlobalEnv)
          message("Loaded custom function: ", func)
        }
        # check the function exists
        if (!exists(func)) {
          stop("The function ", func, " in analysis ", i, " is not defined")
        } else if (parse(text=func) %>% eval() %>% is.function() == FALSE) {
          stop("The function ", func, " in analysis ", i, " is not a function")
        }
      }
    }

    # set up data prep
    if (length(study$prep)) {
      make_func("prep_func",
                study$prep$params,
                study$prep$code,
                study$prep$return)
      message("Loaded data prep function")
    }

    # set up dataframes from data
    n_data <- length(study$data)
    if (n_data > 0) {
      for (i in 1:n_data) {
        vm <- study$data[[i]]$variableMeasured
        n_var <- length(vm)
        if (n_var > 0) {
          n_cols <- length(vm[[1]]$values)
          data <- data.frame(".tmp" = 1:n_cols)
          for (j in 1:n_var) {
            # TODO: check for duplicate or missing names
            colname <- vm[[j]]$name
            data[[colname]] <- vm[[j]]$values
          }
          data[".tmp"] <- NULL
          study$data[[i]]$data <- data
        }

      }
    }
  } else {
    # make empty study object
    study <- c(
      list(name = name),
      list(
        info = list(...),
        authors = list(),
        hypotheses = list(),
        methods = list(),
        data = list(),
        analyses = list()
      )
    )
  }

  class(study) <- c("reg_study", "list")

  invisible(study)
}
