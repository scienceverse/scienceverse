#' Add an analysis
#'
#' Add an analysis to a study object
#'
#' @param study A study list object with class scivrs_study
#' @param id The id for this analysis (index or character) if an analysis with this id already exists, it will overwrite it
#' @param code The code to run or a file name containing the code
#' @param return A list of object names to return from the code
#' @param type Whether the code is a function, text, or file name
#' @param ... further arguments to add

#' @return A study object with class scivrs_study
#' @examples
#'
#' s <- study() %>%
#'   add_hypothesis("H1", "Petal width and length will be positively correlated.") %>%
#'   add_analysis("A1", cor.test(dat$Petal.Width, dat$Petal.Length))
#' study_to_json(s)
#'
#' @export
#'
add_analysis <- function(study, id = NULL, code = "", return = "", type = c("func", "text", "file"), ...) {
  idx <- get_idx(study, id, "analyses")
  id <- ifelse(is.null(id), idx , fix_id(id))
  type <- match.arg(type)

  if (type == "file") {
    if (file.exists(code)) {
      # make function from .R file
      code <- readLines(code)
    } else {
      stop("The file ", code, " was not found.")
    }
  } else if (type == "text") {
    code <- code
  } else {
    code_call <- match.call()$code
    if (is.language(code_call)) {
      code <- utils::capture.output(code_call)
    } else if (isTRUE(code == "")) {
      # handles empty code
      code <- code
    } else {
      stop("The code was not a function.")
    }
  }

  # get study environment
  env <- attr(study, "env")

  # create function
  func <- paste0("analysis_", id)
  make_func(func, code, return, env)

  if (!methods::existsFunction(func, where = env)) {
    stop("The function ", func, " is not defined")
  }

  code <- methods::getFunction(func, where = env)

  analysis <- list(
    id = id,
    code = code
  )
  analysis <- c(analysis, list(...))

  class(analysis) <- c("scivrs_analysis", "list")

  study$analyses[[idx]] <- analysis

  invisible(study)
}


#' Update an analysis
#'
#' @param study A study list object with class scivrs_study
#' @param id The id for this analysis (index or character) if an analysis with this id already exists, it will overwrite it
#' @param code The code to run or a file name containing the code
#' @param return A list of object names to return from the code
#' @param type Whether the code is a function, text, or file name
#' @param new_id A new (character) ID for this hypothesis
#' @param ... further arguments to add

#' @return A study object with class scivrs_study
#' @export
#'
#' @examples
#' s <- study() %>% add_analysis("A1", rnorm(10))
#' s <- update_analysis(s, 1, code = rnorm(20), new_id = "A1a")
#' s
update_analysis <- function(study, id, code = NULL,
                            return = "",
                            type = c("func", "text", "file"),
                            new_id = NULL, ...) {
  idx <- get_idx(study, id, "analyses")
  type <- match.arg(type)

  if (idx > length(study$analyses)) stop("The study does not have an analysis with the ID ", id)

  if (!is.null(new_id))
    study$analyses[[idx]]$id <- fix_id(new_id)

  if (type == "file") {
    if (file.exists(code)) {
      # make function from .R file
      code <- readLines(code)
    } else {
      stop("The file ", code, " was not found.")
    }
  } else if (type == "text") {
    code <- code
  } else {
    code_call <- match.call()$code
    if (is.language(code_call)) {
      code <- utils::capture.output(code_call)
    } else if (isTRUE(code == "")) {
      # handles empty code
      code <- code
    } else {
      code <- output_custom_code(study, id)
    }
  }

  if (!is.null(code) | isTRUE(return != "")) {
    # get study environment
    env <- attr(study, "env")

    # create function
    func <- paste0("analysis_", study$analyses[[idx]]$id)
    make_func(func, code, return, env)

    if (!methods::existsFunction(func, where = env)) {
      stop("The function ", func, " is not defined")
    }

    code <- methods::getFunction(func, where = env)
    study$analyses[[idx]]$code <- code
  }

  ## merge ... extras
  dots <- list(...)
  for (x in names(dots)) {
    study$analyses[[idx]][x] <- dots[x]
  }

  invisible(study)
}
