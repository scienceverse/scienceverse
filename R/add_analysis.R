#' Add Analysis
#'
#' Add an analysis to a study object
#'
#' @param study A study list object with class scivrs_study
#' @param id The id for this analysis (index or character) if an analysis with this id already exists, it will overwrite it
#' @param code The code to run or a file name containing the code
#' @param return A list of object names to return from the code
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
add_analysis <- function(study, id = NULL, code = "", return = "", ...) {
  idx <- get_idx(study, id, "analyses")
  id <- ifelse(is.null(id), idx , fix_id(id))

  # process code to see if it is a function or file
  code_call <- match.call()$code
  if (is.character(code_call)) {
    if (file.exists(code_call[1])) {
      # make function from .R file
      code <- readLines(code_call)
    } else {
      stop("The file ", code_call, " was not found.")
    }
  } else if (is.language(code_call)) {
    code <- utils::capture.output(code_call)
  } else {
    stop("The code was neither a function nor a filename.")
  }

  func <- paste0("analysis_", id, "_func")
  make_func(func, code, return)

  if (!methods::existsFunction(func)) {
    stop("The function ", func, " is not defined")
  }

  code <- NULL
  func_env <- get_env_name(func)
  code <- methods::getFunction(func, where = .GlobalEnv)

  analysis <- list(
    id = id,
    code = code
  )
  analysis <- c(analysis, list(...))

  class(analysis) <- c("scivrs_analysis", "list")

  study$analyses[[idx]] <- analysis

  invisible(study)
}
