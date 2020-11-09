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
      codeText <- readLines(code)
    } else {
      stop("The file ", code, " was not found.")
    }
  } else if (type == "text") {
    # split code text at line breaks
    # code may be a list, vector, or string
    # all might have internal \n
    codeText <- sapply(code, strsplit, split = "\n") %>%
      unlist() %>% unname()
  } else {
    code_call <- match.call()$code
    if (is.language(code_call)) {
      codeText <- utils::capture.output(code_call)
    } else if (isTRUE(code == "")) {
      # handles empty code
      codeText <- code
    } else {
      stop("The code was not a function.")
    }
  }

  # add return list
  codeText <- c(codeText, make_return(return))

  # get study environment
  env <- attr(study, "env")

  # create function
  func <- paste0("analysis_", id)
  make_func(func, codeText, env)

  if (!methods::existsFunction(func, where = env)) {
    stop("The function ", func, " is not defined")
  }

  analysis <- list(
    id = id,
    code = codeText,
    func = methods::getFunction(func, where = env)
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
      codeText <- readLines(code)
    } else {
      stop("The file ", code, " was not found.")
    }
  } else if (type == "text") {
    codeText <- code
  } else {
    code_call <- match.call()$code
    if (is.language(code_call)) {
      codeText <- utils::capture.output(code_call)
    } else if (isTRUE(code == "")) {
      # handles empty code
      codeText <- code
    } else {
      codeText <- output_custom_code(study, id)
    }
  }

  # add return list
  codeText <- c(codeText, make_return(return))

  if (!is.null(codeText)) {
    # get study environment
    env <- attr(study, "env")

    # create function
    func <- paste0("analysis_", study$analyses[[idx]]$id)
    make_func(func, codeText, env)

    if (!methods::existsFunction(func, where = env)) {
      stop("The function ", func, " is not defined")
    }

    study$analyses[[idx]]$code <- codeText
    study$analyses[[idx]]$func <- methods::getFunction(func, where = env)
  }

  ## merge ... extras
  dots <- list(...)
  for (x in names(dots)) {
    study$analyses[[idx]][x] <- dots[x]
  }

  invisible(study)
}

#' Make a function
#'
#' @param func the function name
#' @param code the function body (as character, vector, or list)
#' @param envir the environment in which to define the function
#'
#' @return creates a function
#' @keywords internal
#'
make_func <- function(func, code, envir = .GlobalEnv) {
  p <- paste0(
    fix_id(func),
    " <- function() {\n  ",
    paste(code, collapse = "\n  ") %>% trimws(),
    "\n}"
  )

  tryCatch(eval(parse(text = p), envir = envir),
           error = function(e) {
             stop("The function ", func, " has errors.")
           })
}

#' Make return list
#'
#' @param return a vector of names of objects to return from the function
#'
#' @return character string of return list
#' @keywords internal
#'
make_return <- function(return) {
  if (length(return) == 0 || return[1] == "") {
    return(NULL)
  }

  if (is.null(names(return))) {
    names(return) <- return
  }

  paste0('    `', names(return), '` = ', return) %>%
    paste0(collapse = ",\n") %>%
    paste0("# return values\nlist(\n", ., "\n)") %>%
    strsplit("\n") %>%
    `[[`(1)
}
