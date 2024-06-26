#' Get index from id
#'
#' Get the index from id for an item in the hypotheses, analyses, or data sections of a study object
#'
#' @param study A study list object with class scivrs_study
#' @param id The id for the section (index or character) if NULL, assigns to the last item in the list
#' @param section The section to search, c("hypotheses", "analyses", "data")
#' @return A numeric index
#' @keywords internal
#'
get_idx <- function(study, id = NULL, section = "hypotheses") {
  n <- length(study[[section]])
  idx <- n + 1
  if (length(grep("^\\d+$", id))) { # is an integer
    if (n >= id) idx <- as.numeric(id)
  } else if (is.character(id) & n > 0) {
    for (i in 1:n) {
      if (study[[section]][[i]]$id == id) idx <- i
    }
  }

  return(idx)
}


#' Get index and id
#'
#' Get the index and id from an item in the hypotheses, analyses, or data sections of a study object
#'
#' @param study A study list object with class scivrs_study
#' @param id The id for the section (index or character) if NULL, assigns to the last item in the list
#' @param section The section to search, c("hypotheses", "analyses", "data")
#' @return A list of id and idx
#' @keywords internal
#'
get_id_idx <- function(study, id = NULL, section = "hypotheses") {
  if (length(study[[section]]) == 0) {
    if (length(grep("^\\d+$", id))) id <- 1
    warning("No ", section, " items exist. ",
            "Creating a default item with id = ",
            ifelse(is.null(id), "NULL", id))

    if (section == "hypotheses") study <- add_hypothesis(study, id=id)
    if (section == "analyses") study <- add_analysis(study, id=id, type = "text")
    if (section == "data") study <- add_data(study, id=id)
  }
  n <- length(study[[section]])
  idx <- n # default to the last one

  if (is.null(id)) {
    # set to last item
    last_id <- study[[section]][[n]]$id
    id <- ifelse(is.null(last_id), n, last_id)
  } else if (length(grep("^\\d+$", id))) {
    if (id > n) {
      warning("No ", section, " item with index = ", id,
              " exists. Creating a default item at index = ",
              n+1)

      if (section == "hypotheses") study <- add_hypothesis(study, id=id)
      if (section == "analyses") study <- add_analysis(study, id=id, code = "")
      if (section == "data") study <- add_data(study, id=id)

      idx <- length(study[[section]])
    } else {
      idx <- as.numeric(id)
      id <- study[[section]][[idx]]$id
    }
  } else if (is.character(id)) {
    # find existing item with this id
    ids <- sapply(study[[section]], `[[`, "id")

    if (id %in% ids) {
      idx <- which(id == ids)
    } else {
      warning("No ", section, " item with index = ", id,
              " exists. Creating a default item at index = ",
              n+1)

      if (section == "hypotheses") study <- add_hypothesis(study, id=id)
      if (section == "analyses") study <- add_analysis(study, id=id, code = "")
      if (section == "data") study <- add_data(study, id=id)

      idx <- length(study[[section]])
    }
  }

  list(id = id, idx = idx)
}

#' Fix IDs
#'
#' @param id the id to fix
#'
#' @return a fixed ID character string (only a-z, A-Z, 0-9, and _)
#'
#' @keywords internal
fix_id <- function(id) {
  new_id <- gsub("[^a-zA-Z0-9_]+", "_", id)

  if (new_id != id) {
    message("id \"", id, "\" changed to \"", new_id,"\"")
  }

  new_id
}

#' Get value from results list
#'
#' @param txt text of result to check against names
#' @param results named list of results
#'
#' @return value from results list or the txt if not found
#' @keywords internal
#'
get_res_value <- function(txt, results) {
  # return txt if it is boolean
  bool_vals <- list("TRUE", "FALSE", TRUE, FALSE, "true", "false")
  if (txt %in% bool_vals) return(as.logical(txt))

  # return numeric version if it is numeric
  num <- utils::type.convert(txt, as.is = TRUE)
  if (!is.character(num)) return(num)

  # probably a character so check the results list
  splitres <- stringr::str_split(txt, "(\\$|\\[+'?\"?|'?\"?\\]+)")
  res <- splitres[[1]][1]
  if (length(splitres[[1]]) == 1) {
    # no index
    idx <- NULL
  } else {
    idx <- splitres[[1]][2] %>%
      utils::type.convert(as.is = TRUE)
  }

  if (res %in% names(results)) {
    if (is.null(idx)) return(results[[res]])

    if (is.numeric(idx) && length(results[[res]]) < idx) {
      warning(res, " does not have a numeric index ", idx)
      return(NULL)
    }

    if (is.character(idx) && !idx %in% names(results[[res]])) {
      warning(res, " does not have a named index ", idx)
      return(NULL)
    }

    return(results[[res]][[idx]])
  } else {
    # named result not found
    return(txt)
  }
}

#' Character-safe rounding
#'
#' Round a vector if it is numeric, but return the original vector if it is character.
#'
#' @param x	a character vector.
#' @param digits integer indicating the number of decimal places.
#' @param as_char Whether the result should be formatted as a character with trailing 0s (if relevant)
#' @param ...	arguments to be passed to methods.
#' @return The character vector or the rounded version if numeric.
#' @keywords internal
#'
round_char <- function(x, digits = 0, as_char = FALSE, ...) {
  if (length(x) == 0) return("NA")

  if (is.list(x)) {
    res <- lapply(x, round_char, digits = digits, as_char = as_char)
  } else if (length(x) > 1) {
    res <- sapply(x, round_char, digits = digits, as_char = as_char)
  } else {
    if (!is.numeric(x) & !is.character(x)) return (x)

    num_x <- suppressWarnings(as.numeric(x))
    if (is.na(num_x)) return(x)
    res <- round(num_x, digits, ...)

    if (as_char) {
      fmt <- paste0("%.", digits, "f")
      res <- sprintf(fmt, res)
    }
  }

  names(res) <- names(x)
  return(res)
}


#' Get Package
#'
#' @param f the function to search (as a character string)
#'
#' @return a list of packages/environments the function is in
#' @keywords internal

get_env_name <- function(f) {
  # from https://stackoverflow.com/questions/6429180/how-do-you-you-determine-the-namespace-of-a-function
  # https://stackoverflow.com/users/1863950/artem-klevtsov
  attached <- c(environmentName(.GlobalEnv), loadedNamespaces())
  envs <- c(.GlobalEnv, lapply(attached[-1], .getNamespace))
  attached[vapply(envs, function(env) exists(f, env, inherits = FALSE), logical(1))]
}


#' Load Params
#'
#' Load .data\[id\] and .data\[id\]$col references from the data
#'
#' @param params a list of parameter names and values
#' @param study the study object to get the data from
#'
#' @return params list
#' @keywords internal
#'
load_params <- function(params, study) {
  # replace any params equal to ".data[id]" with the data frame
  pattern <- "^\\.data\\[(.+)\\]$"
  replace_data <- grep(pattern, params)
  if (length(replace_data)) {
    for (j in replace_data) {
      id <- params[[j]] %>%
        regexpr(pattern, .) %>%
        regmatches(params[[j]], .) %>%
        gsub(".data[", "", ., fixed = TRUE) %>%
        gsub("]", "", ., fixed = TRUE)
      idx <- get_idx(study, id, "data")
      if (length(study$data) < idx) stop("dataset ", idx, " does not exist")
      params[[j]] <- study$data[[idx]]$data
    }
  }

  # replace any params equal to ".data[id]$col" with the column vector
  pattern <- "^\\.data\\[(.+)\\]\\$"
  replace_data_cols <- grep(pattern, params)
  if (length(replace_data_cols)) {
    for (j in replace_data_cols) {
      id <- params[[j]] %>%
        regexpr(pattern, .) %>%
        regmatches(params[[j]], .) %>%
        gsub(".data[", "", ., fixed = TRUE) %>%
        gsub("]$", "", ., fixed = TRUE)
      idx <- get_idx(study, id, "data")
      if (length(study$data) < idx) stop("dataset ", idx, " does not exist")

      col <- gsub(pattern, "", params[j])
      params[[j]] <- study$data[[idx]]$data[[col]]
    }
  }

  params
}


#' Print Study Object
#'
#' @param x The scivrs_study list
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
#'
print.scivrs_study <- function(x, ...) {
  hyp <- sapply(x$hypotheses, `[[`, "id") %>%
    paste(collapse = ", ")
  if (hyp == "") hyp <- "None"

  dat <- sapply(x$data, `[[`, "id") %>%
    paste(collapse = ", ")
  if (dat == "") dat <- "None"

  ana <- sapply(x$analyses, `[[`, "id") %>%
    paste(collapse = ", ")
  if (ana == "") ana <- "None"

  underline <- rep("-", nchar(x$name)) %>% paste(collapse="")
  txt <- sprintf("%s\n%s\n\n* Hypotheses: %s\n* Data: %s\n* Analyses: %s\n\n%s", x$name, underline, hyp, dat, ana, eval_summary(x))

  cat(txt)
}

#' Print Results List
#'
#' @param x The scivrs_results list
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
#'
print.scivrs_results <- function(x, ...) {
  cat(nested_list(x))
}

#' Print Author List
#'
#' @param x The scivrs_author list
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
#'
print.scivrs_author <- function(x, ...) {
  cat(nested_list(x))
}

#' Print Authors List
#'
#' @param x The scivrs_authors list
#' @param ... Additional parameters for print
#'
#' @export
#' @keywords internal
#'
print.scivrs_authors <- function(x, ...) {
  cat(nested_list(x))
}

#' Less scary green messages
#'
#' @param ... message components (see \code{\link[base]{message}})
#' @param domain (see \code{\link[base]{message}})
#' @param appendLF append new line? (see \code{\link[base]{message}})
#'
#' @return TRUE
#' @keywords internal
#'
message <- function (..., domain = NULL, appendLF = TRUE) {
  #if (is.null(knitr::opts_knit$get('rmarkdown.pandoc.to'))) {
  if (sv_opts("verbose")) {
    if (interactive()) {
      # not in knitr environment
      base::message("\033[32m", ..., "\033[39m",
                    domain = domain, appendLF = appendLF)
    } else {
      base::message(..., domain = domain, appendLF = appendLF)
    }
  }
}


#' Check if values are NULL, NA, blank after trimming, or an empty list
#'
#' @param x vector or list to test
#' @param test_for values to test for ("null" replaces NULL values, "na", replaces NA values, "trim" replaces empty strings after trimws(), "empty" replaces empty lists)
#'
#' @return vector or list of logical values
#' @export
#'
#' @examples
#' x <- list(NULL, NA, " ", list())
#' is_nowt(x)
#' is_nowt(x, test_for = "null")
#' is_nowt(x, test_for = "na")
#' is_nowt(x, test_for = "trim")
#' is_nowt(x, test_for = "empty")
#'
is_nowt <- function(x, test_for = c("null", "na", "trim", "empty")) {
  # NULL is no longer atomic as of 2023
  if (is.null(x) & "null" %in% test_for) return(TRUE)

  # only handles atomic vectors and lists
  if (!is.atomic(x) & !is.list(x)) return(FALSE)

  if (length(x) > 1) {
    args <- list(X = x, FUN = is_nowt,
                 test_for = test_for)
    func <- ifelse(is.list(x), lapply, sapply)
    y <- do.call(func, args)
    return(y)
  }

  nowt <- FALSE
  if ("null" %in% test_for)
    nowt <- nowt | isTRUE(is.null(x))
  if ("na" %in% test_for)
    nowt <- nowt | isTRUE(is.na(x))
  if ("trim" %in% test_for)
    nowt <- nowt | isTRUE(trimws(x) == "")
  if ("empty" %in% test_for)
    nowt <- nowt | (is.list(x) & length(x) == 0)

  return(nowt)
}


#' Replace values if NULL, NA, blank after trimming, or an empty list
#'
#' @param x vector or list to test
#' @param replace value to replace with
#' @param test_for values to test for ("null" replaces NULL values, "na", replaces NA values, "trim" replaces empty strings after trimws(), "empty" replaces empty lists)
#'
#' @return vector or list with replaced values
#' @export
#'
#' @examples
#' if_nowt(NULL)
#' if_nowt(NA)
#' if_nowt("   ")
#' if_nowt(c(1, 2, NA), replace = 0)
#' x <- list(NULL, NA, " ", list())
#' if_nowt(x) %>% str()
#' if_nowt(x, test_for = "null") %>% str()
#' if_nowt(x, test_for = "na") %>% str()
#' if_nowt(x, test_for = "trim") %>% str()
#' if_nowt(x, test_for = "empty") %>% str()
if_nowt <- function(x, replace = "", test_for = c("null", "na", "trim", "empty")) {
  if (length(x) > 1) {
    args <- list(X = x, FUN = if_nowt,
                 replace = replace,
                 test_for = test_for)
    func <- ifelse(is.list(x), lapply, sapply)
    y <- do.call(func, args)
    return(y)
  }

  if (is_nowt(x, test_for)) {
    return(replace)
  } else {
    return(x)
  }
}



#' Make a Named List
#'
#' Convenience function for making a named list from a vector or list of names and a vector or list of values
#'
#' @param names a vector or list of the names
#' @param values a vector or list of the values
#'
#' @return a named list
#' @export
#'
#' @examples
#' n <- LETTERS[1:3]
#' val <- c("cat", "dog", "ferret")
#' nlist(n, val)
#'
#' nlist(list("A", "B"), 1:2)
#'
nlist <- function(names, values) {
  # turn lists to vectors
  if (is.list(names)) names <- unlist(names)
  if (is.list(values)) values <- unlist(values)

  # make sure vector
  if (!is.atomic(names)) stop("names must be a vector")
  if (!is.atomic(values)) stop("values must be a vector")

  if (length(names) != length(values))
    stop("Names and values must be the same length")
  if (length(names) == 0) return(list())

  # create named list
  x <- as.list(values)
  names(x) <- as.character(names)

  x
}
