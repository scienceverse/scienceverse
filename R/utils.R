#' Get index from id
#'
#' Get the index from id for an item in the hypotheses, analyses, or data sections of a study object
#'
#' @param study A study list object with class scivrs_study
#' @param id The id for the section (index or character) if NULL, assigns to the last item in the list
#' @param section The section to search, c("hypotheses", "analyses", "data")
#' @return A numeric index
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
#'
get_id_idx <- function(study, id = NULL, section = "hypotheses") {
  if (length(study[[section]]) == 0) {
    if (length(grep("^\\d+$", id))) id <- 1
    warning("No ", section, " items exist. ",
            "Creating a default item with id = ",
            ifelse(is.null(id), "NULL", id))

    if (section == "hypotheses") study <- add_hypothesis(study, id=id)
    if (section == "analyses") study <- add_analysis(study, id=id, code = "")
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
    item_exists <- FALSE
    for (i in 1:n) {
      if (study[[section]][[i]]$id == id) idx <- i
      item_exists <- TRUE
    }
    if (!item_exists) {
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
#'
get_res_value <- function(txt, results) {
  # return txt if it is boolean
  bool_vals <- list("TRUE", "FALSE", TRUE, FALSE, "true", "false")
  if (txt %in% bool_vals) return(as.logical(txt))

  # return txt if it is numeric
  suppressWarnings(num <- as.numeric(txt))
  if (isTRUE(num == txt)) return(num)

  # probably a character so check the results list
  splitres <- stringr::str_split(txt, "(\\$|\\[+'?\"?|'?\"?\\]+)")
  res <- splitres[[1]][1]
  if (length(splitres[[1]]) == 1) {
    # no index
    idx <- NULL
  } else {
    idx <- splitres[[1]][2]

    # convert to numeric if it is
    suppressWarnings(n_idx <- as.numeric(idx))
    if (!is.na(n_idx)) idx <- n_idx
  }

  if (res %in% names(results)) {
    if (is.null(idx)) {
      return(results[[res]])
    } else if (is.numeric(idx) & length(results[[res]]) < idx) {
      warning(res, " does not have a numeric index ", idx)
      return(NULL)
    } else if (is.character(idx) & !idx %in% names(results[[res]])) {
      warning(res, " does not have a named index ", idx)
      return(NULL)
    } else {
      return(results[[res]][[idx]])
    }
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

#' Make a function
#'
#' @param func the function name
#' @param code the function body
#' @param return a list of names of objects to return from the function (if blank, defaults to last value from the code, which should be a named list)
#' @param envir the environment in which to define the function
#'
#' @return creates a function
#' @keywords internal
#'
make_func <- function(func, code, return = "", envir = .GlobalEnv) {
  if (length(return) > 1) {
    if (is.null(names(return))) {
      names(return) <- return
    }
    for (r in 1:length(return)) {
      key <- names(return)[r]
      var <-  return[r]
      return[r] <- paste0('  "', key, '" = ', var)
    }
    return = paste0(
      "  list(\n    ",
      paste(return, collapse = ",\n    "),
      "\n  )"
    )
  }

  p <- paste0(
    fix_id(func),
    " <- function() {\n  ",
    paste(code, collapse = "\n  "),
    "\n\n",
    return,
    "\n}"
  )

  tryCatch(eval(parse(text = p), envir = envir),
           error = function(e) {
             stop("The function ", func, " has errors.")
           })
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
#'
print.scivrs_study <- function(x, ...) {
  utils::str(x)
}

#' Print Results List
#'
#' @param x The scivrs_results list
#' @param ... Additional parameters for print
#'
#' @export
#'
print.scivrs_results <- function(x, ...) {
  cat(faux::nested_list(x))
}
