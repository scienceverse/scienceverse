#' Study
#'
#' Create or load a study object
#'
#' @param name The name of the study or a file path to a json file
#' @return A study object with class reg_study
#' @examples
#'
#' mystudy <- study("Iris Petals")
#'
#' @export
#'
study <- function(name = "Demo Study") {
  if (grep("\\.json$", name) && file.exists(name)) {
    study <- jsonlite::read_json(name)

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
    study <- list(
      name = name,
      hypotheses = list(),
      methods = list(),
      data = list(),
      analyses = list()
    )
  }

  class(study) <- c(class(study), "reg_study")

  invisible(study)
}

#' Get index from id
#'
#' Get the index from id for an item in the hypotheses, analyses, or data sections of a study object
#'
#' @param study A study list object with class reg_study
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
#' @param study A study list object with class reg_study
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
    if (section == "analyses") study <- add_analysis(study, id=id)
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
      if (section == "analyses") study <- add_analysis(study, id=id)
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
              " exists. Creating a default item with at index = ",
              n+1)

      if (section == "hypotheses") study <- add_hypothesis(study, id=id)
      if (section == "analyses") study <- add_analysis(study, id=id)
      if (section == "data") study <- add_data(study, id=id)

      idx <- length(study[[section]])
    }
  }

  list(id = id, idx = idx)
}




#' Add Hypothesis
#'
#' Add a hypothesis to a study object
#'
#' @param study A study list object with class reg_study
#' @param description The text descriiption of the hypothesis
#' @param evaluation The rule for evaluating this criterion c("&", "and", "|", "or")
#' @param id The id for this hypothesis (index or character) if NULL, this creates a new hypothesis, if a hypothesis with this id already exists, it will overwrite it
#' @return A study object with class reg_study
#' @examples
#'
#' mystudy <- study("Iris Petals") %>%
#'   add_hypothesis("Petal length and width will be significantly correlated")
#'
#' @export
#'
add_hypothesis <- function(study,
                           description = "Describe your hypothesis",
                           evaluation = "&",
                           id = NULL) {
  hypothesis <- list(
    id = id,
    description = description,
    criteria = list(),
    evaluation = evaluation
  )

  class(hypothesis) <- c(class(hypothesis), "reg_study_hypothesis")

  idx <- get_idx(study, id, "hypotheses")

  study$hypotheses[[idx]] <- hypothesis

  invisible(study)
}

#' Add Criterion
#'
#' Add a criterion to a hypothesis in a study object
#'
#' @param study A study list object with class reg_study
#' @param result The name of the item in the analysis results list to compare
#' @param operator The operator for comparison c("<", "=", ">", "!=")
#' @param comparator The value to compare
#' @param hypothesis_id The id for the hypothesis (index or character) if NULL, assigns to the last hypothesis in the list
#' @param analysis_id The id for the relevant analysis (index or character) if NULL, assigns to the last analysis in the list
#' @return A study object with class reg_study
#' @examples
#'
#' mystudy <- study("Iris Petals") %>%
#'   add_hypothesis("Petal length and width will be significantly correlated") %>%
#'   add_criterion("p.value", "<", 0.05)
#'
#' @export
#'
add_criterion <- function(study,
                          result,
                          operator,
                          comparator,
                          hypothesis_id = NULL,
                          analysis_id = NULL) {
  # get ids and indices
  hypothesis <- get_id_idx(study, hypothesis_id, "hypotheses")
  analysis <- get_id_idx(study, analysis_id, "analyses")

  # set up criterion structure
  criterion <- list(
    hypothesis_id = hypothesis$id,
    analysis_id = analysis$id,
    result = result,
    operator = operator,
    comparator = comparator
  )

  class(criterion) <- c(class(criterion),
                        "reg_study_hypothesis_criterion")

  # add criterion to hypothesis
  crit_idx <- length(study$hypotheses[[hypothesis$idx]]$criteria) + 1
  study$hypotheses[[hypothesis$idx]]$criteria[[crit_idx]] <- criterion

  invisible(study)
}





#' Add Analysis
#'
#' Add an analysis to a study object
#'
#' @param study A study list object with class reg_study
#' @param func The name of the function to run
#' @param params A list of parameters for the function arguments
#' @param code Code to define custom functions
#' @param id The id for this analysis (index or character) if NULL, this creates a new analysis, if an analysis with this id already exists, it will overwrite it
#' @return A study object with class reg_study
#'
#' @export
#'
add_analysis <- function(study,
                         func = "list",
                         params = list(),
                         code = NULL,
                         id = NULL) {
  analysis <- list(
    id = id,
    func = func,
    params = params,
    code = code
  )

  class(analysis) <- c(class(analysis), "reg_study_analysis")

  idx <- get_idx(study, id, "analyses")

  study$analyses[[idx]] <- analysis

  invisible(study)
}

#' Add Data
#'
#' Add a dataset to a study object
#'
#' @param study A study list object with class reg_study
#' @param data The dataset as a data.frame, codebook object, path to a data file, or path to a codebook file
#' @param id The id for this dataset (index or character) if NULL, this creates a new dataset, if an analysis with this id already exists, it will overwrite it
#' @return A study object with class reg_study
#' @examples
#'
#' mystudy <- study() %>%
#'   add_data(iris)
#'
#' @export
#'
add_data <- function(study, data = NULL, id = NULL) {
  vm <- list()
  if (is.data.frame(data)) {
    # get variableMeasured list from table structure
    colnames <- names(data)

    for (i in 1:ncol(data)) {
      vm[[i]] <- list(
        type = "PropertyValue",
        unitText = colnames[i],
        name = colnames[i],
        values = data[[i]]
      )

      if (is.numeric(data[[i]])) {
        vm[[i]]$minValue <- min(data[[i]], na.rm = TRUE)
        vm[[i]]$maxValue <- max(data[[i]], na.rm = TRUE)
      }
    }
  }
  d <- list(
    id = id,
    "@type" = "Dataset",
    schemaVersion = "Psych-DS 0.1.0",
    variableMeasured = vm,
    data = as.data.frame(data)
  )

  class(d) <- c(class(d), "reg_study_data")

  idx <- get_idx(study, id, "data")

  study$data[[idx]] <- d

  invisible(study)
}

#' Run analysis
#'
#' Run the analyses on the data
#'
#' @param study A study list object with class reg_study
#' @return A study object with class reg_study
#'
#' @export
#'
study_analyse <- function(study) {
  analysis_n <- length(study$analyses)
  for (i in 1:analysis_n) {
    func <- study$analyses[[i]]$func
    params <- study$analyses[[i]]$params
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

    # save results, convert to list, and make class list
    # (jsonlite doesn't deal well with non-list classes like htest , etc)
    study$analyses[[i]]$results <- do.call(func,params) %>%
      as.list()
    class(study$analyses[[i]]$results) <- "list"
  }

  # evaluate each hypothesis
  hypothesis_n <- length(study$hypotheses)
  for (i in 1:hypothesis_n) {
    h <- study$hypotheses[[i]]

    # evaluate each criterion
    criteria_n <- length(h$criteria)
    criteria <- vector()
    for (j in 1:criteria_n) {
      criterion <- h$criteria[[j]]
      analysis <- grep(criterion$analysis, study$analyses, fixed = TRUE)

      value <- study$analyses[[analysis]]$results[[criterion$result]]
      if (criterion$operator == "<") {
        conclusion <- value < criterion$comparator
      } else if (criterion$operator == ">") {
        conclusion <- value > criterion$comparator
      } else if (criterion$operator == "=") {
        conclusion <- value == criterion$comparator
      } else if (criterion$operator == "!=") {
        conclusion <- value != criterion$comparator
      } else {
        conclusion <- NA
      }
      criteria[j] <- conclusion
      study$hypotheses[[i]]$criteria[[j]]$conclusion <- conclusion
    }

    # evaluate hypothesis
    eval <- tolower(study$hypotheses[[i]]$evaluation)
    if (eval %in% c("&", "and")) {
      study$hypotheses[[i]]$conclusion = (mean(criteria) == TRUE)
    } else if (eval %in% c("|", "or")) {
      study$hypotheses[[i]]$conclusion = (mean(criteria) > 0)
    } else {
      study$hypotheses[[i]]$conclusion = NA
    }
  }

  invisible(study)
}

#' Save study
#'
#' Save the study framework to a JSON file
#'
#' @param study A study list object with class reg_study
#' @param filename The name to save the file
#' @param data_values Whether to include data values in the JSON file (defaults to TRUE)
#' @return A study object with class reg_study
#'
#' @export
#'
study_save <- function(study, filename = "study.json", data_values = TRUE) {
  # make a copy to modify for JSON format
  json_study <- study

  if (!length(grep("\\.json$", filename))) {
    # add .json extension if not already specified
    filename <- paste0(filename, ".json")
  }

  n_data <- length(json_study$data)
  if (n_data > 0) {
    for (i in 1:n_data) {
      # remove data frame
      json_study$data[[i]]$data <- NULL

      n_vars <- length(json_study$data[[i]]$variableMeasured)
      if (!data_values && n_vars > 0) {
        for (j in 1:n_vars) {
          json_study$data[[i]]$variableMeasured$values <- NULL
        }
      }
    }
  }

  json_study %>%
    jsonlite::toJSON(auto_unbox = TRUE) %>%
    jsonlite::prettify(indent = 2) %>%
    writeLines(filename)

  invisible(study)
}

#' Generate a Report
#'
#' Generate a study report
#'
#' @param study A study list object with class reg_study
#' @param template The type of report c("prereg", "postreg") or a path to a custom template
#' @param filename The file path to save to
#' @return A study object with class reg_study
#' @examples
#'
#' mystudy <- study("Iris Study") %>%
#'   add_hypothesis("Petal length and width will be significantly correlated") %>%
#'   add_analysis("cor.test", list(
#'     x = ".data$Petal.Length",
#'     y = ".data$Petal.Width"
#'   )) %>%
#'   add_criterion("p.value", "<", 0.05) %>%
#'   add_data(iris) %>%
#'   study_analyse() %>%
#'   study_report(template = "postreg")
#'
#' @export
#'
study_report <- function(study, template = "prereg",
                         filename = "study.html") {
  if (!length(grep("\\.html$", filename))) {
    # add .html extension if not already specified
    filename <- paste0(filename, ".html")
  }
  if (substr(filename, 1, 1) != "/") {
    filename <- paste0(getwd(), "/", filename)
  }
  message("Saving to", filename)

  if (template == "prereg") {
    template <- system.file("rmarkdown", "prereg.Rmd", package = "reg")
  } else if (template == "postreg") {
    template <- system.file("rmarkdown", "postreg.Rmd", package = "reg")
  }
  options(knitr.duplicate.label = 'allow')
  rmarkdown::render(template,
                    output_file = filename,
                    quiet = TRUE,
                    envir = new.env(),
                    encoding = "UTF-8")
  invisible(study)
}



#' Output hypotheses
#'
#' Output hypotheses specified in the json file
#'
#' @param study A study list object created with pipeline()
#' @return The study object
#'
#' @export

output_hypotheses <- function(study) {
  cat("## Hypotheses\n\n")

  for (i in 1:length(study$hypotheses)) {

    cat("### Hypothesis ", i, "\n\n", study$hypotheses[[i]]$desc, "\n\n", sep = "")

    criteria <- study$hypotheses[[i]]$criteria

    for (j in 1:length(criteria)) {
      cat("* Criterion", j, "is confirmed if analysis",
          criteria[[j]]$test, "yields",
          criteria[[j]]$result,
          criteria[[j]]$operator,
          criteria[[j]]$comparator,
          "  \n"
      )
    }

    cat("\n")

    # explain evaluation
    eval <- study$hypotheses[[i]]$evaluation
    if (eval %in% c("&", "and")) {
      cat("If all criteria are met, this hypothesis is supported.")
    } else if (eval %in% c("|", "or")) {
      cat("If any criteria are met, this hypothesis is supported.")
    } else {
      cat(eval)
    }

    cat("\n\n\n")
  }

  invisible(study)
}


#' Output results
#'
#' Output results specified in the json file
#'
#' @param study A study list object created with pipeline()
#' @param digits integer indicating the number of decimal places.
#' @return The study object
#'
#' @export

output_results <- function(study, digits = 3) {
  cat("## Results\n\n")
  for (i in 1:length(study$hypotheses)) {

    cat("### Hypothesis ", i, "\n\n", study$hypotheses[[1]]$desc, "\n\n", sep = "")

    criteria <- study$hypotheses[[i]]$criteria

    for (j in 1:length(criteria)) {
      analysis <- grep(criteria[[j]]$analysis, study$analyses, fixed = TRUE)
      result <- study$analyses[[analysis]]$results[[criteria[[j]]$result]]

      cat("* Criterion ", j, " was ",
          criteria[[j]]$result, " ",
          criteria[[j]]$operator, " ",
          criteria[[j]]$comparator,
          " in analysis ", criteria[[j]]$analysis, ".  \n    The result was ",
          criteria[[j]]$result, " = ", round_char(result, digits),
          "  \n",
          sep = ""
      )
    }

    cat("\n**Conclusion**: ")
    eval <- study$hypotheses[[i]]$evaluation
    conclusion <- study$hypotheses[[i]]$conclusion
    if (eval %in% c("&", "and")) {
      if (conclusion) {
        cat("Congratulations! All criteria were met, this hypothesis was supported.")
      } else {
        cat("All criteria were not met, this hypothesis was not supported.")
      }
    } else if (eval %in% c("|", "or")) {
      if (conclusion) {
        cat("At least one criterion was met, this hypothesis was supported.")
      } else {
        cat("No criteria were met, this hypothesis was not supported.")
      }
    } else {
      cat("The evaluation criteria could not be automatically evaluated.")
    }

    cat("\n\n")
  }

  invisible(study)
}


#' Output analyses
#'
#' Output analysis plan specified in the json file
#'
#' @param study A study list object created with pipeline()
#' @return The study object
#'
#' @export

output_analyses <- function(study) {
  cat("## Analyses\n\n")

  for (i in 1:length(study$analyses)) {
    cat("###", study$analyses[[i]]$name, "\n\n")

    func <- study$analyses[[i]]$func
    params <- study$analyses[[1]]$params

    keys <- names(params)
    vals <- unlist(params) %>% unname()
    x <- c()
    for (j in 1:length(keys)) {
      x[j] <- paste0(keys[j], " = ", vals[j])
    }

    cat("We will run `",
        func, "(", paste0(x, collapse = ", "), ")`\n\n\n",
        sep = "")
  }

  invisible(study)
}

#' Character-safe rounding
#'
#' Round a vector if it is numeric, but return the original vector if it is character.
#'
#' @param x	a character vector.
#' @param digits integer indicating the number of decimal places.
#' @param ...	arguments to be passed to methods.
#' @return The character vector or the rounded version if numeric.
#'
round_char <- function(x, digits = 0, ...) {
  num_x <- suppressWarnings(as.numeric(x))
  if (is.na(num_x)) return(x)
  round(num_x, digits, ...)
}
