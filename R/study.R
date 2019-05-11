#' Study
#'
#' Create or load a study object
#'
#' @param name The name of the study or a file path to a json file
#' @param ... further arguments to add
#' @return A study object with class reg_study
#' @examples
#'
#' mystudy <- study("Iris Petals")
#'
#' @export
#'
study <- function(name = "Demo Study", ...) {
  if (grep("\\.json$", name) && file.exists(name)) {
    study <- jsonlite::read_json(name)

    # set up custom analysis code
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
    study <- c(
      list(name = name),
      list(...),
      list(
        hypotheses = list(),
        methods = list(),
        data = list(),
        prep = list(),
        analyses = list()
      )
    )
  }

  class(study) <- c("reg_study", "list")

  invisible(study)
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

  class(hypothesis) <- c("reg_study_hypothesis", "list")

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

  class(criterion) <- c("reg_study_hypothesis_criterion", "list")

  # add criterion to hypothesis
  crit_idx <- length(study$hypotheses[[hypothesis$idx]]$criteria) + 1
  study$hypotheses[[hypothesis$idx]]$criteria[[crit_idx]] <- criterion

  invisible(study)
}

#' Add Data Prep
#'
#' Add a data prep step to a study object
#'
#' @param study A study list object with class reg_study
#' @param code Code to define the custom prep function or a reference to the file with the code
#' @param params A list of parameters for the function arguments
#' @param return A list of object names to return from the prep code
#' @return A study object with class reg_study
#'
#' @export
#'
add_prep <- function(study, code,
                     params = list("data" = ".data[1]"),
                     return = "data") {

  if (file.exists(code)) {
    code <- readLines(code)
  }

  prep <- list(
    params = params,
    code = code,
    return = return
  )

  make_func("prep_func", params, code, return)

  class(prep) <- c("reg_study_prep", "list")

  study$prep <- prep

  invisible(study)
}


#' Add Analysis
#'
#' Add an analysis to a study object
#'
#' @param study A study list object with class reg_study
#' @param func The name of the function to run
#' @param params A list of parameters for the function arguments
#' @param return A list of object names to return from the prep code
#' @param id The id for this analysis (index or character) if NULL, this creates a new analysis, if an analysis with this id already exists, it will overwrite it
#' @return A study object with class reg_study
#'
#' @export
#'
add_analysis <- function(study,
                         func = "list",
                         params = list(),
                         return = c(),
                         id = NULL) {

  idx <- get_idx(study, id, "analyses")

  if (file.exists(func)) {
    # make function from .R file
    code <- readLines(func)
    aid <- ifelse(is.null(id), idx , id)
    func <- paste0("analysis_", id, "_func")
    make_func(func, params, code, return)
  }

  # handle pckg::func version of func
  func_parts <- strsplit(func, "::", TRUE)
  pckg <- ""
  if (length(func_parts) == 2) {
    func <- func_parts[2]
    pckg <- func_parts[1]
  }

  if (!methods::existsFunction(func)) {
    stop("The function ", func, " is not defined")
  }

  code <- NULL
  func_env <- get_env_name(func)
  if (pckg %in% func_env) {
    func <- paste0(pckg, "::", func)
  } else if ("R_GlobalEnv" %in% func_env) {
    code <- methods::getFunction(func, where = .GlobalEnv)
  } else if (length(func_env) > 1) {
    stop(func, " is in more than one package: ", paste(func, collapse = ", "))
  } else if (!(func_env %in% c("base", "stats"))) {
    func <- paste0(func_env[1], "::", func)
  }

  analysis <- list(
    id = id,
    func = func,
    params = params,
    code = code
  )

  class(analysis) <- c("reg_study_analysis", "list")

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
  d <- list(id = id)

  if (is.character(data)) {
    if (!file.exists(data)) {
      warning("The file ", data, " does not exist.")
      return(invisible(study))
    }

    accepted_ext <- c("csv", "xls", "xlsx", "txt", "tsv", "sav")

    filename <- data
    ext <- strsplit(basename(filename), split="\\.")[[1]][-1]
    if (ext == "json") {
      json <- jsonlite::read_json(filename)
      d <- c(d, json)
    } else if (ext %in% accepted_ext) {
      data <- rio::import(filename)
    } else {
      warning("The ", ext, " format is not supported.\nPlease add data in one of the following formats: ", paste(accepted_ext, collapse = ", "))
    }
  }

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
    d <- list(
      id = id,
      "@type" = "Dataset",
      schemaVersion = "Psych-DS 0.1.0",
      variableMeasured = vm,
      data = data
    )
  }

  class(d) <- c("reg_study_data", "list")

  idx <- get_idx(study, id, "data")

  study$data[[idx]] <- d

  invisible(study)
}

#' Run data prep
#'
#' Run the data prep on the raw data
#'
#' @param study A study list object with class reg_study
#' @return A study object with class reg_study
#' @export
data_prep <- function(study) {
  prep_n <- length(study$prep)
  if (prep_n == 0) {
    message("No prep has been specified")
    return(invisible(study))
  }

  params <- load_params(study$prep$params, study)

  res <- do.call("prep_func",params)

  for (i in 1:length(res)) {
    study <- add_data(study, data = res[[i]], id = names(res)[i])
  }

  invisible(study)
}


#' Run analysis
#'
#' Run the analyses on the data
#'
#' @param study A study list object with class reg_study
#' @return A study object with class reg_study
#' @export
#' @examples
#' study() %>%
#'   add_hypothesis() %>%
#'   add_analysis("cor.test", list(
#'     x = ".data[1]$Petal.Width",
#'     y = ".data[1]$Petal.Length"
#'   )) %>%
#'   add_criterion(
#'     result = "p.value",
#'     operator = "<",
#'     comparator = 0.05
#'   ) %>%
#'   add_data(iris) %>%
#'   study_analyse()
#'
study_analyse <- function(study) {
  analysis_n <- length(study$analyses)
  if (analysis_n == 0) {
    message("No analyses have been specified")
    return(invisible(study))
  }

  for (i in 1:analysis_n) {
    func <- study$analyses[[i]]$func
    params <- study$analyses[[i]]$params %>%
      load_params(study)

    # check the function exists
    if (!exists(func)) {
      stop("The function ", func, " in analysis ", i, " is not defined")
    } else if (parse(text=func) %>% eval() %>% is.function() == FALSE) {
      stop("The function ", func, " in analysis ", i, " is not defined")
    }

    # check arguments are OK
    func_args <- formals(func) %>% names()
    param_args <- names(params)
    if (!("..." %in% func_args) && mean(param_args %in% func_args) != 1) {
      bad_args <- setdiff(func_args, param_args) %>% paste(collapse = ",")
      stop("Some arguments in analysis ", i, " are not in function ", func, ": ", bad_args)
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


#' @rdname study_analyse
#' @export
study_analyze <- study_analyse

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
study_save <- function(study,
                       filename = "study.json",
                       data_values = TRUE) {

  if (!length(grep("\\.json$", filename))) {
    # add .json extension if not already specified
    filename <- paste0(filename, ".json")
  }

  study_json(study, data_values) %>%
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
#'     x = ".data[1]$Petal.Length",
#'     y = ".data[1]$Petal.Width"
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


#' Study Object to JSON string
#'
#' Convert a study object to a JSON string
#'
#' @param study A study list object with class reg_study
#' @param data_values Whether to include data values in the JSON file (defaults to TRUE)
#' @return A prettified JSON string
#' @export
#'
study_json <- function (study, data_values = TRUE) {
  n_data <- length(study$data)
  if (n_data > 0) {
    for (i in 1:n_data) {
      # remove data frame
      study$data[[i]]$data <- NULL

      n_vars <- length(study$data[[i]]$variableMeasured)
      if (!data_values && n_vars > 0) {
        for (j in 1:n_vars) {
          study$data[[i]]$variableMeasured$values <- NULL
        }
      }
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

