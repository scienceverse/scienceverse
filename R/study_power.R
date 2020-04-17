#' Power Analysis through Simulation
#'
#' This function is experimental. Check power analyses with an external package before using for important decisions.
#'
#' @param study A study list object with class reg_study
#' @param rep The number of simulations to run
#'
#' @return A study object with class reg_study
#' @export
#'
#' @examples
#'
#' study <- study() %>%
#'  add_hypothesis("H1") %>%
#'  add_analysis("A1", t.test(y~A, data = D1)) %>%
#'  add_criterion("C1", "p.value", "<", 0.05) %>%
#'  add_analysis("A2", t.test(y~A, data = D2)) %>%
#'  add_criterion("C2", "p.value", "<", 0.05) %>%
#'  add_eval("corroboration", "", "C1 & C2") %>%
#'  add_eval("falsification", "", "!C1 & !C2") %>%
#'  add_sim_data("D1", between = 2, n = 25, mu = c(0, 0.5)) %>%
#'  add_sim_data("D2", between = 2, n = 50, mu = c(0, 0.5)) %>%
#'  study_power(rep = 100)
#'
study_power <- function(study, rep = 100) {
  # checks ----
  if (!is.numeric(rep) | rep < 1) {
    stop("The argument `rep` needs to be a positive number.")
  }

  if (length(study$hypotheses) == 0) {
    stop("There are no hypotheses.")
  }

  if (length(study$analyses) == 0) {
    stop("There are no analyses")
  }

  # check data have designs ----
  simdata <- list()
  for (d in study$data) {
    if (!"design" %in% class(d$design)) {
      if (is.null(d$data)) {
        warning("There is no data or design information for `", d$id, "`. Analyses that require this data are likely to fail.")
      } else if (scienceverse_options("verbose")) {
        message("The data `", d$id, "` will not be simulated, but be used as is for each analysis.")
        # load static data
        assign(d$id, d$data, envir = .GlobalEnv)
      }
    } else {
      # simulate new data ----
      simdata[[d$id]] <- faux::sim_data(d$design, rep = rep)
    }
  }

  dataids <- names(simdata)

  results <- list()
  for (i in 1:rep) {
    # assign data ----
    for (id in dataids) {
      assign(id, simdata[[id]][["data"]][[i]], envir = .GlobalEnv)
    }

    # run analyses ----
    for (a in study$analyses) {
      func <- paste0("analysis_", a$id, "_func")
      res <- do.call(func, list()) %>% as.list()
      class(res) <- "list"
      results[[a$id]][[i]] <- res
    }
  }

  # evaluate each hypothesis ----
  for (i in 1:length(study$hypotheses)) {
    h <- study$hypotheses[[i]]
    # evaluate each criterion ----
    criteria_n <- length(h$criteria)
    if (criteria_n == 0) {
      if (scienceverse_options("verbose")) {
        message("Hypothesis ", h$id, " has no criteria")
      }
    } else {
      criteria <- list()
      values <- list()
      analysis_ids <- sapply(study$analyses, function(x) {x$id})
      for (j in 1:criteria_n) {
        criterion <- h$criteria[[j]]
        analysis <- match(criterion$analysis_id, analysis_ids)

        # get value, handle indices in result
        splitres <- stringr::str_split(criterion$result, "(\\[|\\])")
        res <- splitres[[1]][1]
        idx <- as.integer(splitres[[1]][2])
        idx <- ifelse(isTRUE(idx > 0), idx, 1)
        value <- sapply(results[[analysis]], function(x) {
          x[[res]][[idx]]
        })

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

        values[[criterion$id]] <- value
        criteria[[criterion$id]] <- conclusion
      }

      # evaluate hypothesis ----
      replacement <- sapply(criteria, as.character)
      #names(replacement) <- names(criteria)

      if (is.null(h$corroboration$evaluation)) {
        warning("Hypothesis ", h$id, " has no evaluation criteria for corroboration")
        corrob <- FALSE
      } else {
        tryCatch({
          corrob_pieces <- h$corroboration$evaluation %>%
            gsub("(\\(|\\)|\\||\\!|&)", " \\1 ", .) %>%
            strsplit("\\s+", perl = TRUE) %>%
            magrittr::extract2(1)
          potential_criteria <- grep("^[a-zA-Z0-9_]+$", corrob_pieces)

          cp <- replicate(rep, corrob_pieces)
          if (is.matrix(cp)) {
            cp <- t(cp)
          } else if (is.vector(cp)) {
            cp <- matrix(cp, ncol = 1)
          }

          for (pc in potential_criteria) {
            crit <- corrob_pieces[pc]
            if (crit %in% colnames(replacement)) {
              cp[, pc] <- replacement[,crit]
            } else {
              stop(crit, " is not a defined criterion")
            }
          }

          corrob <- apply(cp, 1, paste, collapse="") %>%
            sapply(function(x) {
            parse(text = x) %>%
              eval(envir = .GlobalEnv)
            }) %>% unname()
        }, error = function(e) {
          warning("Hypothesis ", h$id, " has an error in the evaluation criteria for corroboration: ", h$corroboration$evaluation)
          corrob <<- FALSE
        })
      }
      if (is.null(h$falsification$evaluation)) {
        warning("Hypothesis ", h$id, " has no evaluation criteria for falsification")
        falsify <- FALSE
      } else {
        tryCatch({
          falsify_pieces <- h$falsification$evaluation %>%
            gsub("(\\(|\\)|\\||\\!|&)", " \\1 ", .) %>%
            strsplit("\\s+", perl = TRUE) %>%
            magrittr::extract2(1)
          potential_criteria <- grep("^[a-zA-Z0-9_]+$", falsify_pieces)
          fp <- replicate(rep, falsify_pieces)
          if (is.matrix(fp)) {
            fp <- t(fp)
          } else if (is.vector(fp)) {
            fp <- matrix(fp, ncol = 1)
          }

          for (pc in potential_criteria) {
            crit <- falsify_pieces[pc]
            if (crit %in% colnames(replacement)) {
              fp[, pc] <- replacement[,crit]
            } else {
              stop(crit, " is not a defined criterion")
            }
          }

          falsify <- apply(fp, 1, paste, collapse="") %>%
            sapply(function(x) {
              parse(text = x) %>%
                eval(envir = .GlobalEnv)
            }) %>% unname()
        }, error = function(e) {
          warning("Hypothesis ", h$id, " has an error in the evaluation criteria for falsification: ", h$falsification$evaluation)
          falsify <<- FALSE
        })
      }
    }

    # summarise conclusions ----
    conc <- ifelse(corrob & !falsify, "c",
                   ifelse(!corrob & falsify, "f", "i"))

    study$hypotheses[[i]][["power"]] <- list(
      corroboration = mean(conc == "c"),
      falsification = mean(conc == "f"),
      inconclusive = mean(conc == "i"),
      criteria = values
    )

    if (scienceverse_options("verbose")) {
      message(sprintf("Hypothesis %s
      corroboration: %03.1f%%
      falsification: %03.1f%%
      inconclusive:  %03.1f%%",
                      h$id,
                      round(100*mean(conc == "c"), 1),
                      round(100*mean(conc == "f"), 1),
                      round(100*mean(conc == "i"), 1)))
    }
  }
  invisible(study)
}


#' Get Power Calculations as List
#'
#' @param study A study list object with class reg_study
#' @param values Whether or not to retun criterion values (default FALSE)
#'
#' @return a list of power (and criteria) values
#' @export
#'
#' @examples
#' study() %>%
#'  add_hypothesis("H1") %>%
#'  add_analysis("A1", t.test(y~A, data = D1)) %>%
#'  add_criterion("C1", "p.value", "<", 0.05) %>%
#'  add_analysis("A2", t.test(y~A, data = D2)) %>%
#'  add_criterion("C2", "p.value", "<", 0.05) %>%
#'  add_eval("corroboration", "", "C1 & C2") %>%
#'  add_eval("falsification", "", "!C1 & !C2") %>%
#'  add_sim_data("D1", between = 2, n = 25, mu = c(0, 0.5)) %>%
#'  add_sim_data("D2", between = 2, n = 50, mu = c(0, 0.5)) %>%
#'  study_power(rep = 100) %>%
#'  get_power()
#'
get_power <- function(study, values = FALSE) {
  power <- list()
  for (h in study$hypotheses) {
    power[[h$id]] <- h$power
    if (!values) {
      power[[h$id]]$criteria <- NULL
    }
  }
  power
}
