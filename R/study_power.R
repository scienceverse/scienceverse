#' Power analysis through simulation
#'
#' This function is experimental. Check power analyses with an external package before using for important decisions.
#'
#' @param study A study list object with class scivrs_study
#' @param rep The number of simulations to run
#'
#' @return A study object with class scivrs_study
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
#'  add_eval("corroboration", "C1 & C2") %>%
#'  add_eval("falsification", "!C1 & !C2") %>%
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
    message("There are no hypotheses.")
  }

  if (length(study$analyses) == 0) {
    stop("There are no analyses")
  }

  # get study environment
  env <- attr(study, "env")

  # check data have designs ----
  simdata <- list()
  for (d in study$data) {
    if (!"design" %in% class(d$design)) {
      if (is.null(d$data)) {
        warning("There is no data or design information for `", d$id, "`. Analyses that require this data are likely to fail.")
      } else {
        message("The data `", d$id, "` will not be simulated, but be used as is for each analysis.")
        # load static data
        assign(d$id, d$data, envir = env)
      }
    } else {
      is_long = isTRUE(d$design$long)

      # simulate new data ----
      message("Simulating Datasets...")
      simdata[[d$id]] <- faux::sim_data(d$design, long = is_long, rep = rep)
    }
  }

  # run analyses ----
  message("Running Analyses...")
  pb <- progress::progress_bar$new(total = rep)
  results <- list()
  dataids <- names(simdata)
  for (i in 1:rep) {
    if (interactive()) { pb$tick() }

    # assign data ----
    for (id in dataids) {
      assign(id, simdata[[id]][["data"]][[i]], envir = env)
    }

    # run analyses ----
    for (a in study$analyses) {
      func <- paste0("analysis_", a$id)
      suppressMessages(
        res <- do.call(func, list(), envir = env) %>% as.list()
      )
      class(res) <- "list"
      results[[a$id]][[i]] <- res
    }
  }

  # record all results ----
  for (a in seq_along(study$analyses)) {
    resnames <- names(results[[a]][[1]])
    names(resnames) <- resnames
    resvals <- lapply(resnames, function(x) {
      sapply(results[[a]], `[[`, x)
    })

    study$analyses[[a]]$power <- resvals
  }


  # evaluate each hypothesis ----
  message("Evaluating Hypotheses...")
  for (i in 1:length(study$hypotheses)) {
    h <- study$hypotheses[[i]]
    # evaluate each criterion ----
    criteria_n <- length(h$criteria)
    if (criteria_n == 0) {
      message("Hypothesis ", h$id, " has no criteria")
    } else {
      criteria <- list()
      values <- list()
      analysis_ids <- sapply(study$analyses, function(x) {x$id})
      for (j in 1:criteria_n) {
        criterion <- h$criteria[[j]]
        analysis <- match(criterion$analysis_id, analysis_ids)

        # get result and comparator values from results
        value <- c()
        comp_value <- c()
        for (k in 1:rep) {
          kres <- results[[criterion$analysis_id]][[k]]
          value[k] <- get_res_value(criterion$result, kres)
          comp_value[k] <- get_res_value(criterion$comparator, kres)
        }

        if (criterion$operator == "<") {
          conclusion <- value < comp_value
        } else if (criterion$operator == ">") {
          conclusion <- value > comp_value
        } else if (criterion$operator == "=") {
          conclusion <- value == comp_value
        } else if (criterion$operator == "!=") {
          conclusion <- value != comp_value
        } else {
          conclusion <- NA
        }

        values[[criterion$id]] <- value
        criteria[[criterion$id]] <- conclusion
      }

      # evaluate hypothesis ----
      replacement <- sapply(criteria, as.character)

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
          cp <- list()
          for (j in 1:rep) {
            cp[[j]] <- corrob_pieces
          }
          for (pc in potential_criteria) {
            crit <- corrob_pieces[pc]
            if (crit %in% names(replacement[1,])) {
              for (j in 1:rep) {
                cp[[j]][pc] <- replacement[j,crit]
              }
            } else {
              stop(crit, " is not a defined criterion")
            }
          }

          corrob <- sapply(cp, function(x) {
            paste(x, collapse = "") %>%
              parse(text = .) %>%
              eval(envir = .GlobalEnv)
          })
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
          fp <- list()
          for (j in 1:rep) {
            fp[[j]] <- falsify_pieces
          }
          for (pc in potential_criteria) {
            crit <- falsify_pieces[pc]
            if (crit %in% names(replacement[1,])) {
              for (j in 1:rep) {
                fp[[j]][pc] <- replacement[j,crit]
              }
            } else {
              stop(crit, " is not a defined criterion")
            }
          }

          falsify <- sapply(fp, function(x) {
            paste(x, collapse = "") %>%
              parse(text = .) %>%
              eval(envir = .GlobalEnv)
          })
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
      inconclusive = mean(conc == "i")
    )

    message(sprintf("Hypothesis %s
      corroboration: %03.1f%%
      falsification: %03.1f%%
      inconclusive:  %03.1f%%",
                      h$id,
                      round(100*mean(conc == "c"), 1),
                      round(100*mean(conc == "f"), 1),
                      round(100*mean(conc == "i"), 1)))
  }
  invisible(study)
}


#' Get Power Calculations as List
#'
#' @param study A study list object with class scivrs_study
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
#'  add_eval("corroboration", "C1 & C2") %>%
#'  add_eval("falsification", "!C1 & !C2") %>%
#'  add_sim_data("D1", between = 2, n = 25, mu = c(0, 0.5)) %>%
#'  add_sim_data("D2", between = 2, n = 50, mu = c(0, 0.5)) %>%
#'  study_power(rep = 100) %>%
#'  get_power()
#'
get_power <- function(study, values = FALSE) {
  power <- list()

  power$power <- lapply(study$hypotheses, `[[`, "power")
  names(power$power) <- lapply(study$hypotheses, `[[`, "id")

  if (values) {
    power$results <- lapply(study$analyses, `[[`, "power")
    names(power$results) <- lapply(study$analyses, `[[`, "id")
  }

  power
}
