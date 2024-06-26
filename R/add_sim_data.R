#' Add simulated data
#'
#' Uses \code{faux::sim_design()} to generate a data table with a specified within and between design.
#'
#' @param study A study list object with class scivrs_study
#' @param data_id The id for this dataset (index or character) if a dataset with this id already exists, it will overwrite it
#' @param within a list of the within-subject factors
#' @param between a list of the between-subject factors
#' @param n the number of samples required
#' @param mu the means of the variables
#' @param sd the standard deviations of the variables
#' @param r the correlations among the variables (can be a single number, full correlation matrix as a matrix or vector, or a vector of the upper right triangle of the correlation matrix
#' @param empirical logical. If true, mu, sd and r specify the empirical not population mean, sd and covariance
#' @param long Whether the returned tbl is in wide (default = FALSE) or long (TRUE) format
#' @param dv the name of the dv for long plots (defaults to y)
#' @param id the name of the id column (defaults to id)
#' @param plot whether to show a plot of the design
#' @param rep the number of data frames to return (default 1); if greater than 1, the returned data frame is nested by rep
#'
#' @return A study object with class scivrs_study
#'
#' @examples
#'
#' s <- study() %>%
#'   add_sim_data("dat", within = list(time = c("day", "night")),
#'                between = list(pet = c("cat", "dog")),
#'                n = 10, mu = 100, sd = 10, r = 0.5)
#' s$data[[1]]$data
#'
#' @export

add_sim_data <- function(study, data_id,
                         within = list(), between = list(),
                         n = 100, mu = 0, sd = 1, r = 0,
                         empirical = FALSE, long = FALSE,
                         dv = list(y = "value"),
                         id = list(id = "id"),
                         plot = FALSE,
                         rep = 1) {

  design <- faux::check_design(
    within = within, between = between,
    n = n, mu = mu, sd = sd, r = r, dv = dv, id = id,
    plot = plot)
  design$long <- long

  dat <- faux::sim_design(
    design = design,
    empirical = empirical,
    long = long,
    plot = plot,
    rep = rep)

  if (long) {
    vardesc <- list(levels = c(between, within))
    vardesc[["description"]] <- id
  } else {
    vardesc <- list(levels = between)
    vardesc[["description"]] <- c(id, dv)
  }

  study <- add_data(study, data_id, data = dat,
                    vardesc = vardesc, design = design)

  invisible(study)
}
