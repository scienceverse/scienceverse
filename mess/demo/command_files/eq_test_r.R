#' Equivalence test for a correlation run on data
#'
#' `eq_test_r` runs a equivalence test on a correlation
#'
#' @param data A data table
#' @param col1 The column name for group 1
#' @param col2 The column name for group 2
#' @param alpha Alpha level
#' @param low_eqbound_r Lower equivaence bound
#' @param high_eqbound_r Upper equivalence bound
#' @return Returns a list of results parameters
#' @examples
#'
#' eq_test_r(iris, col1 = "Sepal.Width", col2 = "Sepal.Length", alpha = 0.05, high_eqbound_r = 0.3, low_eqbound_r = -0.3)
#'
#' @export
#'
eq_test_r <- function (data,
                       col1 = "",
                       col2 = "",
                       alpha = alpha,
                       high_eqbound_r = high_eqbound_r,
                       low_eqbound_r = low_eqbound_r){

  x <- data[[col1]]
  y <- data[[col2]]

  r <- cor(x,y)
  n <- length(x)

  # t-test for CI around mean difference
  test_res <- TOSTER::TOSTr(n = n,
                            r = r,
                            high_eqbound_r = high_eqbound_r,
                            low_eqbound_r = low_eqbound_r,
                            alpha = alpha,
                            plot = FALSE,
                            verbose = FALSE)
  invisible(test_res)
}
