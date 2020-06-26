#' T-test with N (formula)
#'
#' See \code{stats::\link[stats]{t.test}} for details.
#' @export
#'
t_test <- function(x, ...) UseMethod("t_test")

#' T-test with N
#'
#' See \code{stats::\link[stats]{t.test}} for details.
#' @export
#'
t_test.default <- function(x, y = NULL,
                   alternative = c("two.sided", "less", "greater"),
                   mu = 0, paired = FALSE, var.equal = FALSE,
                   conf.level = 0.95, names = NULL, ...) {
  res <- t.test(x, y, alternative, mu, paired, var.equal, conf.level, ...)

  if (!is.null(y) & is.null(names)) {
    xname <- utils::capture.output(match.call()$x)
    yname <- utils::capture.output(match.call()$y)
    names <- c(xname, yname)
  }

  res$n <- t_test_n(x, y, paired, names)

  res
}

# t_test.formula <- function(formula, data, ...) {
#   res <- t.test(formula, data, ...)
#
#   f <- as.character(formula)
#   lvls <- unique(data[,f[3]])
#   x <- data[which(data[,f[3]] == lvls[1]), f[2]]
#   y <- data[which(data[,f[3]] == lvls[2]), f[2]]
#   paired <- isTRUE(list(...)$paired)
#
#   n <- t_test_n(x, y, paired)
#   c(res, n)
# }

#' T-test with N (formula)
#'
#' See \code{stats::\link[stats]{t.test}} for details.
#' @export
#'
t_test.formula <- function (formula, data, subset, na.action, ...)
{
  if (missing(formula) || (length(formula) != 3L))
    stop("'formula' missing or incorrect")
  oneSampleOrPaired <- FALSE
  if (length(attr(terms(formula[-2L]), "term.labels")) != 1L)
    if (formula[[3]] == 1L)
      oneSampleOrPaired <- TRUE
  else stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  if (!oneSampleOrPaired) {
    g <- factor(mf[[-response]])
    if (nlevels(g) != 2L)
      stop("grouping factor must have exactly 2 levels")
    DATA <- setNames(split(mf[[response]], g), c("x", "y"))
    y <- do.call("t_test", c(DATA, list(...)))
    if (length(y$estimate) == 2L) {
      names(y$estimate) <- paste("mean in group", levels(g))
      names(y$n) <- levels(g)
    }
  }
  else {
    respVar <- mf[[response]]
    if (inherits(respVar, "Pair")) {
      DATA <- list(x = respVar[, 1], y = respVar[, 2],
                   paired = TRUE)
      y <- do.call("t_test", c(DATA, list(...)))
    }
    else {
      DATA <- list(x = respVar)
      y <- do.call("t_test", c(DATA, list(...)))
    }
  }
  y$data.name <- DNAME
  y
}

#' Calculate Ns for t_test
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param y n optional (non-empty) numeric vector of data values.
#' @param paired a logical indicating whether you want a paired t-test.
#'
#' @return list of Ns
#' @internal
#'
t_test_n <- function(x, y = NULL, paired = FALSE, names = NULL) {
  if (is.null(y)) { # 1-sample
    n <- sum(!is.na(x))
  } else {
    not_na1 <- !is.na(x)
    not_na2 <- !is.na(y)
    if (paired) {   # paired
      n <- sum(not_na1 & not_na2)
    } else {        # 2-sample
      n <- c(sum(not_na1), sum(not_na2))
      names(n) <- names[1:2]
    }
  }
  n
}
