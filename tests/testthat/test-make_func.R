test_that("make_func", {
  make_func("myfunc", "t.test(rnorm(100))", "", environment())
  myfunc2 <- function() { t.test(rnorm(100)) }
  expect_equal(myfunc, myfunc2)

  remove(myfunc)
  remove(myfunc2)
})

test_that("study env", {
  s <- study()
  e <- attr(s, "env")

  make_func("myfunc", "t.test(rnorm(100))", "", e)
  myfunc2 <- function() { t.test(rnorm(100)) }
  expect_equal(e$myfunc, myfunc2)

  remove(myfunc, envir = e)
  remove(myfunc2)
})

test_that("unnamed return list", {
  code <- "{
    a <- 1
    b <- 2
    c <- 3
  }"
  make_func("myfunc", code, list("a", "b", "c"), environment())
  x <- myfunc()
  expect_equal(x, list("a" = 1, "b" = 2, "c" = 3))

  remove(myfunc)
})

test_that("named return list", {
  code <- "{
    a <- 1
    b <- 2
    c <- 3
    d <- list(e = 4, f = 5)
  }"
  make_func("myfunc", code, list(a = "a", b = "b", e = "d$e", g = "c/2"), environment())
  x <- myfunc()
  expect_equal(x, list("a" = 1, "b" = 2, "e" = 4, "g" = 1.5))

  remove(myfunc)
})

test_that("return vector", {
  code <- "{
    a <- 1
    b <- 2
    c <- 3
  }"
  make_func("myfunc", code, c("a", "b", "c"), environment())
  x <- myfunc()
  expect_equal(x, list("a" = 1, "b" = 2, "c" = 3))

  remove(myfunc)
})

test_that("code from match.call", {
  get_func <- function(x) { match.call()$x }
  code <- get_func({
    dat <- iris
    t.test(dat$Sepal.Width)
  }) %>% utils::capture.output()
  make_func("myfunc", code, "", environment())
  x <- myfunc()
  dat <- iris
  y <- t.test(dat$Sepal.Width)

  expect_equal(x, y)
})

test_that("bad function names", {
  make_func("my *BAD* func", "t.test(rnorm(100))", "", environment())
  expect_true(exists("my_BAD_func"))
  remove(my_BAD_func)
})

# add function with parse error ----
test_that("parse error", {
  expect_error(make_func("f", "x <-"),
               "The function f has errors.")
})
