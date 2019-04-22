context("test-add_analysis")

test_that("defaults", {
  s <- study() %>%
    add_analysis()

  expect_equal(length(s$analyses), 1)
  expect_null(s$analyses[[1]]$id)
  expect_equal(s$analyses[[1]]$func, "list")
  expect_equal(s$analyses[[1]]$params, list())
  expect_null(s$analyses[[1]]$code)
})

test_that("set values", {
  params <- list(data = ".data")
  code <- "m <- function(data) { mean(data[[1]]) }"
  s <- study() %>%
    add_analysis("m", params, code, "A1")

  expect_equal(s$analyses[[1]]$id, "A1")
  expect_equal(s$analyses[[1]]$func, "m")
  expect_equal(s$analyses[[1]]$params, params)
  expect_equal(s$analyses[[1]]$code, code)
})
