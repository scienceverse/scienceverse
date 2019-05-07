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

# custom ----
test_that("custom", {
  mean_abs_diff <- function(x, y) {
    (x - y) %>%
      abs() %>%
      mean() %>%
      magrittr::set_names("mean_abs_diff") %>%
      as.list()
  }

  s <- study() %>%
    add_hypothesis() %>%
    add_analysis("mean_abs_diff", list(
      x = ".data[1]$Petal.Width",
      y = ".data[1]$Petal.Length"
    ), mean_abs_diff)

  expect_equal(s$analyses[[1]]$code, mean_abs_diff)
  expect_equal(s$analyses[[1]]$func, "mean_abs_diff")
  expect_equal(s$analyses[[1]]$params %>% names(), c("x", "y"))
  expect_equal(s$analyses[[1]]$params$x, ".data[1]$Petal.Width")
  expect_equal(s$analyses[[1]]$params$y, ".data[1]$Petal.Length")
})
