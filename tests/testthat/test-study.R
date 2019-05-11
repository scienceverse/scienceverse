context("test-study")

test_that("default study", {
  s <- study()

  expect_equal(s$name, "Demo Study")
  expect_equal(s$hypotheses, list())
  expect_equal(s$methods, list())
  expect_equal(s$data, list())
  expect_equal(s$prep, list())
  expect_equal(s$analyses, list())
  expect_s3_class(s, "list")
  expect_s3_class(s, "reg_study")
})

test_that("study with name", {
  s <- study("Test Name")

  expect_equal(s$name, "Test Name")
})

test_that("study from json", {
  mean_abs_diff <- function(x, y) {
    (x - y) %>%
      abs() %>%
      mean() %>%
      magrittr::set_names("mean_abs_diff") %>%
      as.list()
  }

  s <- study() %>%
    add_analysis("mean_abs_diff", list(
      x = ".data[1]$Petal.Width",
      y = ".data[1]$Petal.Length"
    ))

  # remove function to force load from code
  study_save(s, "test.json")
  rm(mean_abs_diff)
  s2 <- study("test.json")
  file.remove("test.json")
})

test_that("study from json - error", {
  expect_error(s <- study() %>%
    add_analysis("not_there", list(
      x = ".data[1]$Petal.Width",
      y = ".data[1]$Petal.Length"
    )), "The function not_there is not defined")
})
