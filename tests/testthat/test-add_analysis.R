context("test-add_analysis")

test_that("defaults", {
  s <- study() %>%
    add_analysis()

  expect_equal(length(s$analyses), 1)
  expect_equal(s$analyses[[1]]$id, 1)
  expect_equal(s$analyses[[1]]$func, "list")
  expect_equal(s$analyses[[1]]$params, list())
  expect_null(s$analyses[[1]]$code)
})

test_that("set values", {
  params <- list(data = ".data")
  s <- study() %>%
    add_analysis("t.test", params, id= "A1")

  expect_equal(s$analyses[[1]]$id, "A1")
  expect_equal(s$analyses[[1]]$func, "t.test")
  expect_equal(s$analyses[[1]]$params, params)
  expect_null(s$analyses[[1]]$code)
})

# custom ----
test_that("custom", {
  mean_abs_diff <<- function(x, y) {
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
    ))

  expect_equal(s$analyses[[1]]$code, mean_abs_diff)
  expect_equal(s$analyses[[1]]$func, "mean_abs_diff")
  expect_equal(s$analyses[[1]]$params %>% names(), c("x", "y"))
  expect_equal(s$analyses[[1]]$params$x, ".data[1]$Petal.Width")
  expect_equal(s$analyses[[1]]$params$y, ".data[1]$Petal.Length")

  study_save(s, "demotext.json")
  rm(mean_abs_diff, envir = .GlobalEnv)
  study <- study("demotext.json")
  file.remove("demotext.json")

  expect_true(exists("mean_abs_diff"))
  expect_true(is.function(mean_abs_diff))
})

# add from package
test_that("add from package", {
  s <- study() %>%
    add_analysis("stats::sd", list(x = 1:10)) %>%
    add_hypothesis() %>%
    add_criterion(1, ">", 0)

  expect_equal(s$analyses[[1]]$func, "stats::sd")

  s <- study_analyse(s)

  expect_equal(s$hypotheses[[1]]$criteria[[1]]$conclusion, TRUE)
  expect_equal(s$analyses[[1]]$results[[1]], sd(1:10))

  s <- study() %>% add_analysis("lme4::lmer")
  expect_equal(s$analyses[[1]]$func, "lme4::lmer")
})

# add from file
test_that("add from file", {
  testthat::skip("only works in testthat")

  s <- study() %>%
    add_hypothesis(id = "H1") %>%
    add_analysis("../custom_code_test.R",
                 params = list(a = 1, b = 2),
                 return = "answer",
                 id = "A1") %>%
    add_criterion("answer", "=", 3, "H1", "A1") %>%
    study_analyse()

  expect_equal(s$analyses[[1]]$results, list(answer = 3))
  expect_equal(s$hypotheses[[1]]$criteria[[1]]$conclusion, TRUE)
  expect_equal(s$hypotheses[[1]]$conclusion, TRUE)
})
