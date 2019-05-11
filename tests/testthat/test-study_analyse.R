context("test-study_analyse")

# errors ----
test_that("errors", {
  expect_error(study() %>% add_analysis("wrong_test", list()),
               "The function wrong_test is not defined")
})


# defaults ----
test_that("defaults", {
  s <- study() %>%
    add_hypothesis() %>%
    add_analysis("cor.test", list(
      x = ".data[1]$Petal.Width",
      y = ".data[1]$Petal.Length"
    )) %>%
    add_criterion(
      result = "p.value",
      operator = "<",
      comparator = 0.05) %>%
    add_data(iris) %>%
    study_analyse()

  calc_res <- s$analyses[[1]]$results
  true_res <- cor.test(iris$Petal.Width, iris$Petal.Length)

  expect_named(calc_res, names(true_res), ignore.order = TRUE)

  # data.name is specified differently, so won't match
  names <- names(true_res)
  names <- names[names != "data.name"]

  for (name in names) {
    expect_equal(calc_res[[name]], true_res[[name]])
  }
})

# alias ----
test_that("alias", {
  s <- study() %>%
    add_hypothesis() %>%
    add_analysis("cor.test", list(
      x = ".data[1]$Petal.Width",
      y = ".data[1]$Petal.Length"
    )) %>%
    add_criterion(result = "p.value", operator = "<", comparator = 0.05) %>%
    add_data(iris) %>%
    study_analyze()

  calc_res <- s$analyses[[1]]$results
  true_res <- cor.test(iris$Petal.Width, iris$Petal.Length)

  expect_named(calc_res, names(true_res), ignore.order = TRUE)

  # data.name is specified differently, so won't match
  names <- names(true_res)
  names <- names[names != "data.name"]

  for (name in names) {
    expect_equal(calc_res[[name]], true_res[[name]])
  }
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
    )) %>%
    add_criterion(result = "mean_abs_diff", operator = ">", comparator = 1)

  # remove function to force load from code
  study_save(s, "demotext.json")
  rm(mean_abs_diff)
  s2 <- study("demotext.json") %>%
    add_data(iris) %>%
    study_analyse()
  file.remove("demotext.json")

  calc_res <- s2$analyses[[1]]$results
  comp_res <- (iris$Petal.Width - iris$Petal.Length) %>%
    abs() %>% mean()

  expect_equal(calc_res$mean_abs_diff, comp_res)
})
