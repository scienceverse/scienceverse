context("test-study_analyse")

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
