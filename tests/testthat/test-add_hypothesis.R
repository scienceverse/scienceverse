context("test-add_hypothesis")

test_that("defaults", {
  s <- study() %>%
    add_hypothesis()

  expect_equal(length(s$hypotheses), 1)
  expect_equal(s$hypotheses[[1]]$id, 1)
  expect_equal(s$hypotheses[[1]]$description, "Describe your hypothesis")
  expect_equal(s$hypotheses[[1]]$criteria, list())
  expect_equal(s$hypotheses[[1]]$falsification, list())
  expect_equal(s$hypotheses[[1]]$corroboration, list())
})

test_that("set values", {
  s <- study() %>%
    add_hypothesis("H1", "My test hypothesis")

  expect_equal(s$hypotheses[[1]]$id, "H1")
  expect_equal(s$hypotheses[[1]]$description, "My test hypothesis")
})

test_that("multiple hypotheses", {
  s <- study() %>%
    add_hypothesis("H1", "My test hypothesis 1") %>%
    add_hypothesis("H2", "My test hypothesis 2")

  expect_equal(s$hypotheses[[1]]$id, "H1")
  expect_equal(s$hypotheses[[1]]$description, "My test hypothesis 1")

  expect_equal(s$hypotheses[[2]]$id, "H2")
  expect_equal(s$hypotheses[[2]]$description, "My test hypothesis 2")
})

test_that("multiple unnamed hypotheses", {
  s <- study() %>%
    add_hypothesis(description = "My test hypothesis 1") %>%
    add_hypothesis(description = "My test hypothesis 2")

  expect_equal(s$hypotheses[[1]]$id, 1)

  expect_equal(s$hypotheses[[2]]$id, 2)
})

test_that("overwrite hypothesis", {
  s <- study() %>%
    add_hypothesis("H1", "My test hypothesis 1") %>%
    add_hypothesis("H1", "My test hypothesis 1 - revised")

  expect_equal(s$hypotheses[[1]]$id, "H1")
  expect_equal(s$hypotheses[[1]]$description, "My test hypothesis 1 - revised")
  expect_equal(length(s$hypotheses), 1)
})
