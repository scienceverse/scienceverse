context("test-add_hypothesis")

test_that("defaults", {
  s <- study() %>%
    add_hypothesis()

  expect_equal(length(s$hypotheses), 1)
  expect_equal(s$hypotheses[[1]]$id, 1)
  expect_equal(s$hypotheses[[1]]$description, "Describe your hypothesis")
  expect_equal(s$hypotheses[[1]]$criteria, list())
  expect_equal(s$hypotheses[[1]]$evaluation, "&")
})

test_that("set values", {
  s <- study() %>%
    add_hypothesis("My test hypothesis", "|", "H1")

  expect_equal(s$hypotheses[[1]]$id, "H1")
  expect_equal(s$hypotheses[[1]]$description, "My test hypothesis")
  expect_equal(s$hypotheses[[1]]$evaluation, "|")
})

test_that("multiple hypotheses", {
  s <- study() %>%
    add_hypothesis("My test hypothesis 1", "|", "H1") %>%
    add_hypothesis("My test hypothesis 2", "&","H2")

  expect_equal(s$hypotheses[[1]]$id, "H1")
  expect_equal(s$hypotheses[[1]]$description, "My test hypothesis 1")
  expect_equal(s$hypotheses[[1]]$evaluation, "|")

  expect_equal(s$hypotheses[[2]]$id, "H2")
  expect_equal(s$hypotheses[[2]]$description, "My test hypothesis 2")
  expect_equal(s$hypotheses[[2]]$evaluation, "&")
})
