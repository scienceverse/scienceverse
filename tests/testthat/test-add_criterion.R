context("test-add_criterion")

test_that("default", {
  s <- study() %>%
    add_hypothesis() %>%
    add_analysis() %>%
    add_criterion(result = "p.value", operator = "<", comparator = 0.05)

  expect_equal(s$hypotheses[[1]]$criteria[[1]]$hypothesis_id, 1)
  expect_equal(s$hypotheses[[1]]$criteria[[1]]$analysis_id, 1)
  expect_equal(s$hypotheses[[1]]$criteria[[1]]$result, "p.value")
  expect_equal(s$hypotheses[[1]]$criteria[[1]]$operator, "<")
  expect_equal(s$hypotheses[[1]]$criteria[[1]]$comparator, 0.05)
})
