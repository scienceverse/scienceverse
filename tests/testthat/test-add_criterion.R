context("test-add_criterion")

test_that("default", {
  s <- study() %>%
    add_hypothesis() %>%
    add_analysis() %>%
    add_criterion("id", result = "p.value", operator = "<", comparator = 0.05)

  cr <- s$hypotheses[[1]]$criteria[[1]]
  expect_equal(cr$id, "id")
  expect_equal(cr$analysis_id, 1)
  expect_equal(cr$result, "p.value")
  expect_equal(cr$operator, "<")
  expect_equal(cr$comparator, 0.05)
})
