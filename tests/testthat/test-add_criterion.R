context("test-add_criterion")

test_that("default", {
  s <- study() %>%
    add_hypothesis() %>%
    add_analysis(NULL, t.test(rnorm(100))) %>%
    add_criterion("id", result = "p.value", operator = "<", comparator = 0.05)

  cr <- s$hypotheses[[1]]$criteria[[1]]
  expect_equal(cr$id, "id")
  expect_equal(cr$analysis_id, 1)
  expect_equal(cr$result, "p.value")
  expect_equal(cr$operator, "<")
  expect_equal(cr$comparator, 0.05)
})

test_that("default", {
  s <- study() %>%
    add_hypothesis("A") %>%
    add_analysis("A1", t.test(rnorm(100))) %>%
    add_analysis("A2", t.test(rnorm(100))) %>%
    add_criterion("A2", "p.value", "<", 0.12) %>%
    add_hypothesis("B") %>%
    add_analysis("B1", t.test(rnorm(100))) %>%
    add_analysis("B2", t.test(rnorm(100))) %>%
    add_criterion("B1", "p.value", "<", 0.21, 1, 2) %>%
    add_criterion("B2", "p.value", "<", 0.22, 2, 2) %>%
    add_criterion("A1", "p.value", "<", 0.11, 1, 1)

  A1 <- s$hypotheses[[1]]$criteria[[2]]
  expect_equal(A1$id, "A1")
  expect_equal(A1$analysis_id, "A1")
  expect_equal(A1$result, "p.value")
  expect_equal(A1$operator, "<")
  expect_equal(A1$comparator, 0.11)

  A2 <- s$hypotheses[[1]]$criteria[[1]]
  expect_equal(A2$id, "A2")
  expect_equal(A2$analysis_id, "A2")
  expect_equal(A2$result, "p.value")
  expect_equal(A2$operator, "<")
  expect_equal(A2$comparator, 0.12)

  B1 <- s$hypotheses[[2]]$criteria[[1]]
  expect_equal(B1$id, "B1")
  expect_equal(B1$analysis_id, "A1")
  expect_equal(B1$result, "p.value")
  expect_equal(B1$operator, "<")
  expect_equal(B1$comparator, 0.21)

  B2 <- s$hypotheses[[2]]$criteria[[2]]
  expect_equal(B2$id, "B2")
  expect_equal(B2$analysis_id, "A2")
  expect_equal(B2$result, "p.value")
  expect_equal(B2$operator, "<")
  expect_equal(B2$comparator, 0.22)


  s <- s %>%
    update_criterion("A1", NULL,       NULL, 0.31, "A1", "A", "C1") %>%
    update_criterion("A2", "statistic", "=", 0.32, "A2", "A", "C2") %>%
    update_criterion("B1", "statistic", "=", 0.41, "A1", "B", "D1") %>%
    update_criterion("B2", "statistic", "=", 0.42, "A2", "B", "D2")

  A1 <- s$hypotheses[[1]]$criteria[[2]]
  expect_equal(A1$id, "C1")
  expect_equal(A1$analysis_id, "A1")
  expect_equal(A1$result, "p.value")
  expect_equal(A1$operator, "<")
  expect_equal(A1$comparator, 0.31)

  A2 <- s$hypotheses[[1]]$criteria[[1]]
  expect_equal(A2$id, "C2")
  expect_equal(A2$analysis_id, "A2")
  expect_equal(A2$result, "statistic")
  expect_equal(A2$operator, "=")
  expect_equal(A2$comparator, 0.32)

  B1 <- s$hypotheses[[2]]$criteria[[1]]
  expect_equal(B1$id, "D1")
  expect_equal(B1$analysis_id, "A1")
  expect_equal(B1$result, "statistic")
  expect_equal(B1$operator, "=")
  expect_equal(B1$comparator, 0.41)

  B2 <- s$hypotheses[[2]]$criteria[[2]]
  expect_equal(B2$id, "D2")
  expect_equal(B2$analysis_id, "A2")
  expect_equal(B2$result, "statistic")
  expect_equal(B2$operator, "=")
  expect_equal(B2$comparator, 0.42)
})
