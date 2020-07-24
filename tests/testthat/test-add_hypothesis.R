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
    add_analysis("A1") %>%
    add_hypothesis("H1", "My test hypothesis 1") %>%
    add_criterion("p", "p.value", "<", 0.05, "A1", "H1") %>%
    add_hypothesis("H1", "My test hypothesis 1 - revised")

  expect_equal(length(s$hypotheses), 1)
  expect_equal(s$hypotheses[[1]]$id, "H1")
  expect_equal(s$hypotheses[[1]]$description, "My test hypothesis 1 - revised")
  expect_equal(length(s$hypotheses[[1]]$criteria), 0)

})

test_that("update hypothesis", {
  s <- study()

  expect_error(update_hypothesis(s, 1, "New", "New"),
                 "The study does not have a hypothesis with the ID 1")
  expect_error(update_hypothesis(s, "H1", "New", "New"),
                 "The study does not have a hypothesis with the ID H1")

  s <- study() %>%
    add_hypothesis("H1", "My test hypothesis 1") %>%
    add_analysis("A1") %>%
    add_criterion("p", "p.value", "<", 0.05, "A1", "H1") %>%
    update_hypothesis(1, "New desc", "NewID")

  h <- s$hypotheses[[1]]

  expect_equal(h$id, "NewID")
  expect_equal(h$description, "New desc")
  expect_equal(length(h$criteria), 1)
  expect_equal(h$criteria[[1]]$id, "p")

  s <- update_hypothesis(s, "NewID", "Newer desc", "NewerID")
  h <- s$hypotheses[[1]]

  expect_equal(h$id, "NewerID")
  expect_equal(h$description, "Newer desc")
  expect_equal(length(h$criteria), 1)
  expect_equal(h$criteria[[1]]$id, "p")

})
