context("test-get_idx")

test_that("defaults", {
  # no sections exist yet, so should always return 1
  s <- study()

  expect_equal(get_idx(s, "test", "hypotheses"), 1)
  expect_equal(get_idx(s, "test", "analyses"), 1)
  expect_equal(get_idx(s, "test", "data"), 1)

  expect_equal(get_idx(s, 1, "hypotheses"), 1)
  expect_equal(get_idx(s, 1, "analyses"), 1)
  expect_equal(get_idx(s, 1, "data"), 1)

  expect_equal(get_idx(s, 10, "hypotheses"), 1)
  expect_equal(get_idx(s, 10, "analyses"), 1)
  expect_equal(get_idx(s, 10, "data"), 1)
})

test_that("set", {
  s <- study() %>%
    add_hypothesis(id="A") %>%
    add_hypothesis(id="B") %>%
    add_analysis(id="C", t.test(rnorm(100))) %>%
    add_analysis(id="D", t.test(rnorm(100))) %>%
    add_data(id="E") %>%
    add_data(id="F")

  # return correct idx for id
  expect_equal(get_idx(s, "A", "hypotheses"), 1)
  expect_equal(get_idx(s, "B", "hypotheses"), 2)
  expect_equal(get_idx(s, "C", "analyses"), 1)
  expect_equal(get_idx(s, "D", "analyses"), 2)
  expect_equal(get_idx(s, "E", "data"), 1)
  expect_equal(get_idx(s, "F", "data"), 2)

  # return same idx for idx
  expect_equal(get_idx(s, 1, "hypotheses"), 1)
  expect_equal(get_idx(s, 2, "hypotheses"), 2)
  expect_equal(get_idx(s, 1, "analyses"), 1)
  expect_equal(get_idx(s, 2, "analyses"), 2)
  expect_equal(get_idx(s, 1, "data"), 1)
  expect_equal(get_idx(s, 2, "data"), 2)

  # return next idx for non-existing item ids
  expect_equal(get_idx(s, "test", "hypotheses"), 3)
  expect_equal(get_idx(s, "test", "analyses"), 3)
  expect_equal(get_idx(s, "test", "data"), 3)

  # return next idx for non-existing item idxs
  expect_equal(get_idx(s, 5, "hypotheses"), 3)
  expect_equal(get_idx(s, 6, "analyses"), 3)
  expect_equal(get_idx(s, 10, "data"), 3)
})
