context("test-get_id_idx")

test_that("warnings", {
  expect_warning(
    study() %>% get_id_idx(),
    "No hypotheses items exist. Creating a default item with id = NULL"
  )

  expect_warning(
    study() %>% get_id_idx(1),
    "No hypotheses items exist. Creating a default item with id = 1"
  )

  expect_warning(
    study() %>% get_id_idx(10),
    "No hypotheses items exist. Creating a default item with id = 1"
  )

  expect_warning(
    study() %>% get_id_idx("test"),
    "No hypotheses items exist. Creating a default item with id = test"
  )
})

test_that("warnings", {
  i <- study() %>% add_hypothesis() %>% get_id_idx()
  expect_equal(i$id, 1)
  expect_equal(i$idx, 1)

  i <- study() %>% add_hypothesis(id="test") %>% get_id_idx(1)
  expect_equal(i$id, "test")
  expect_equal(i$idx, 1)

  i <- study() %>% add_hypothesis(id="test") %>% get_id_idx("test")
  expect_equal(i$id, "test")
  expect_equal(i$idx, 1)
})

test_that("correct", {
  s <- study() %>%
    add_hypothesis("H1") %>%
    add_hypothesis("H2") %>%
    add_analysis("A1") %>%
    add_analysis("A2") %>%
    add_data("D1") %>%
    add_data("D2")

  expect_equal(get_id_idx(s),
               list(id = "H2", idx = 2))
  expect_equal(get_id_idx(s, "H1"),
               list(id = "H1", idx = 1))
  expect_equal(get_id_idx(s, "H2"),
               list(id = "H2", idx = 2))
  expect_equal(get_id_idx(s, 1),
               list(id = "H1", idx = 1))
  expect_equal(get_id_idx(s, 2),
               list(id = "H2", idx = 2))
  expect_warning(i <- get_id_idx(s, 3),
                 "No hypotheses item with index = 3 exists. Creating a default item at index = 3", fixed = TRUE)
  expect_equal(i, list(id = 3, idx = 3))

  expect_equal(get_id_idx(s, NULL, "analyses"),
               list(id = "A2", idx = 2))
  expect_equal(get_id_idx(s, "A1", "analyses"),
               list(id = "A1", idx = 1))
  expect_equal(get_id_idx(s, "A2", "analyses"),
               list(id = "A2", idx = 2))

  expect_equal(get_id_idx(s, NULL, "data"),
               list(id = "D2", idx = 2))
  expect_equal(get_id_idx(s, "D1", "data"),
               list(id = "D1", idx = 1))
  expect_equal(get_id_idx(s, "D2", "data"),
               list(id = "D2", idx = 2))
})
