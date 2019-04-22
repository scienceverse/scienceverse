context("test-get_id_idx")

test_that("warnings", {
  testthat::expect_warning(
    study() %>% get_id_idx(),
    "No hypotheses items exist. Creating a default item with id = NULL"
  )

  testthat::expect_warning(
    study() %>% get_id_idx(1),
    "No hypotheses items exist. Creating a default item with id = 1"
  )

  testthat::expect_warning(
    study() %>% get_id_idx(10),
    "No hypotheses items exist. Creating a default item with id = 1"
  )

  testthat::expect_warning(
    study() %>% get_id_idx("test"),
    "No hypotheses items exist. Creating a default item with id = test"
  )
})

test_that("warnings", {
  i <- study() %>% add_hypothesis() %>% get_id_idx()
  testthat::expect_equal(i$id, 1)
  testthat::expect_equal(i$idx, 1)

  i <- study() %>% add_hypothesis("test") %>% get_id_idx(1)
  testthat::expect_equal(i$id, "test")
  testthat::expect_equal(i$idx, 1)

  i <- study() %>% add_hypothesis("test") %>% get_id_idx("test")
  testthat::expect_equal(i$id, "test")
  testthat::expect_equal(i$idx, 1)
})
