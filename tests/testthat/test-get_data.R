test_that("basic", {
  dat1 <- data.frame(x = 1:10)

  s <- study() %>%
    add_data("test", dat1)

  dat2 <- get_data(s, "test")

  expect_equal(dat1, dat2)
})

test_that("from sim_data", {
  s <- study() %>%
    add_sim_data("test", within = 2, between = 2)

  dat <- get_data(s, "test")

  expect_equal(names(dat), c("id", "B1", "W1a", "W1b"))
  expect_equal(nrow(dat), 200)
})

test_that("from sim_data reps", {
  s <- study() %>%
    add_sim_data("test", within = 2, between = 2, rep = 10)

  dat <- get_data(s, "test")

  expect_equal(names(dat), c("rep", "data"))
  expect_equal(nrow(dat), 10)
})
