test_that("multiplication works", {
  s <- study() %>%
    add_sim_data("dat", within = 2, between = 2,
                 n = 10, mu = 100, sd = 10, r = 0.5,
                 long = TRUE, seed = 8675309)

  dat <- s$data[[1]]$data

  expect_equal(names(dat), c("id", "B", "A", "y"))
})
