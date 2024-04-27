test_that("sim_data", {
  s <- study() %>%
    add_sim_data("dat", within = 2, between = 2,
                 n = 10, mu = 100, sd = 10, r = 0.5,
                 long = TRUE, plot = FALSE)

  dat <- s$data[[1]]$data

  expect_equal(names(dat), c("id", "B1", "W1", "y"))

  des <- s$data[[1]]$design
  cdes <- faux::check_design(2, 2, 10, 100, 10, 0.5, plot = FALSE)
  cdes$long <- TRUE

  expect_equal(des, cdes)

  expect_equal(unlist(des$mu), c(B1a.W1a = 100, B1a.W1b = 100, B1b.W1a = 100, B1b.W1b = 100))
  expect_equal(unlist(des$sd) %>% sum(), 40)
  expect_equal(unlist(des$n), c(B1a = 10, B1b = 10))
  mat <- matrix(c(1.0, 0.5, 0.5, 1.0), 2, dimnames = list(c("W1a", "W1b"), c("W1a", "W1b")))
  expect_equal(des$r$B1a, mat)
  expect_equal(des$r$B1b, mat)
  expect_equal(unlist(des$within), c(W1.W1a = "W1a", W1.W1b = "W1b"))
  expect_equal(unlist(des$between), c(B1.B1a = "B1a", B1.B1b = "B1b"))
})
