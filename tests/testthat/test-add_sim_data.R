test_that("sim_data", {
  s <- study() %>%
    add_sim_data("dat", within = 2, between = 2,
                 n = 10, mu = 100, sd = 10, r = 0.5,
                 long = TRUE, seed = 8675309, plot = FALSE)

  dat <- s$data[[1]]$data

  expect_equal(names(dat), c("id", "B", "A", "y"))

  des <- s$data[[1]]$design
  cdes <- faux::check_design(2, 2, 10, 100, 10, 0.5, plot = FALSE)

  expect_equal(des, cdes)

  expect_equal(unlist(des$mu), c(B1.A1 = 100, B1.A2 = 100, B2.A1 = 100, B2.A2 = 100))
  expect_equal(unlist(des$sd) %>% sum(), 40)
  expect_equal(unlist(des$n), c(B1 = 10, B2 = 10))
  mat <- matrix(c(1.0, 0.5, 0.5, 1.0), 2, dimnames = list(c("A1", "A2"), c("A1", "A2")))
  expect_equal(des$r$B1, mat)
  expect_equal(des$r$B2, mat)
  expect_equal(unlist(des$within), c(A.A1 = "A1", A.A2 = "A2"))
  expect_equal(unlist(des$between), c(B.B1 = "B1", B.B2 = "B2"))
})
