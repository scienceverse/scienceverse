dat <- faux::sim_design(between = 2, n = c(10, 20), plot = FALSE)
dat$y[1] <- NA # remove one A1
dat$y[12] <- NA # remove 1 A2

A1 <- dat$y[which(dat$A=="A1")]
A2 <- dat$y[which(dat$A=="A2")]

test_that("2-sample", {
  x <- t_test(A1, A2)
  expect_equal(x$n, c(A1 = 9, A2 = 19))

  x <- t_test(x=A1, A2)
  expect_equal(x$n, c(A1 = 9, A2 = 19))

  x <- t_test(A1, y=A2)
  expect_equal(x$n, c(A1 = 9, A2 = 19))

  x <- t_test(x=A1, y=A2)
  expect_equal(x$n, c(A1 = 9, A2 = 19))

  x <- t_test(y=A2, x=A1)
  expect_equal(x$n, c(A1 = 9, A2 = 19))
})


test_that("1-sample", {
  x <- t_test(A1) # 9
  expect_equal(x$n, 9)

  x <- t_test(x=A2) # 19
  expect_equal(x$n, 19)
})

test_that("paired", {
  x <- t_test(A1, A2[1:10], paired = TRUE) #8
  expect_equal(x$n, 8)
})

test_that("formula", {
  x <- t_test(y ~ A, data = dat) #9, 19
  expect_equal(x$n, c(A1 = 9, A2 = 19))

  x <- t_test(y ~ A, dat) #9, 19
  expect_equal(x$n, c(A1 = 9, A2 = 19))

  x <- t_test(dat$y ~ dat$A) #9, 19
  expect_equal(x$n, c(A1 = 9, A2 = 19))

})
