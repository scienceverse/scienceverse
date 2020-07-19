test_that("numbers", {
  tol <- .001
  expect_equal(round_char(1.123456), 1, tolerance = tol)
  expect_equal(round_char(1.123456, 0), 1, tolerance = tol)
  expect_equal(round_char(1.123456, 1), 1.1, tolerance = tol)
  expect_equal(round_char(1.123456, 2), 1.12, tolerance = tol)
  expect_equal(round_char(1.123456, 3), 1.123, tolerance = tol)
  expect_equal(round_char(1.123456, 4), 1.1234, tolerance = tol)
  expect_equal(round_char(1.123456, 5), 1.12346, tolerance = tol)
  expect_equal(round_char(1.120456, 3), 1.12, tolerance = tol)
})

test_that("numbers as char", {
  expect_equal(round_char(1.123456, as_char = TRUE), "1")
  expect_equal(round_char(1.123456, 2, as_char = TRUE), "1.12")
  expect_equal(round_char(1.120456, 3, as_char = TRUE), "1.120")
})

test_that("non-numbers", {
  expect_equal(round_char("A"), "A")
  expect_equal(round_char(TRUE), TRUE)
  expect_equal(round_char(FALSE), FALSE)
})

test_that("lists", {
  cc <- c(1.0, 1.22, 1.25, 1.27)
  expect_equal(round_char(cc), c(1,1,1,1))
  expect_equal(round_char(cc, 1), c(1.0,1.2,1.2,1.3))
  expect_equal(round_char(cc, 1, TRUE), c("1.0","1.2","1.2","1.3"))
  expect_equal(round_char(LETTERS), LETTERS)

  expect_equal(round_char(list(1.34, 1.36), 1), list(1.3, 1.4))
  expect_equal(round_char(list("A", 1.36), 1), list("A", 1.4))
  expect_equal(round_char(list(a = "A", b = 1.36), 1), list(a = "A", b = 1.4))
  expect_equal(round_char(c(a = 1.34), 1), c(a = 1.3))

  l1 <- list(
    a = 1.37,
    b = "string",
    c = list(1.0, 1.11, 1.16),
    d = list(d1 = 1.0, d2 = 1.11, d3 = 1.16),
    e = c(1.0, 1.11, 1.16),
    f = c(f1 = 1.0, f2 = 1.11, f3 = 1.16),
    g = list(
      g1 = list(1.0, 1.11, 1.16),
      g2 = list(g3 = 1.0, g4 = 1.11, g5 = 1.16),
      g3 = LETTERS
    )
  )

  l2 <- list(
    a = 1.4,
    b = "string",
    c = list(1.0, 1.1, 1.2),
    d = list(d1 = 1.0, d2 = 1.1, d3 = 1.2),
    e = c(1.0, 1.1, 1.2),
    f = c(f1 = 1.0, f2 = 1.1, f3 = 1.2),
    g = list(
      g1 = list(1.0, 1.1, 1.2),
      g2 = list(g3 = 1.0, g4 = 1.1, g5 = 1.2),
      g3 = LETTERS
    )
  )

  l3 <- list(
    a = "1.4",
    b = "string",
    c = list("1.0", "1.1", "1.2"),
    d = list(d1 = "1.0", d2 = "1.1", d3 = "1.2"),
    e = c("1.0", "1.1", "1.2"),
    f = c(f1 = "1.0", f2 = "1.1", f3 = "1.2"),
    g = list(
      g1 = list("1.0", "1.1", "1.2"),
      g2 = list(g3 = "1.0", g4 = "1.1", g5 = "1.2"),
      g3 = LETTERS
    )
  )

  expect_equal(round_char(l1, 1), l2)
  expect_equal(round_char(l1, 1, TRUE), l3)
})


test_that("test", {
  x <- t.test(cars$speed, cars$dist)
  res <- list(statistic  = c(t = -7.41),
              parameter  = c(df = 53.12),
              p.value    = 0,
              conf.int   = c( -35.04, -20.12),
              estimate   = c("mean of x" = 15.40,  "mean of y" = 42.98),
              null.value = c("difference in means" = 0),
              stderr     = 3.72,
              alternative= "two.sided",
              method     = "Welch Two Sample t-test",
              data.name  = "cars$speed and cars$dist")
  expect_equal(round_char(x, 2), res)

})
