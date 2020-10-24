test_that("error", {
  err <- "Names and values must be the same length"
  expect_error(nlist(1:2, 1:3), err)
  expect_error(nlist(list(1:2, 3:4), 1:2), err)
})

test_that("simple", {
  x <- list(A = 1, B = 2)
  expect_equal(nlist(c("A", "B"), 1:2), x)

  expect_equal(nlist(list("A", "B"), 1:2), x)

  x <- list(`1` = 1, `2` = 2)
  expect_equal(nlist(1:2, 1:2), x)
})

test_that("nested list", {
  # will work if unlisted list lengths ==
  x <- as.list(1:8)
  names(x) <- LETTERS[1:8]

  n <- list(LETTERS[1:4], LETTERS[5:8])
  v <- list(1:4, 5:8)
  expect_equal(nlist(n, v), x)

  ## bit weird, but also works if names and values
  ## lists are grouped differently
  n <- list(LETTERS[1:3], LETTERS[4:8])
  v <- list(1:4, 5:8)
  expect_equal(nlist(n, v), x)
})
