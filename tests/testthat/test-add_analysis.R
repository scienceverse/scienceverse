test_that("defaults", {
  s <- study() %>%
    add_analysis(NULL, t.test(rnorm(100)))
  expect_equal(length(s$analyses), 1)
  expect_equal(s$analyses[[1]]$id, 1)
  expect_equal(s$analyses[[1]]$func, "analysis_1_func")
  expect_equal(s$analyses[[1]]$code, function() {
    t.test(rnorm(100))
  })
})

# custom function ----
test_that("custom function", {
  myfunc <- function() {
    {
      a <- 1
      b <- 2
    }

    list(
      "a" = a,
      "b" = b
    )
  }

  s2 <- study() %>%
    add_analysis(NULL, t.test(rnorm(100))) %>%
    add_analysis("a2", {
    a <- 1
    b <- 2
  }, c("a", "b"))
  expect_equal(length(s2$analyses), 2)
  expect_equal(s2$analyses[[2]]$id, "a2")
  expect_equal(s2$analyses[[2]]$func, "analysis_a2_func")
  expect_equal(s2$analyses[[2]]$code, myfunc)

  expect_message(s3 <- add_analysis(s2, "a3 with spaces", a <- 1), "id \"a3 with spaces\" changed to \"a3_with_spaces\"")
  expect_equal(s3$analyses[[3]]$id, "a3_with_spaces")
})

# function with undefined data ----
test_that("undefined data", {
  s <- study() %>%
    add_analysis("A1", {
      t.test(dat$a, dat$b)
    })

  func <- function() {{ t.test(dat$a, dat$b) }}
  expect_equal(s$analyses[[1]]$code, func)
})

# add from file ----
test_that("add from file", {
  testthat::skip("only works in testthat")

  s <- study() %>%
    add_analysis(NULL, "custom_code_test.R")

  s$analyses[[1]]$code
})
