context("test-output_custom_code")

test_that("null", {
  s <- study() %>%
    add_analysis("t.test", list(x = 1:10, y = 2:11))

  op <- output_custom_code(s)

  expect_equal(op, "t.test")
})

test_that("custom from code", {
  ccode <<- function() {
    "it worked"
  }

  s <- study() %>% add_analysis("ccode")

  rm(ccode, envir = .GlobalEnv)

  op <- output_custom_code(s)

  expect_equal(op, "ccode <- function () \n{\n    \"it worked\"\n}")
})
