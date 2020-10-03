context("test-output_custom_code")

test_that("null", {
  s <- study() %>%
    add_analysis("A1", {
      a <- rnorm(10)
      b<- rnorm(10)
      t.test(a, b)
    })

  op <- "{\n    a <- rnorm(10)\n    b <- rnorm(10)\n    t.test(a, b)\n}"
  expect_equal(op, output_custom_code(s))

  op <- output_custom_code(s, 1)
  expect_equal(op, output_custom_code(s))

  op <- output_custom_code(s, "A1")
  expect_equal(op, output_custom_code(s))

  op <- output_custom_code(s, prefix = ">")
  expect_equal(gsub(">", "", op),
               output_custom_code(s))
})
