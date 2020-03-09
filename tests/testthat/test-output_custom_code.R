context("test-output_custom_code")

test_that("null", {
  s <- study() %>%
    add_analysis("id", t.test(x = 1:10, y = 2:11))

  op <- output_custom_code(s)

  expect_equal(op, "analysis_id_func <- function () \n{\n    t.test(x = 1:10, y = 2:11)\n}")
})
