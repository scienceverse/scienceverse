context("test-study")

test_that("default study", {
  s <- study()

  expect_equal(s$name, "Demo Study")
  expect_equal(s$hypotheses, list())
  expect_equal(s$methods, list())
  expect_equal(s$data, list())
  expect_equal(s$analyses, list())
  expect_s3_class(s, "list")
  expect_s3_class(s, "reg_study")
})

test_that("study with name", {
  s <- study("Test Name")

  expect_equal(s$name, "Test Name")
})
