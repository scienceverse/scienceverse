test_that("default study", {
  s <- study()

  expect_equal(s$name, "Demo Study")
  expect_equal(s$authors, list())
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

test_that("study from json", {
  s <- study() %>%
    add_analysis(id = "mean_abs_diff", code = {
      (data1$Petal.Width - data1$Petal.Length) %>%
        abs() %>%
        mean() %>%
        magrittr::set_names("mean_abs_diff") %>%
        as.list()
    })

  # remove function to force load from code
  study_save(s, "test.json")
  s2 <- study("test.json")
  file.remove("test.json")

  expect_equal(s$analyses[[1]]$id, s2$analyses[[1]]$id)
  expect_equal(s$analyses[[1]]$func, s2$analyses[[1]]$func)

  s2code <- s2$analyses[[1]]$code %>% unlist() %>% trimws() %>%paste(collapse = " ")
  expect_equal(s2code, "function () { { (data1$Petal.Width - data1$Petal.Length) %>% abs() %>% mean() %>% magrittr::set_names(\"mean_abs_diff\") %>% as.list() } }")
})
