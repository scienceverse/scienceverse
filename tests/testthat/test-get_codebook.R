test_that("errors", {
  s <- study()

  expect_error(get_codebook(s), "The study does not have dataset 1")
  expect_error(get_codebook(s, 2), "The study does not have dataset 2")
  expect_error(get_codebook(s, "missing"), "The study does not have dataset missing")

  s <- add_data(s, "dat", data.frame(x = 1:10))
  expect_error(get_codebook(s, 2), "The study does not have dataset 2")

  s$data[[1]]$codebook <- NULL
  s$data[[1]]$data <- NULL
  expect_error(get_codebook(s), "Dataset 1 has neither a codebook nor data to make one")
})

test_that("messages", {
  s <- study() %>% add_data("dat", data.frame(x = 1:10))
  s$data[[1]]$codebook <- NULL
  expect_message(get_codebook(s), "Dataset 1 does not have a codebook; one is being created")
  expect_message(get_codebook(s, "dat"), "Dataset dat does not have a codebook; one is being created")
})

test_that("default", {
  s <- study() %>% add_data("dat", data.frame(x = 1:10))
  cb <- get_codebook(s)
  expect_s3_class(cb, "psychds_codebook")
  expect_s3_class(cb, "list")
})
