test_that("defaults", {
  s <- study()

  # make sure file doesn't already exist
  if (file.exists("study.json")) unlink("study.json")
  study_save(s)

  expect_true(file.exists("study.json"))

  # check file contexts are as expected
  filecontents <- readLines("study.json") %>% paste(collapse = "\n")
  expected <- '{
    "name": "Demo Study",
    "info": [],
    "authors": [],
    "hypotheses": [],
    "methods": [],
    "data": [],
    "analyses": []
}
'
  expect_equal(filecontents, expected)

  if (file.exists("study.json")) unlink("study.json") # clean up
})

test_that("file name json suffix", {
  s <- study()

  fname <- "custom_file_name.json"
  if (file.exists(fname)) unlink(fname) # make sure doesn't already exist
  study_save(s, fname)

  expect_true(file.exists(fname))
  if (file.exists(fname)) unlink(fname) # clean up
})

test_that("file name no suffix", {
  s <- study()

  nname <- "custom_nosuffix_name"
  fname <- paste0(nname, ".json") # make sure doesn't already exist
  if (file.exists(fname)) unlink(fname)
  study_save(s, nname)

  expect_true(file.exists(fname))
  if (file.exists(fname)) unlink(fname) # clean up
})

test_that("data", {
  s <- study() %>% add_data("iris", iris)
  fname <- "iris.json"
  if (file.exists(fname)) unlink(fname)
  study_save(s, fname)

  fcontents <- jsonlite::read_json(fname)

  d <- fcontents$data[[1]]$data

  expect_equal(names(d), names(iris))
  expect_equal(unlist(d$Sepal.Length), iris$Sepal.Length)
})


