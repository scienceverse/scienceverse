path <- file.path(tempdir(), "scivrs_test")
unlink(path, recursive = TRUE) # clean up
dir.create(path)
setwd(path)

test_that("errors", {
  expect_true(is.function(study_save))

  expect_error(study_save(1),
               "The study argument needs to be a scivrs_study object or a list of them.",
               fixed = TRUE)
})

test_that("defaults", {
  s <- study()

  # make sure file doesn't already exist
  if (file.exists("demo_study.json")) unlink("demo_study.json")
  study_save(s)

  expect_true(file.exists("demo_study.json"))

  # check file contexts are as expected
  filecontents <- readLines("demo_study.json") %>% paste(collapse = "\n")
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
})

test_that("file name json suffix", {
  s <- study()

  fname <- "custom_file_name.json"
  if (file.exists(fname)) unlink(fname) # make sure doesn't already exist
  study_save(s, fname)

  expect_true(file.exists(fname))
})

test_that("file name no suffix", {
  s <- study()

  nname <- "custom_nosuffix_name"
  fname <- paste0(nname, ".json") # make sure doesn't already exist
  if (file.exists(fname)) unlink(fname)
  study_save(s, nname)

  expect_true(file.exists(fname))
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

test_that("verbose", {
  scienceverse_options(verbose = FALSE)
  expect_silent(study() %>% study_save())
  scienceverse_options(verbose = TRUE)
})

test_that("iteration", {
  studies <- list(
    study("A"),
    study("B")
  )

  if (file.exists("a.json")) unlink("a.json")
  if (file.exists("b.json")) unlink("b.json")
  study_save(studies)
  expect_true(file.exists("a.json"))
  expect_true(file.exists("b.json"))
})

# list.files(path)
unlink(path, recursive = TRUE) # clean up
