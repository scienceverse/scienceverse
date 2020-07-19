test_that("default", {
  study <- study() %>%
    add_data("D1", mtcars) %>%
    add_data("D2", cars)

  dir <- "data"
  if (dir.exists(dir)) unlink(dir, recursive = TRUE)

  make_data(study)
  expect_true(dir.exists(dir))
  expect_true(file.exists(sprintf("%s/D1_data.tsv", dir)))
  expect_true(file.exists(sprintf("%s/D2_data.tsv", dir)))
  expect_true(file.exists(sprintf("%s/D1_data.json", dir)))
  expect_true(file.exists(sprintf("%s/D2_data.json", dir)))

  d1 <- read.csv(sprintf("%s/D1_data.tsv", dir), sep = "\t")
  expect_equivalent(d1, study$data[[1]]$data)

  d2 <- read.csv(sprintf("%s/D2_data.tsv", dir), sep = "\t")
  expect_equivalent(d2, study$data[[2]]$data)

  if (dir.exists(dir)) unlink(dir, recursive = TRUE)
})

test_that("custom directory and csv", {
  study <- study() %>%
    add_data("D1", mtcars) %>%
    add_data("D2", cars)

  dir <- "test/the_data"
  if (dir.exists(dir)) unlink(dir, recursive = TRUE)

  make_data(study, dir = dir, format = "csv")
  expect_true(dir.exists(dir))
  expect_true(file.exists(sprintf("%s/D1_data.csv", dir)))
  expect_true(file.exists(sprintf("%s/D2_data.csv", dir)))
  expect_true(file.exists(sprintf("%s/D1_data.json", dir)))
  expect_true(file.exists(sprintf("%s/D2_data.json", dir)))

  d1 <- read.csv(sprintf("%s/D1_data.csv", dir))
  expect_equivalent(d1, study$data[[1]]$data)

  d2 <- read.csv(sprintf("%s/D2_data.csv", dir))
  expect_equivalent(d2, study$data[[2]]$data)

  if (dir.exists(dir)) unlink(dir, recursive = TRUE)
})

test_that("extra argument to write.table", {
  study <- study() %>% add_data("D1", mtcars)

  dir <- "data"
  if (dir.exists(dir)) unlink(dir, recursive = TRUE)

  newnames <- paste0("VV",1:11)
  make_data(study, col.names = newnames)
  expect_true(file.exists(sprintf("%s/D1_data.tsv", dir)))

  d1 <- read.csv(sprintf("%s/D1_data.tsv", dir), sep = "\t")
  expect_equal(newnames, names(d1))

  if (dir.exists(dir)) unlink(dir, recursive = TRUE)
})
