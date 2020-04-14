context("test-add_data")

test_that("warnings", {
  expect_warning(
    study() %>% add_data("dat", "not_a_real_file.csv"),
    "The file not_a_real_file.csv does not exist."
  )

  filename <- system.file("rmarkdown/postreg.Rmd", package = "scienceverse")
  expect_warning(
    study() %>% add_data("dat", filename),
    "The Rmd format is not supported.\nPlease add data in one of the following formats: csv, xls, xlsx, txt, tsv, sav"
  )
})

test_that("defaults", {
  s <- study() %>% add_data("dat", iris)

  expect_equal(s$data[[1]]$id, "dat")

  cb <- s$data[[1]]$codebook

  expect_equal(cb$schemaVersion, "Psych-DS 0.1.0")
  expect_equal(cb[["@type"]], "Dataset")
  expect_equal(length(cb$variableMeasured), 5)
  expect_equal(s$data[[1]]$data, iris)
  for (i in 1:5) {
    vm <- cb$variableMeasured[[i]]
    expect_equal(vm$`@type`, "PropertyValue")
    expect_equal(vm$description, names(iris)[i])
    expect_equal(vm$name, names(iris)[i])
    if (is.numeric(iris[,i])) {
      expect_equal(vm$type, "float")
    } else if (is.factor(iris[,i])) {
      expect_equal(vm$type, "factor")
    }
  }
})

test_that("add data from file", {
  filename <- system.file("extdata/iris.csv", package = "scienceverse")
  s <- study() %>% add_data("myiris", filename)
  dat <- rio::import(filename)

  expect_equal(s$data[[1]]$codebook$schemaVersion, "Psych-DS 0.1.0")
  expect_equal(s$data[[1]]$codebook[["@type"]], "Dataset")
  expect_equal(length(s$data[[1]]$codebook$variableMeasured), 5)
  expect_equal(s$data[[1]]$data, dat)
  for (i in 1:5) {
    vm <- s$data[[1]]$codebook$variableMeasured[[i]]
    expect_equal(vm$`@type`, "PropertyValue")
    expect_equal(vm$description, names(dat)[i])
    expect_equal(vm$name, names(dat)[i])
  }
})

test_that("add data from codebook", {
  filename <- system.file("extdata/iat-codebook.json", package = "scienceverse")
  s <- study() %>% add_data("iat", filename)
  j <- c(list(id = "iat"), jsonlite::read_json(filename))
  class(j) <- c("reg_study_data", "list")

  expect_equal(s$data[[1]], j)
})
