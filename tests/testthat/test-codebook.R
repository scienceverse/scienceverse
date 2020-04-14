test_that("default", {
  cb <- codebook(iris, as_json = FALSE)

  expect_equal(cb$`@context`, "https://schema.org/")
  expect_equal(cb$`@type`, "Dataset")
  expect_equal(cb$name, "iris")
  expect_null(cb$description)
  expect_equal(cb$schemaVersion, "Psych-DS 0.1.0")
  for (vm in cb$variableMeasured) {
    expect_equal(vm$name, vm$description)
  }
})


test_that("example", {
  vardesc = list(
    description = c("Length of the sepal",
                    "Width of the sepal",
                    "Length of the petal",
                    "Width of the petal",
                    "The flower species"),
    type = c("float", "float", "float", "float", "string")
  )
  cb <- codebook(iris, vardesc = vardesc, as_json = FALSE)

  get_desc <- sapply(cb$variableMeasured, `[[`, "description")
  expect_equal(get_desc, vardesc$description)

  get_desc <- sapply(cb$variableMeasured, `[[`, "type")
  expect_equal(get_desc, vardesc$type)

  expect_equivalent(cb$variableMeasured[[5]]$levels,
                    levels(iris$Species))
})

# non-standard variable properties----
test_that("non-standard", {
  vardesc = list(
    goodness = c("good", "bad", "ok", "ok", "perfect"),
    type = c("float", "float", "float", "float", "string"),
    typo = list(Species = "yup")
  )

  expect_warning(cb <- codebook(iris, vardesc = vardesc),
                 "The following variable properties are not standard: goodness, typo",
                 fixed = TRUE)

  expect_warning(cb <- codebook(iris, a = "no", b = "nope"),
                 "The following dataset properties are not standard: a, b",
                 fixed = TRUE)
})

# doi----
test_that("doi", {
  cb <- codebook(iris, doi = "doi: 10.100/23hh", as_json = FALSE)
  expect_null(cb$doi)
  expect_equal(cb$sameAs, "https://doi.org/10.100/23hh")

  cb <- codebook(iris, doi = "https://doi.org/10.100/4444", as_json = FALSE)
  expect_equal(cb$sameAs, "https://doi.org/10.100/4444")

  cb <- codebook(iris, doi = "   https://doi.org/10.100/ff   ", as_json = FALSE)
  expect_equal(cb$sameAs, "https://doi.org/10.100/ff")

  cb <- codebook(iris, doi = "10.100/just/doi", as_json = FALSE)
  expect_equal(cb$sameAs, "https://doi.org/10.100/just/doi")
})
