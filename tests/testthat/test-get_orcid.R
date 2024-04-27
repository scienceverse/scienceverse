test_that("error", {
  expect_error(get_orcid(" "), "You must include a family name")
})

test_that("get_orcid", {
  skip_if_offline()

  expect_message(get_orcid("DeBruinez", "Z"), "No ORCID found for Z DeBruinez", fixed = TRUE)

  oid <- c("0000-0002-7523-5539", "0000-0003-2234-4827")
  expect_message(db <- get_orcid("DeBruine"), "Multiple (2) ORCIDs found for * DeBruine", fixed = TRUE)
  expect_identical(oid, sort(db))

  skip("Takes too long")
  expect_identical(oid[1], get_orcid("DeBruine", "L"))
  expect_identical(oid[1], get_orcid("DeBruine", "Lisa"))
  expect_identical(oid[1], get_orcid("DeBruine", "L M"))
  expect_identical(oid[1], get_orcid("DeBruine", "Lisa M"))

  # wildcards
  oid <- "0000-0002-0247-239X"
  expect_identical(oid, get_orcid("Laken*", "Daniel"))

  ## special characters
  oid <- "0000-0002-0459-880X"
  expect_identical(oid, get_orcid("Van Ertvelde", "Anaïs"))
})
