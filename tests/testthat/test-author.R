demo <- list(orcid = "0000-0002-7523-5539",
             name = c(surname = "DeBruine", given = "Lisa M."),
             roles = c("Conceptualization", "Methodology"))
class(demo) <- c("scivrs_author", "list")

test_that("create author object", {
  a <- author(orcid = demo$orcid,
              surname = "DeBruine", given = "Lisa M.",
              roles = demo$roles)

  expect_equal(a, demo)
})

test_that("create authors in study object", {
  a <- study() %>%
    add_author(orcid = demo$orcid,
               surname = "DeBruine", given = "Lisa M.",
               roles = demo$roles)

  expect_equal(a$authors[[1]], demo)
})

test_that("bad roles", {
  demo$roles <- c("Nope", "Methodology", "No")
  expect_error(author(orcid = demo$orcid,
                      surname = "DeBruine", given = "Lisa M.",
                      roles = demo$roles),
               "These roles do not exist in the CRediT taxonomy: Nope, No\n  See http://credit.casrai.org/")
})

test_that("author_jats", {
  a <- author(orcid = demo$orcid,
              surname = "DeBruine", given = "Lisa M.",
              roles = c("Conceptualization", "Methodology"))

  r <- author_jats(a)
  expect_equal(r, "<contrib contrib-type=\"author\">\n  <name>\n    <surname>DeBruine</surname>\n    <given-names>Lisa M.</given-names>\n  </name>\n  <role vocab=\"CRediT\" vocab-identifier=\" http://credit.casrai.org/\">Conceptualization</role>\n  <role vocab=\"CRediT\" vocab-identifier=\" http://credit.casrai.org/\">Methodology</role>\n</contrib>", fixed = TRUE)
})
