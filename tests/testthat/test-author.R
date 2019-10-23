test_that("create author object", {
  demo <- list(name = list(surname = "DeBruine", given = "Lisa M."),
               roles = list("Conceptualization", "Methodology"))
  class(demo) <- c("scivrs_author", "list")
  a <- author(name = demo$name,
              roles = demo$roles)

  expect_equal(a, demo)
})

test_that("bad roles", {
  demo <- list(name = c(surname = "DeBruine", given = "Lisa M."),
               roles = c("Nope", "Methodology", "No"))
  expect_error(author(name = demo$name, roles = demo$roles),
               "These roles do not exist in the CRediT taxonomy: Nope, No\n  See http://credit.casrai.org/")
})

test_that("author_jats", {
  a <- author(name = c(surname = "DeBruine", given = "Lisa M."),
               roles = c("Conceptualization", "Methodology"))

  r <- author_jats(a)
  expect_equal(r, "<contrib contrib-type=\"author\">\n  <name>\n    <surname>DeBruine</surname>\n    <given-names>Lisa M.</given-names>\n  </name>\n  <role vocab=\"CRediT\" vocab-identifier=\" http://credit.casrai.org/\">Conceptualization</role>\n  <role vocab=\"CRediT\" vocab-identifier=\" http://credit.casrai.org/\">Methodology</role>\n</contrib>", fixed = TRUE)
})
