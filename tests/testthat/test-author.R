demo <- list(orcid = "0000-0002-7523-5539",
             name = list(surname = "DeBruine", given = "Lisa M."),
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

test_that("bad orcid", {
  expect_warning(author(orcid = "00",
                      surname = "DeBruine", given = "Lisa M.",
                      roles = demo$roles),
               "The ORCiD 00 is not valid.")
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
              roles = credit_roles("names"))

  r <- author_jats(a)
  expect_equal(r, "<contrib-group>\n<contrib>\n  <contrib-id authenticated=\"true\" contrib-id-type=\"orcid\">https://orcid.org/0000-0002-7523-5539</contrib-id>\n  <string-name>\n    <surname>DeBruine</surname>\n    <given-names>Lisa M.</given-names>\n  </string-name>\n  <role content-type=\"https://dictionary.casrai.org/Contributor_Roles/Conceptualization\" >Conceptualization</role>\n  <role content-type=\"https://dictionary.casrai.org/Contributor_Roles/Data_curation\" >Data curation</role>\n  <role content-type=\"https://dictionary.casrai.org/Contributor_Roles/Formal_analysis\" >Formal analysis</role>\n  <role content-type=\"https://dictionary.casrai.org/Contributor_Roles/Funding_acquisition\" >Funding acquisition</role>\n  <role content-type=\"https://dictionary.casrai.org/Contributor_Roles/Investigation\" >Investigation</role>\n  <role content-type=\"https://dictionary.casrai.org/Contributor_Roles/Methodology\" >Methodology</role>\n  <role content-type=\"https://dictionary.casrai.org/Contributor_Roles/Project_administration\" >Project administration</role>\n  <role content-type=\"https://dictionary.casrai.org/Contributor_Roles/Resources\" >Resources</role>\n  <role content-type=\"https://dictionary.casrai.org/Contributor_Roles/Software\" >Software</role>\n  <role content-type=\"https://dictionary.casrai.org/Contributor_Roles/Supervision\" >Supervision</role>\n  <role content-type=\"https://dictionary.casrai.org/Contributor_Roles/Validation\" >Validation</role>\n  <role content-type=\"https://dictionary.casrai.org/Contributor_Roles/Visualization\" >Visualization</role>\n  <role content-type=\"https://dictionary.casrai.org/Contributor_Roles/Writing_original_draft\" >Writing - original draft</role>\n  <role content-type=\"https://dictionary.casrai.org/Contributor_Roles/Writing_review_editing\" >Writing - review & editing</role>\n</contrib>\n</contrib-group>", fixed = TRUE)
})

test_that("multi author_jats", {
  s <- study() %>%
    add_author(orcid = demo$orcid,
              surname = "DeBruine", given = "Lisa M.",
              roles = credit_roles("names")[1:2]) %>%
    add_author(orcid = "0000-0002-0247-239X",
               surname = "Lakens", given = "Daniel",
               roles = credit_roles("names")[3:4])

  r <- author_jats(s)
  expect_equal(r, "<contrib-group>\n<contrib>\n  <contrib-id authenticated=\"true\" contrib-id-type=\"orcid\">https://orcid.org/0000-0002-7523-5539</contrib-id>\n  <string-name>\n    <surname>DeBruine</surname>\n    <given-names>Lisa M.</given-names>\n  </string-name>\n  <role content-type=\"https://dictionary.casrai.org/Contributor_Roles/Conceptualization\" >Conceptualization</role>\n  <role content-type=\"https://dictionary.casrai.org/Contributor_Roles/Data_curation\" >Data curation</role>\n</contrib>\n<contrib>\n  <contrib-id authenticated=\"true\" contrib-id-type=\"orcid\">https://orcid.org/0000-0002-0247-239X</contrib-id>\n  <string-name>\n    <surname>Lakens</surname>\n    <given-names>Daniel</given-names>\n  </string-name>\n  <role content-type=\"https://dictionary.casrai.org/Contributor_Roles/Formal_analysis\" >Formal analysis</role>\n  <role content-type=\"https://dictionary.casrai.org/Contributor_Roles/Funding_acquisition\" >Funding acquisition</role>\n</contrib>\n</contrib-group>", fixed = TRUE)
})

test_that("check_orcid", {
  expect_equal(check_orcid("0000-0002-7523-5539"), "0000-0002-7523-5539")
  expect_equal(check_orcid("0000-0002-0247-239X"), "0000-0002-0247-239X")
  expect_equal(check_orcid("https://orcid.org/0000-0002-0247-239X"), "0000-0002-0247-239X")
  expect_warning(co <- check_orcid("0000-0002-0247-2394"), "The ORCiD 0000-0002-0247-2394 is not valid.", fixed = TRUE)
  expect_equal(co, FALSE)
})
