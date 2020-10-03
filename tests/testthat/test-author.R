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
  expect_equal(r, "<contrib-group>\n<contrib>\n  <contrib-id authenticated=\"true\" contrib-id-type=\"orcid\">https://orcid.org/0000-0002-7523-5539</contrib-id>\n  <string-name>\n    <surname>DeBruine</surname>\n    <given-names>Lisa M.</given-names>\n  </string-name>\n  <role vocab=\"credit\"\n  vocab-identifier=\"http://credit.niso.org/\"\n  vocab-term=\"Conceptualization\"\n  vocab-term-identifier=\"http://credit.niso.org/contributor-roles/conceptualization/\">Conceptualization</role>\n  <role vocab=\"credit\"\n  vocab-identifier=\"http://credit.niso.org/\"\n  vocab-term=\"Data curation\"\n  vocab-term-identifier=\"http://credit.niso.org/contributor-roles/data-curation/\">Data curation</role>\n  <role vocab=\"credit\"\n  vocab-identifier=\"http://credit.niso.org/\"\n  vocab-term=\"Formal analysis\"\n  vocab-term-identifier=\"http://credit.niso.org/contributor-roles/formal-analysis/\">Formal analysis</role>\n  <role vocab=\"credit\"\n  vocab-identifier=\"http://credit.niso.org/\"\n  vocab-term=\"Funding acquisition\"\n  vocab-term-identifier=\"http://credit.niso.org/contributor-roles/funding-acquisition/\">Funding acquisition</role>\n  <role vocab=\"credit\"\n  vocab-identifier=\"http://credit.niso.org/\"\n  vocab-term=\"Investigation\"\n  vocab-term-identifier=\"http://credit.niso.org/contributor-roles/investigation/\">Investigation</role>\n  <role vocab=\"credit\"\n  vocab-identifier=\"http://credit.niso.org/\"\n  vocab-term=\"Methodology\"\n  vocab-term-identifier=\"http://credit.niso.org/contributor-roles/methodology/\">Methodology</role>\n  <role vocab=\"credit\"\n  vocab-identifier=\"http://credit.niso.org/\"\n  vocab-term=\"Project administration\"\n  vocab-term-identifier=\"http://credit.niso.org/contributor-roles/project-administration/\">Project administration</role>\n  <role vocab=\"credit\"\n  vocab-identifier=\"http://credit.niso.org/\"\n  vocab-term=\"Resources\"\n  vocab-term-identifier=\"http://credit.niso.org/contributor-roles/resources/\">Resources</role>\n  <role vocab=\"credit\"\n  vocab-identifier=\"http://credit.niso.org/\"\n  vocab-term=\"Software\"\n  vocab-term-identifier=\"http://credit.niso.org/contributor-roles/software/\">Software</role>\n  <role vocab=\"credit\"\n  vocab-identifier=\"http://credit.niso.org/\"\n  vocab-term=\"Supervision\"\n  vocab-term-identifier=\"http://credit.niso.org/contributor-roles/supervision/\">Supervision</role>\n  <role vocab=\"credit\"\n  vocab-identifier=\"http://credit.niso.org/\"\n  vocab-term=\"Validation\"\n  vocab-term-identifier=\"http://credit.niso.org/contributor-roles/validation/\">Validation</role>\n  <role vocab=\"credit\"\n  vocab-identifier=\"http://credit.niso.org/\"\n  vocab-term=\"Visualization\"\n  vocab-term-identifier=\"http://credit.niso.org/contributor-roles/visualization/\">Visualization</role>\n  <role vocab=\"credit\"\n  vocab-identifier=\"http://credit.niso.org/\"\n  vocab-term=\"Writing - original draft\"\n  vocab-term-identifier=\"http://credit.niso.org/contributor-roles/writing-original-draft/\">Writing - original draft</role>\n  <role vocab=\"credit\"\n  vocab-identifier=\"http://credit.niso.org/\"\n  vocab-term=\"Writing - review & editing\"\n  vocab-term-identifier=\"http://credit.niso.org/contributor-roles/writing-review-editing/\">Writing - review & editing</role>\n</contrib>\n</contrib-group>", fixed = TRUE)
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
  expect_equal(r, "<contrib-group>\n<contrib>\n  <contrib-id authenticated=\"true\" contrib-id-type=\"orcid\">https://orcid.org/0000-0002-7523-5539</contrib-id>\n  <string-name>\n    <surname>DeBruine</surname>\n    <given-names>Lisa M.</given-names>\n  </string-name>\n  <role vocab=\"credit\"\n  vocab-identifier=\"http://credit.niso.org/\"\n  vocab-term=\"Conceptualization\"\n  vocab-term-identifier=\"http://credit.niso.org/contributor-roles/conceptualization/\">Conceptualization</role>\n  <role vocab=\"credit\"\n  vocab-identifier=\"http://credit.niso.org/\"\n  vocab-term=\"Data curation\"\n  vocab-term-identifier=\"http://credit.niso.org/contributor-roles/data-curation/\">Data curation</role>\n</contrib>\n<contrib>\n  <contrib-id authenticated=\"true\" contrib-id-type=\"orcid\">https://orcid.org/0000-0002-0247-239X</contrib-id>\n  <string-name>\n    <surname>Lakens</surname>\n    <given-names>Daniel</given-names>\n  </string-name>\n  <role vocab=\"credit\"\n  vocab-identifier=\"http://credit.niso.org/\"\n  vocab-term=\"Formal analysis\"\n  vocab-term-identifier=\"http://credit.niso.org/contributor-roles/formal-analysis/\">Formal analysis</role>\n  <role vocab=\"credit\"\n  vocab-identifier=\"http://credit.niso.org/\"\n  vocab-term=\"Funding acquisition\"\n  vocab-term-identifier=\"http://credit.niso.org/contributor-roles/funding-acquisition/\">Funding acquisition</role>\n</contrib>\n</contrib-group>", fixed = TRUE)
})

test_that("check_orcid", {
  expect_equal(check_orcid("0000-0002-7523-5539"), "0000-0002-7523-5539")
  expect_equal(check_orcid("0000-0002-0247-239X"), "0000-0002-0247-239X")
  expect_equal(check_orcid("https://orcid.org/0000-0002-0247-239X"), "0000-0002-0247-239X")
  expect_warning(co <- check_orcid("0000-0002-0247-2394"), "The ORCiD 0000-0002-0247-2394 is not valid.", fixed = TRUE)
  expect_equal(co, FALSE)
})
