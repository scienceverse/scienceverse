context("test-study_json")

test_that("defaults", {
  j <- study() %>% study_json()

  comp <- '{
  "name": "Demo Study", "hypotheses": [],
  "methods": [],
  "data": [],
  "analyses": []
}
'
  class(comp) <- "json"

  testthat::expect_equal(j, comp)
})

test_that("numeric arrays", {
  dat <- data.frame(x = 1:10)
  j <- study()
})
