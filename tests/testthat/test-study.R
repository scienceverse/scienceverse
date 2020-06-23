test_that("default study", {
  s <- study()

  expect_equal(s$name, "Demo Study")
  expect_equal(s$authors, list())
  expect_equal(s$hypotheses, list())
  expect_equal(s$methods, list())
  expect_equal(s$data, list())
  expect_equal(s$analyses, list())
  expect_s3_class(s, "list")
  expect_s3_class(s, "scivrs_study")
})

test_that("study with name", {
  s <- study("Test Name")

  expect_equal(s$name, "Test Name")
})

test_that("study from json", {
  s <- study() %>%
    add_analysis(id = "mean_abs_diff", code = {
      (data1$Petal.Width - data1$Petal.Length) %>%
        abs() %>%
        mean() %>%
        magrittr::set_names("mean_abs_diff") %>%
        as.list()
    })

  # remove function to force load from code
  study_save(s, "test.json")
  s2 <- study("test.json")
  file.remove("test.json")

  expect_equal(s$analyses[[1]]$id, s2$analyses[[1]]$id)
  expect_equal(s$analyses[[1]]$code, s2$analyses[[1]]$code)
})

test_that("data from json", {
  s <- study() %>% add_data("iris", iris)
  if (file.exists("iris.json")) unlink("iris.json")
  study_save(s, "iris.json")

  s2 <- study("iris.json")
  d <- s2$data[[1]]$data

  expect_equal(iris, d)
})

test_that("replace info from json", {
  s <- study("input-data/postreg.json", abstract = "a")
  expect_equal(s$info$abstract, "a")
})

test_that("complex study from json", {
  s <- study("input-data/postreg.json")

  expect_equal(s$name, "Kinship and Prosocial Behaviour")
  expect_equal(s$info$abstract, "A reanalysis of data from DeBruine (2002) Facial Resemblance Enhances Trust, PRSLB.")
  expect_equal(s$authors[[1]]$orcid, "0000-0002-7523-5539")
  expect_equal(s$authors[[1]]$name, list("DeBruine", "Lisa M."))
  expect_equal(length(s$authors[[1]]$roles), 5)
  expect_equal(s$authors[[1]]$email, "lisa.debruine@glasgow.ac.uk")

  h <- s$hypotheses[[1]]
  expect_equal(h$id, "self_pref")
  expect_equal(h$description, "Cues of kinship will increase prosocial behaviour. Cues of kinship will be manipulated by morphed facial self-resemblance. Prosocial behaviour will be measured by responses in the trust game. The prediction is that the number of trusting AND/OR reciprocating moves will be greater to self morphs than to other morphs.")
  expect_equal(length(h$criteria), 4)
  expect_equal(h$criteria[[1]], list(
    id = "trust_lowbound",
    analysis_id = "trust",
    result = "conf.int[1]",
    operator = ">",
    comparator = 0,
    conclusion = TRUE
  ))
  expect_equal(h$corroboration$description, "The hypothesis is corroborated if the 97.5% CI lower bound is greater than 0 and the 97.5% CI upper bound is greater than 0.2 (the SESOI) for either the trust or reciprocation moves.")
  expect_equal(h$corroboration$evaluation, "(trust_lowbound & trust_highbound) | (recip_lowbound & recip_highbound)")
  expect_equal(h$corroboration$result, TRUE)

  expect_equal(s$data[[1]]$id, "kin")
  data <- s$data[[1]]$data
  expect_equal(nrow(data), 24)
  expect_equal(ncol(data), 4)

  vardesc <- list(
    description = list(
      trust_self  = "Number of trusting moves towards self-morphs",
      trust_other = "Number of trusting moves towards self-morphs",
      recip_self  = "Number of reciprocating moves towards other-morphs",
      recip_other = "Number of reciprocating moves towards other-morphs"
    ),
    type = rep("float", 4)
  )

  cb_json <- s$data[[1]]$codebook
  cb_data <- faux::codebook(data, "kin", vardesc = vardesc, as_json = FALSE)
  expect_equal(cb_json, cb_data)
})
