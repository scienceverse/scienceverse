context("test-study_analyse")

# messages ----
test_that("message", {
  expect_message(study() %>% study_analyse(),
               "No analyses have been specified")

  expect_message(study() %>% add_hypothesis() %>% add_analysis() %>% study_analyse(),
                  "Hypothesis 1 has no criteria")
})

# warnings ---
test_that("warnings", {
  expect_warning(
    study() %>% add_hypothesis() %>% add_analysis() %>%
      add_criterion("C1", "p.value", "<", 0.05) %>% study_analyse(),
    "Hypothesis 1 has no evaluation criteria for corroboration",
    fixed = TRUE, all = FALSE
  )

  expect_warning(
    study() %>% add_hypothesis() %>% add_analysis() %>%
      add_criterion("C1", "p.value", "<", 0.05) %>% study_analyse(),
    "Hypothesis 1 has no evaluation criteria for falsification",
    fixed = TRUE, all = FALSE
  )

  expect_warning(
    study() %>% add_hypothesis() %>% add_analysis() %>%
      add_criterion("C1", "p.value", "<", 0.05) %>%
      add_eval("corroboration", "", "(oops)") %>% study_analyse(),
    "Criteria oops have not been defined yet.",
    fixed = TRUE, all = FALSE
  )

  expect_warning(
    study() %>% add_hypothesis() %>% add_analysis() %>%
      add_criterion("C1", "p.value", "<", 0.05) %>%
      add_eval("corroboration", "", "(oops)") %>% study_analyse(),
    "Hypothesis 1 has an error in the evaluation criteria for corroboration: (oops)",
    fixed = TRUE, all = FALSE
  )
})



# simple function ----
test_that("simple function", {
  s <- study() %>%
    add_hypothesis() %>%
    add_analysis("A1", cor.test(dat$Petal.Width, dat$Petal.Length)) %>%
    add_criterion("sig", "p.value", "<", 0.05) %>%
    add_criterion("pos", "estimate", ">", 0) %>%
    add_eval("corroboration", "Petal width is significantly and positively correlated to length", "sig & pos") %>%
    add_eval("falsification", "Petal width is significantly and negatively correlated to length", "sig & !pos") %>%
    add_data("dat", iris)

  expect_message(s <- study_analyse(s), "Hypothesis 1, Criterion sig: p.value < 0.05 is TRUE (p.value = 0)", fixed = TRUE, all = FALSE)
  expect_message(s <- study_analyse(s), "Hypothesis 1, Criterion pos: estimate > 0 is TRUE (estimate = 0.96)", fixed = TRUE, all = FALSE)
  expect_message(s <- study_analyse(s), "Hypothesis 1:
    Corroborate: TRUE
    Falsify: FALSE
    Conclusion: corroborate", fixed = TRUE, all = FALSE)

  scienceverse_options(verbose = FALSE)

  expect_silent(study_analyse(s))

  scienceverse_options(verbose = TRUE)

  calc_res <- s$analyses[[1]]$results
  true_res <- cor.test(iris$Petal.Width, iris$Petal.Length)

  expect_named(calc_res, names(true_res), ignore.order = TRUE)

  # data.name is specified differently, so won't match
  names <- names(true_res)
  names <- names[names != "data.name"]

  for (name in names) {
    expect_equal(calc_res[[name]], true_res[[name]])
  }

  expect_equal(s$hypotheses[[1]]$corroboration$result, TRUE)
  expect_equal(s$hypotheses[[1]]$falsification$result, FALSE)
  expect_equal(s$hypotheses[[1]]$conclusion, "corroborate")
})


# criterion name overlap ----
test_that("criterion name overlap", {
  s <- study() %>%
    add_hypothesis() %>%
    add_analysis("A1", t.test(rnorm(1000))) %>%
    add_criterion("a", "p.value", ">", 0.001) %>%
    add_criterion("aa", "estimate", "<", 10) %>%
    add_criterion("aa2", "estimate", ">", -10) %>%
    add_eval("corroboration", "", "a | (aa | aa2)") %>%
    add_eval("falsification", "", "aa2 | (aa & a)") %>%
    study_analyse()

  expect_equal(s$hypotheses[[1]]$corroboration$result, TRUE)
  expect_equal(s$hypotheses[[1]]$falsification$result, TRUE)
  expect_equal(s$hypotheses[[1]]$conclusion, "inconclusive")
})

