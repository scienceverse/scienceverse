test_that("errors", {
  s <- study() %>%
    add_hypothesis("H1", "Yup")

  expect_error(get_result(s, "p.value"),
               "Analysis 1 does not exist.")

  s <- add_analysis(s, "A1", t.test(-10:15)) %>%
    add_criterion("p", "p.value", "<", .05) %>%
    add_criterion("d", "estimate", ">", 0)

  expect_error(get_result(s, "p.value"),
               "The analysis does not have results yet. Try running study_analyse first.")

  s <- s %>%
    add_eval("corroboration", "", "p&d") %>%
    add_eval("falsification", "", "p & !d") %>% # yes I know this is bad
    study_analyse()

  expect_error(get_result(s, "p.value", "A2"),
               "Analysis A2 does not exist.")

  expect_error(get_result(s, "p.value", 2),
               "Analysis 2 does not exist.")

  expect_error(get_result(s, "p", "A1"),
               "The result p is not found. Possible results are: statistic, parameter, p.value, conf.int, estimate, null.value, stderr, alternative, method, data.name", fixed = TRUE)
})

test_that("all results", {
  s <- study() %>%
    add_hypothesis("H1", "Yup") %>%
    add_analysis("A1", t.test(-10:15)) %>%
    add_criterion("p", "p.value", "<", .05) %>%
    add_criterion("d", "estimate", ">", 0) %>%
    add_eval("corroboration", "", "p&d") %>%
    add_eval("falsification", "", "p & !d") %>%
    study_analyse()

  res <- get_result(s)

  expect_equal(length(res), 10)
  expect_equal(res$stderr, 1.5)
})
