test_that("hypotheses", {
  s <- study() %>%
    add_hypothesis("H1") %>%
    add_analysis("A1", t.test(1:10, 2:11)) %>%
    add_criterion("C1", result = "p.value", operator = "<", comparator = 0.05)

  op <- utils::capture.output(output_hypotheses(s))

  expect_equal(op[[1]], "## Hypotheses")
  expect_equal(op[[3]], "### Hypothesis 1: H1")
  expect_equal(op[[7]], "#### Criteria" )
})

test_that("hypotheses header_lvl", {
  s <- study() %>%
    add_hypothesis("H1") %>%
    add_analysis("A1", t.test(1:10, 2:11)) %>%
    add_criterion("C1", result = "p.value", operator = "<", comparator = 0.05)

  op <- utils::capture.output(output_hypotheses(s, header_lvl = 3))

  expect_equal(op[[1]], "### Hypotheses")
  expect_equal(op[[3]], "#### Hypothesis 1: H1")
  expect_equal(op[[7]], "##### Criteria" )
})

test_that("multiple hypotheses", {
  s <- study() %>%
    add_hypothesis("H1") %>%
    add_analysis("A1", t.test(rnorm(100))) %>%
    add_criterion("C1", result = "p.value", operator = "<", comparator = 0.05,
                  hypothesis_id = "H1", analysis_id = "A1") %>%
    add_hypothesis("H2") %>%
    add_analysis("A2", t.test(rnorm(100))) %>%
    add_criterion("C2", result = "p.value", operator = "<", comparator = 0.05,
                  hypothesis_id = "H2", analysis_id = "A2") %>%
    add_criterion("C3", result = "estimate", operator = ">", comparator = 0,
                  hypothesis_id = "H2", analysis_id = "A2")

  op <- utils::capture.output(output_hypotheses(s))

  expect_equal(op[[1]], "## Hypotheses")
  expect_equal(op[[3]], "### Hypothesis 1: H1")
  expect_equal(op[[7]], "#### Criteria" )
  expect_equal(op[[32]], "### Hypothesis 2: H2")
})

test_that("analyses", {
  s <- study() %>%
    add_analysis("A1", t.test(1:10, 2:11))

  op <- utils::capture.output(output_analyses(s))

  expect_equal(op[[1]], "## Analyses")
  expect_equal(op[[3]], "### Analysis 1: A1 {#A1}")
  expect_equal(op[[5]], "<code><pre> analysis_A1_func <- function () ")
})

test_that("analyses header_lvl", {
  s <- study() %>%
    add_analysis("A1", t.test(1:10, 2:11))

  op <- utils::capture.output(output_analyses(s, header_lvl = 3))

  expect_equal(op[[1]], "### Analyses")
  expect_equal(op[[3]], "#### Analysis 1: A1 {#A1}")
  expect_equal(op[[5]], "<code><pre> analysis_A1_func <- function () ")
})

test_that("multiple analyses", {
  s <- study() %>%
    add_analysis("A1", t.test(1:10, 2:11)) %>%
    add_analysis("A2", cor(rnorm(10), rnorm(10)))

  op <- utils::capture.output(output_analyses(s, header_lvl = 3))

  expect_equal(op[[1]], "### Analyses")
  expect_equal(op[[3]], "#### Analysis 1: A1 {#A1}")
  expect_equal(op[[7]], "    t.test(1:10, 2:11)" )
  expect_equal(op[[10]], "#### Analysis 2: A2 {#A2}")
  expect_equal(op[[14]], "    cor(rnorm(10), rnorm(10))" )
})

test_that("results", {
  s <- study() %>%
    add_hypothesis("H1") %>%
    add_analysis("A1", t.test(1:10, 2:11)) %>%
    add_criterion("C1", result = "p.value", operator = "<", comparator = 0.05) %>%
    add_eval("c", "sig", "C1") %>%
    add_eval("f", "non-sig", "!C1") %>%
    study_analyse()

  op <- utils::capture.output(output_results(s))

  expect_equal(op[[1]], "## Results")
  expect_equal(op[[3]], "### Hypothesis 1: H1" )
  expect_equal(op[[8]], "The result was p.value = 0.47 (<span style=\"color:red\">FALSE</span>)  ")
})

test_that("results header_lvl", {
  s <- study() %>%
    add_hypothesis("H1") %>%
    add_analysis("A1", t.test(1:10, 2:11)) %>%
    add_criterion("C1", result = "p.value", operator = "<", comparator = 0.05) %>%
    add_eval("c", "sig", "C1") %>%
    add_eval("f", "non-sig", "!C1") %>%
    study_analyse()

  op <- utils::capture.output(output_results(s, header_lvl = 1))

  expect_equal(op[[1]], "# Results")
  expect_equal(op[[3]], "## Hypothesis 1: H1" )
})
