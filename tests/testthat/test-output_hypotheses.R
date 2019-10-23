test_that("default", {
  s <- study() %>%
    add_hypothesis() %>%
    add_analysis(func = "t.test", params = "x") %>%
    add_criterion(result = "p.value", operator = "<", comparator = 0.05)

  op <- capture.output(output_hypotheses(s))
  m <- c("## Hypotheses",
         "",
         "### Hypothesis 1: 1",
         "",
         "Describe your hypothesis",
         "",
         "* Criterion 1 is confirmed if analysis 1 yields p.value < 0.05   ",
         "",
         "If all criteria are met, this hypothesis is supported.",
         "",
         "")

  expect_equal(op, m)
})

test_that("multiple hypotheses", {
  s <- study() %>%
    add_hypothesis("First H", "&", "H1") %>%
    add_analysis(func = "t.test", params = "x", id = "A1") %>%
    add_criterion(result = "p.value", operator = "<", comparator = 0.05,
                  hypothesis_id = "H1", analysis_id = "A1") %>%
    add_hypothesis("Second H", "or", "H2") %>%
    add_analysis(func = "cor", params = c("x", "y"), id = "A2") %>%
    add_criterion(result = "p.value", operator = "<", comparator = 0.05,
                  hypothesis_id = "H2", analysis_id = "A2") %>%
    add_criterion(result = "estimate", operator = ">", comparator = 0,
                  hypothesis_id = "H2", analysis_id = "A2")

  op <- capture.output(output_hypotheses(s))
  m <- c( "## Hypotheses",
          "",
          "### Hypothesis 1: H1",
          "",
          "First H",
          "",
          "* Criterion 1 is confirmed if analysis A1 yields p.value < 0.05   ",
          "",
          "If all criteria are met, this hypothesis is supported.",
          "",
          "",
          "### Hypothesis 2: H2",
          "",
          "Second H",
          "",
          "* Criterion 1 is confirmed if analysis A2 yields p.value < 0.05   ",
          "* Criterion 2 is confirmed if analysis A2 yields estimate > 0   ",
          "",
          "If any criteria are met, this hypothesis is supported.",
          "",
          "")
  expect_equal(op, m)
})
