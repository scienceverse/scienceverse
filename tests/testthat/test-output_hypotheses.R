test_that("default", {
  s <- study() %>%
    add_hypothesis("H1") %>%
    add_analysis("A1", t.test(1:10, 2:11)) %>%
    add_criterion("C1", result = "p.value", operator = "<", comparator = 0.05)

  op <- utils::capture.output(output_hypotheses(s))

  expect_equal(op, op)
})

test_that("multiple hypotheses", {
  s <- study() %>%
    add_hypothesis("H1") %>%
    add_analysis("A1") %>%
    add_criterion("C1", result = "p.value", operator = "<", comparator = 0.05,
                  hypothesis_id = "H1", analysis_id = "A1") %>%
    add_hypothesis("H2") %>%
    add_analysis("A2") %>%
    add_criterion("C2", result = "p.value", operator = "<", comparator = 0.05,
                  hypothesis_id = "H2", analysis_id = "A2") %>%
    add_criterion("C3", result = "estimate", operator = ">", comparator = 0,
                  hypothesis_id = "H2", analysis_id = "A2")

  op <- utils::capture.output(output_hypotheses(s))
  m <- c()
  expect_equal(op, op)
})
