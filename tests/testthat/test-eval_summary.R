test_that("basic", {
  s <- study()
  expect_equal(eval_summary(s), "")
  sum <- capture.output(s)
  expect_equal(sum[1], "Demo Study")
  expect_equal(sum[2], "----------")
  expect_equal(sum[3], "")
  expect_equal(sum[4], "* Hypotheses: None")
  expect_equal(sum[5], "* Data: None")
  expect_equal(sum[6], "* Analyses: None")

  s <- study("Demo")
  expect_equal(eval_summary(s), "")
  sum <- capture.output(s)
  expect_equal(sum[1], "Demo")

  s <- add_hypothesis(s, "H1", "test")
  txt <- "Hypothesis H1: test\n\nConclusion: You may need to run `study_analyse()`\n* Corroborate (*no criteria*): \n* Falsify (*no criteria*):"
  expect_equal(eval_summary(s), txt)
  sum <- capture.output(s)
  expect_equal(sum[4], "* Hypotheses: H1")

  s <- add_data(s, "dat", iris)
  expect_equal(eval_summary(s), txt)
  sum <- capture.output(s)
  expect_equal(sum[5], "* Data: dat")

  s <- add_analysis(s, "A1", t.test(dat$Petal.Width, dat$Sepal.Width))
  expect_equal(eval_summary(s), txt)
  sum <- capture.output(s)
  expect_equal(sum[6], "* Analyses: A1")

  s <- study_analyse(s)
  expect_equal(eval_summary(s), txt)

  s <- add_criterion(s, "p", "p.value", "<", .05, "H1", "A1")
  txt <- "Hypothesis H1: test\n\nCriterion p:\n* p.value < 0.05 is \n* p.value = 0.000\n\nConclusion: You may need to run `study_analyse()`\n* Corroborate (*no criteria*): \n* Falsify (*no criteria*):"
  expect_equal(eval_summary(s), txt)

  s <- study_analyse(s)
  txt <- "Hypothesis H1: test\n\nCriterion p:\n* p.value < 0.05 is TRUE\n* p.value = 0.000\n\nConclusion: inconclusive\n* Corroborate (*no criteria*): FALSE\n* Falsify (*no criteria*): FALSE"
  expect_equal(eval_summary(s), txt)

  s$hypotheses[[1]]$criteria <- list()
  s$hypotheses[[1]]$conclusion <- NULL
  s <- study_analyse(s)
  txt <- "Hypothesis H1: test\n\nConclusion: You may need to run `study_analyse()`\n* Corroborate (*no criteria*): FALSE\n* Falsify (*no criteria*): FALSE"
  expect_equal(eval_summary(s), txt)
})
