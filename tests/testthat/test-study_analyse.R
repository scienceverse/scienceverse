context("test-study_analyse")

# messages ----
test_that("message", {
  expect_message(study() %>% study_analyse(),
               "No analyses have been specified")

  expect_message(study() %>% add_hypothesis() %>%
                   add_analysis(NULL, t.test(rnorm(100))) %>%
                   study_analyse(),
                  "Hypothesis 1 has no criteria")
})

# warnings ----
test_that("warnings", {
  s <- study() %>% add_hypothesis() %>%
    add_analysis(NULL, t.test(rnorm(100))) %>%
    add_criterion("C1", "p.value", "<", 0.05)

  expect_message(
     study_analyse(s),
    "Hypothesis 1 has no evaluation criteria for corroboration",
    fixed = TRUE, all = FALSE
  )
  expect_message(
    study_analyse(s),
    "Hypothesis 1 has no evaluation criteria for falsification",
    fixed = TRUE, all = FALSE
  )

  expect_warning(
    s <- add_eval(s, "corroboration", "oops"),
    "Criteria `oops` have not been defined yet.",
    fixed = TRUE, all = FALSE
  )

  expect_warning(
    study_analyse(s),
    "Hypothesis 1 has an error in the evaluation criteria for corroboration: oops",
    fixed = TRUE, all = FALSE
  )
})



# simple function ----
test_that("simple function", {
  s <- study() %>%
    add_hypothesis("H1") %>%
    add_analysis("A1", cor.test(dat$Petal.Width, dat$Petal.Length)) %>%
    add_criterion("sig", "p.value", "<", 0.05) %>%
    add_criterion("pos", "estimate", ">", 0) %>%
    add_eval("corroboration", "sig & pos", "Petal width is significantly and positively correlated to length") %>%
    add_eval("falsification", "sig & !pos", "Petal width is significantly and negatively correlated to length") %>%
    add_data("dat", iris)

  evsum <- study_analyse(s) %>% eval_summary()
  evsum_check <- "Hypothesis H1: Describe your hypothesis\n\nCriterion sig:\n* p.value < 0.05 is TRUE\n* p.value = 0.000\n\nCriterion pos:\n* estimate > 0 is TRUE\n* estimate = 0.963\n\nConclusion: corroborate\n* Corroborate (sig & pos): TRUE\n* Falsify (sig & !pos): FALSE"
  expect_equal(evsum, evsum_check)

  expect_message(s <- study_analyse(s), evsum, fixed = TRUE, all = TRUE)

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
    add_eval("corroboration", "a | (aa | aa2)") %>%
    add_eval("falsification", "aa2 | (aa & a)") %>%
    study_analyse()

  expect_equal(s$hypotheses[[1]]$corroboration$result, TRUE)
  expect_equal(s$hypotheses[[1]]$falsification$result, TRUE)
  expect_equal(s$hypotheses[[1]]$conclusion, "inconclusive")
})

# multiple hypotheses ----
test_that("multiple hypotheses", {
  s <- study() %>%
    add_sim_data("dat", within = 2, between = 2, r = 0.5) %>%
    add_hypothesis("H1", "W1a and W1b will be correlated") %>%
    add_analysis("A1", cor.test(dat$W1a, dat$W1b)) %>%
    add_criterion("sig", "p.value", "<", 0.05) %>%
    add_criterion("pos", "estimate", ">", 0) %>%
    add_eval("c", "sig & pos") %>%
    add_eval("f", "sig & !pos") %>%

    add_hypothesis("H2", "B1a will have a bigger DV than B1b") %>%
    add_analysis("A2", t.test((W1a+W1b)~B1, dat)) %>%
    add_criterion("sig", "p.value", "<", 0.05) %>%
    add_criterion("pos", "estimate[1]", ">", "estimate[2]") %>%
    add_eval("c", "sig & pos") %>%
    add_eval("f", "sig & !pos")

  s2 <- study_analyse(s)
  es <- eval_summary(s2)

  expect_equal(grep("Hypothesis H1: W1a and W1b will be correlated", es), 1)
  expect_equal(grep("Hypothesis H2: B1a will have a bigger DV than B1b", es), 1)
})


# result in comparator ----
test_that("result in comparator", {
  s <- study() %>%
    add_hypothesis("H1") %>%
    add_analysis("A1", t.test(dat$Petal.Width, dat$Petal.Length)) %>%
    add_criterion("pos", "estimate[1]", "<", "estimate[2]") %>%
    add_eval("corroboration", "pos", "Petal width is longer than length") %>%
    add_eval("falsification", "!pos", "Petal width is shorter than length") %>%
    add_data("dat", iris)

  ev_sum <- "Hypothesis H1: Describe your hypothesis\n\nCriterion pos:\n* estimate[1] < estimate[2] is TRUE\n* estimate[1] = 1.199\n* estimate[2] = 3.758\n\nConclusion: corroborate\n* Corroborate (pos): TRUE\n* Falsify (!pos): FALSE"

  expect_message(s <- study_analyse(s), ev_sum, fixed = TRUE)

  expect_equal(eval_summary(s), ev_sum)
})

# app ----
# test_that("app", {
#   set.seed(8675309)
#   s <- study() %>%
#     add_hypothesis("H1") %>%
#     add_analysis("A1", cor.test(rnorm(20), rnorm(20))) %>%
#     add_criterion("p", "p.value", "<", 0.5) %>%
#     add_criterion("r", "estimate", ">", 0.2) %>%
#     add_eval("corroboration", "p & r") %>%
#     add_eval("falsification", "p & !r")
#
#
#   s <- study_analyse(s)
#   p <- get_result(s, "p.value")
# })

test_that("check logic", {
  s <- study() %>%
    add_hypothesis("H1") %>%
    add_analysis("A1", t.test(rnorm(10, 10))) %>%
    add_criterion("p", "p.value", "<", .05) %>%
    add_criterion("dir", "estimate", ">", 0)

  s2 <- add_eval(s, "c", "p") %>% study_analyse()
  expect_equal(s2$hypotheses[[1]]$corroboration$result, TRUE)

  s2 <- add_eval(s, "c", "dir") %>% study_analyse()
  expect_equal(s2$hypotheses[[1]]$corroboration$result, TRUE)

  s2 <- add_eval(s, "c", "p & dir") %>% study_analyse()
  expect_equal(s2$hypotheses[[1]]$corroboration$result, TRUE)

  s2 <- add_eval(s, "c", "(p & dir)") %>% study_analyse()
  expect_equal(s2$hypotheses[[1]]$corroboration$result, TRUE)

  s2 <- add_eval(s, "c", "p | dir") %>% study_analyse()
  expect_equal(s2$hypotheses[[1]]$corroboration$result, TRUE)

  s2 <- add_eval(s, "c", "!p | dir") %>% study_analyse()
  expect_equal(s2$hypotheses[[1]]$corroboration$result, TRUE)

  s2 <- add_eval(s, "c", "p | !dir") %>% study_analyse()
  expect_equal(s2$hypotheses[[1]]$corroboration$result, TRUE)

  s2 <- add_eval(s, "c", "!p") %>% study_analyse()
  expect_equal(s2$hypotheses[[1]]$corroboration$result, FALSE)

  s2 <- add_eval(s, "c", "!dir") %>% study_analyse()
  expect_equal(s2$hypotheses[[1]]$corroboration$result, FALSE)

  s2 <- add_eval(s, "c", "!p & !dir") %>% study_analyse()
  expect_equal(s2$hypotheses[[1]]$corroboration$result, FALSE)

  s2 <- add_eval(s, "c", "p & !dir") %>% study_analyse()
  expect_equal(s2$hypotheses[[1]]$corroboration$result, FALSE)

  s2 <- add_eval(s, "c", "!p & dir") %>% study_analyse()
  expect_equal(s2$hypotheses[[1]]$corroboration$result, FALSE)

  s2 <- add_eval(s, "c", "!p | !dir") %>% study_analyse()
  expect_equal(s2$hypotheses[[1]]$corroboration$result, FALSE)
})

