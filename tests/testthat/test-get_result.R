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

  expect_warning(get_result(s, "p", "A1"),
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

test_that("results with indices", {
  s <- study() %>% add_analysis("A1", list(a = 1, b = "hi", c = 1:10,
                                  d = c(d1 = 1, d2 = 2),
                                  e = list(e1 = 1, e2 = "bye"))) %>%
    study_analyse()

  expect_equal(get_result(s, "a"), 1)
  expect_equal(get_result(s, "b"), "hi")
  expect_equal(get_result(s, "c"), 1:10)
  expect_equal(get_result(s, "c[2]"), 2)
  expect_equal(get_result(s, "d"), c(d1 = 1, d2 = 2))
  expect_equal(get_result(s, "d[1]"), 1)
  expect_equal(get_result(s, "d[[1]]"), 1)
  expect_equal(get_result(s, "d['d2']"), 2)
  expect_equal(get_result(s, 'd["d2"]'), 2)
  expect_equal(get_result(s, "d[['d2']]"), 2)
  expect_warning(get_result(s, "d[3]"), "d does not have a numeric index 3")

  expect_equal(get_result(s, "e"), list(e1 = 1, e2 = "bye"))
  expect_equal(get_result(s, "e[1]"), 1)
  expect_equal(get_result(s, "e[[1]]"), 1)
  expect_equal(get_result(s, "e['e2']"), "bye")
  expect_equal(get_result(s, "e[['e2']]"), "bye")
  expect_equal(get_result(s, "e$e2"), "bye")

  expect_warning(get_result(s, "misspell"), "The result misspell is not found. Possible results are: a, b, c, d, e")

  # technically not OK but work
  expect_equal(get_result(s, "d[d2]"), 2) # no quotes
  expect_equal(get_result(s, "d[[[[[d2]]]"), 2) # too many brackets
  expect_equal(get_result(s, "d$d2"), 2)  #d is a vector, not a list
})

test_that("rounding", {
 s <- study() %>%
   add_analysis("A1", t.test(cars$speed, cars$dist)) %>%
   study_analyse()

  check <- list(statistic  = c(t = -7.41),
              parameter  = c(df = 53.12),
              p.value    = 0,
              conf.int   = c( -35.04, -20.12),
              estimate   = c("mean of x" = 15.40,  "mean of y" = 42.98),
              null.value = c("difference in means" = 0),
              stderr     = 3.72,
              alternative= "two.sided",
              method     = "Welch Two Sample t-test",
              data.name  = "cars$speed and cars$dist")
  class(check) <- c("scivrs_results", "list")
  res <- get_result(s, digits = 2)

  expect_equal(check, res)

})

test_that("char", {
  s <- study() %>%
    add_analysis("A1", t.test(cars$speed, cars$dist)) %>%
    study_analyse()

  check <- list(statistic  = c(t = "-7.41"),
                parameter  = c(df = "53.12"),
                p.value    = "0.00",
                conf.int   = c( "-35.04", "-20.12"),
                estimate   = c("mean of x" = "15.40",  "mean of y" = "42.98"),
                null.value = c("difference in means" = "0.00"),
                stderr     = "3.72",
                alternative= "two.sided",
                method     = "Welch Two Sample t-test",
                data.name  = "cars$speed and cars$dist")
  class(check) <- c("scivrs_results", "list")
  res <- get_result(s, digits = 2, return = "char")

  expect_equal(check, res)

})

test_that("html", {
  s <- study() %>%
    add_analysis("A1", t.test(cars$speed, cars$dist)) %>%
    study_analyse()

  res <- get_result(s, "estimate[1]", digits = 2, return = "html")
  check <- "<a href='analysis.html#analysis_1' title='Analysis 1 Result estimate[1]'>15.40</a>"
  expect_equal(res, check)

  res <- get_result(s, "estimate[2]", digits = 2, return = "html")
  check <- "<a href='analysis.html#analysis_1' title='Analysis 1 Result estimate[2]'>42.98</a>"
  expect_equal(res, check)

  res <- get_result(s, "statistic", digits = 2, return = "html")
  check <- "<a href='analysis.html#analysis_1' title='Analysis 1 Result statistic'>-7.41</a>"
  expect_equal(res, check)
})
