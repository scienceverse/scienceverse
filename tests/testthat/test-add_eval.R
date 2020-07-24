s <- study() %>%
  add_hypothesis("H1") %>%
  add_analysis("A1", t.test(rnorm(10, 10))) %>%
  add_criterion("p", "p.value", "<", .05) %>%
  add_criterion("dir", "estimate", ">", 0)

test_that("errors", {
  err <- "argument \"evaluation\" is missing, with no default"
  expect_error(add_eval(s, "c"), err, fixed = TRUE)

  err <- "argument \"type\" is missing, with no default"
  expect_error(add_eval(s), err, fixed = TRUE)
  expect_error(add_eval(s, evaluation = "p"), err, fixed = TRUE)

  type_err <- "The type must be one of 'corroboration' or 'falsification' (or 'c'/'f')"
  expect_error(add_eval(s, "e", "p"), type_err, fixed = TRUE)
  expect_error(add_eval(s, "", "p"), type_err, fixed = TRUE)

  # all types starting with c or f work
  expect_silent(add_eval(s, "c", "p"))
  expect_silent(add_eval(s, "corrob", "p"))
  expect_silent(add_eval(s, "C", "p"))
  expect_silent(add_eval(s, "cancel", "p"))
  expect_silent(add_eval(s, "f", "p"))
  expect_silent(add_eval(s, "falsify", "p"))
  expect_silent(add_eval(s, "F", "p"))
  expect_silent(add_eval(s, "frumpy", "p"))

  err <- "Criteria `r` have not been defined yet."
  expect_warning(add_eval(s, "c", "p | r"), err, fixed = TRUE)

  err <- "Criteria `q`, `r` have not been defined yet."
  expect_warning(add_eval(s, "c", "q & r"), err, fixed = TRUE)
})

test_that("default", {
  s2 <- add_eval(s, "c", "p")
  expect_equal(s2$hypotheses[[1]]$corroboration$evaluation, "p")
  expect_equal(s2$hypotheses[[1]]$corroboration$description, "")

  s2 <- add_eval(s, "corroboration", "dir & p", "desc")
  expect_equal(s2$hypotheses[[1]]$corroboration$evaluation, "dir & p")
  expect_equal(s2$hypotheses[[1]]$corroboration$description, "desc")

  s2 <- add_eval(s, "f", "!p", "desc")
  expect_equal(s2$hypotheses[[1]]$falsification$evaluation, "!p")
  expect_equal(s2$hypotheses[[1]]$falsification$description, "desc")

  s2 <- add_eval(s, "falsification", "!dir", "desc2")
  expect_equal(s2$hypotheses[[1]]$falsification$evaluation, "!dir")
  expect_equal(s2$hypotheses[[1]]$falsification$description, "desc2")
})


test_that("syntax checks", {
  err <- "The evaluation does not contain any criterion references"
  expect_error(add_eval(s, "c", ""), err, fixed = TRUE)
  expect_error(add_eval(s, "c", "&"), err, fixed = TRUE)
  expect_error(add_eval(s, "c", "|"), err, fixed = TRUE)
  expect_error(add_eval(s, "c", "( & | )"), err, fixed = TRUE)

  err <- "The evaluation doesn't parse; there is probably a typo"
  expect_error(add_eval(s, "c", "p &"), err, fixed = TRUE)
  expect_error(add_eval(s, "c", "& p"), err, fixed = TRUE)

  # these should work
  expect_silent(add_eval(s, "c", "(p)"))
  expect_silent(add_eval(s, "c", "(p&dir)"))
  expect_silent(add_eval(s, "c", "(p & dir)"))
  expect_silent(add_eval(s, "c", "p&dir"))
  expect_silent(add_eval(s, "c", "p & dir"))
  expect_silent(add_eval(s, "c", "(p|dir)"))
  expect_silent(add_eval(s, "c", "(p|!dir)"))
  expect_silent(add_eval(s, "c", "!p|!dir"))
  expect_silent(add_eval(s, "c", "!p|(!dir)"))
  expect_silent(add_eval(s, "c", "!p|!(dir)"))
})
