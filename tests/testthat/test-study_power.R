s <- study() %>%
  add_hypothesis("H1") %>%
  add_analysis("A1", t.test(D1$Sepal.Length)) %>%
  add_criterion("C1", "p.value", "<", .05) %>%
  add_eval("corroboration", "C1", "the t-test is significant") %>%
  add_eval("falsification", "!C1", "the t-test is not significant") %>%
  add_data("D1", iris)

test_that("messages", {
  expect_message(study_power(s, 10), "The data `D1` will not be simulated, but be used as is for each analysis.", fixed = TRUE)
})

test_that("warnings", {
  s <- add_data(s, "D2")
  expect_warning(study_power(s, 10), "There is no data or design information for `D2`. Analyses that require this data are likely to fail.", fixed = TRUE)
})

test_that("errors", {
  s <- study()

  err_txt <- "The argument `rep` needs to be a positive number."
  expect_error(study_power(s, -10), err_txt, fixed = TRUE)
  expect_error(study_power(s, "a"), err_txt, fixed = TRUE)
  expect_error(study_power(s, 0.2), err_txt, fixed = TRUE)

  s <- study() %>% add_hypothesis()
  expect_error(study_power(s), "There are no analyses", fixed = TRUE)
})

test_that("basic", {
  study <- study() %>%
    add_hypothesis("H1") %>%
    add_analysis("A1", t.test(y~B1, data = D1)) %>%
    add_criterion("C1", "p.value", "<", 0.05) %>%
    add_analysis("A2", t.test(y~B1, data = D2)) %>%
    add_criterion("C2", "p.value", "<", 0.05) %>%
    add_eval("corroboration", "C1 | C2") %>%
    add_eval("falsification", "!C1 & !C2") %>%
    add_sim_data("D1", between = 2, n = 20, mu = c(0, 1)) %>%
    add_sim_data("D2", between = 2, n = 30, mu = c(0, 1))

  study_power <- study_power(study, 100)

  p <- study_power$hypotheses[[1]]$power

  expect_equal(names(p), c("corroboration",
                           "falsification",
                           "inconclusive"))

  expect_true(p$corroboration > 0.95)
  expect_true(p$falsification < 0.05)
  expect_true(p$inconclusive < 0.05)

  res <- study_power$analyses[[1]]$power

  expect_true(is.numeric(res$p.value))
  expect_equal(length(res$p.value), 100)
})

test_that("null", {
  study <- study() %>%
    add_hypothesis("H1") %>%
    add_analysis("A1", t.test(y~B1, data = D1)) %>%
    add_criterion("C1", "p.value", "<", 0.05) %>%
    add_eval("corroboration", "C1") %>%
    add_eval("falsification", "!C1") %>%
    add_sim_data("D1", between = 2, n = 20) %>%
    study_power(100)

  expect_true(study$hypotheses[[1]]$power$corroboration < .15)
})

test_that("accuracy", {
  skip("Takes forever")

  study <- study() %>%
    add_hypothesis("H1") %>%
    add_analysis("A1", t.test(y~B1, data = D1)) %>%
    add_criterion("C1", "p.value", "<", 0.05) %>%
    add_eval("corroboration", "C1") %>%
    add_eval("falsification", "!C1")

  power <- sapply(seq(0, 1, 0.1), function(d) {
     p <- add_sim_data(study, "D1", between = 2, n = 20, mu = c(0, d)) %>%
      study_power(100) %>%
      get_power()

     p$power$H1$corroboration
  })

  p2 <- lapply(seq(0, 1, 0.1), power.t.test, n = 20) %>%
    sapply(`[[`, "power")

  plot(power, p2)

  max_diff <- abs(power-p2) %>% max()

  expect_true(max_diff < .1)
})
