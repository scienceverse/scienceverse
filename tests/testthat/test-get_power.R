study <- study() %>%
  add_hypothesis("H1") %>%
  add_analysis("A1", t.test(y~B1, data = D1)) %>%
  add_criterion("C1", "p.value", "<", 0.05) %>%
  add_analysis("A2", t.test(y~B1, data = D2)) %>%
  add_criterion("C2", "p.value", "<", 0.05) %>%
  add_eval("corroboration", "C1 | C2") %>%
  add_eval("falsification", "!C1 & !C2") %>%
  add_sim_data("D1", between = 2, n = 20, mu = c(0, 1)) %>%
  add_sim_data("D2", between = 2, n = 30, mu = c(0, 1)) %>%
  study_power(10)

test_that("default", {
  p <- get_power(study)
  expect_equal(names(p), "power")
  expect_equal(names(p$power), "H1")
  expect_equal(names(p$power$H1), c("corroboration",
                              "falsification",
                              "inconclusive"))
})

test_that("values", {
  p <- get_power(study, TRUE)
  expect_equal(names(p), c("power", "results"))
  expect_equal(names(p$power$H1), c("corroboration",
                              "falsification",
                              "inconclusive"))

  expect_equal(names(p$results), c("A1", "A2"))
  expect_equal(length(p$results$A1$p.value), 10)
  expect_equal(length(p$results$A2$p.value), 10)
})

