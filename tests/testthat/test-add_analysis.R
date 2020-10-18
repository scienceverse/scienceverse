# errors ----
test_that("errors", {
  s <- study()

  expect_error(add_analysis(s, NULL, "not_a_file.R", type = "file"),
               "The file not_a_file.R was not found.",
               fixed = TRUE)

  expect_error(add_analysis(s, NULL, 1),
               "The code was not a function.",
               fixed = TRUE)
})

# defaults ----
test_that("defaults", {
  s <- study() %>%
    add_analysis(NULL, t.test(rnorm(100)))

  expect_equal(length(s$analyses), 1)
  expect_equal(s$analyses[[1]]$id, 1)
  expect_equal(s$analyses[[1]]$func, function() {
    t.test(rnorm(100))
  })
  expect_equal(s$analyses[[1]]$code, "t.test(rnorm(100))")

})

# as text ----
test_that("as text", {
  s <- study() %>%
    add_analysis(NULL, "t.test(rnorm(100))", type = "text")
  expect_equal(length(s$analyses), 1)
  expect_equal(s$analyses[[1]]$id, 1)
  expect_equal(s$analyses[[1]]$func, function() {
    t.test(rnorm(100))
  })
  expect_equal(s$analyses[[1]]$code, "t.test(rnorm(100))")
})

# custom function ----
test_that("custom function", {
  myfunc <- function() {
    {
      a <- 1
      b <- 2
    }

    # return values
    list(
      "a" = a,
      "b" = b
    )
  }

  ct <- c(
    "{",
    "    a <- 1",
    "    b <- 2",
    "}",
    "# return values",
    "list(",
    "    `a` = a,",
    "    `b` = b",
    ")"
  )

  s2 <- study() %>%
    add_analysis(NULL, t.test(rnorm(100))) %>%
    add_analysis("a2", {
    a <- 1
    b <- 2
  }, c("a", "b"))
  expect_equal(length(s2$analyses), 2)
  expect_equal(s2$analyses[[2]]$id, "a2")
  expect_equal(s2$analyses[[2]]$func, myfunc)
  expect_equal(s2$analyses[[2]]$code, ct)

  expect_message(s3 <- add_analysis(s2, "a3 with spaces", a <- 1), "id \"a3 with spaces\" changed to \"a3_with_spaces\"")
  expect_equal(s3$analyses[[3]]$id, "a3_with_spaces")
})

# function with undefined data ----
test_that("undefined data", {
  s <- study() %>%
    add_analysis("A1", t.test(dat$a, dat$b))

  func <- function() { t.test(dat$a, dat$b) }
  expect_equal(s$analyses[[1]]$func, func)
})

# add from file ----
test_that("add from file", {
  #testthat::skip("only works in testthat")
  f <- "input-data/custom_code.R"
  s <- study() %>%
    add_analysis(NULL, f, type = "file")

  txt <- readLines(f)

  func <- function() { answer <- a + b }

  expect_equal(s$analyses[[1]]$func, func)
  expect_equal(s$analyses[[1]]$code, txt)
})


# add function with parse error ----
test_that("parse error", {
  s <- study()
  expect_error(
    add_analysis(s, "A1", "t.test(1:10", type = "text"),
    "The function analysis_A1 has errors.")
})

# return ----
test_that("return", {
  s <- study() %>%
    add_analysis("A1", { a = 1; b = 2 }, list("a", "b"))

  res <- s$analyses[[1]]$func()
  expect_equal(res, list(a = 1, b = 2))

  code <- output_custom_code(s)
  expect_equal(code, "{\n    a = 1\n    b = 2\n}\n# return values\nlist(\n    `a` = a,\n    `b` = b\n)")

  s <- study() %>%
    add_analysis("A1", { a = 1; b = 2 }, list(a1 = "a", a2 = "b"))

  res <- s$analyses[[1]]$func()
  expect_equal(res, list(a1 = 1, a2 = 2))

  code <- output_custom_code(s)
  expect_equal(code, "{\n    a = 1\n    b = 2\n}\n# return values\nlist(\n    `a1` = a,\n    `a2` = b\n)")

})

# extras ----
test_that("extras", {
  s <- study() %>%
    add_analysis("A1", rnorm(10), description = "10 random numbers")

  expect_equal(s$analyses[[1]]$description, "10 random numbers")
})

# update_analysis ----
test_that("update_analysis", {
  s <- study() %>%
    add_analysis("A1", rnorm(10), description = "10 random numbers") %>%
    update_analysis(1, rnorm(20), new_id = "ANA1", description = "20 rnorm")

  expect_equal(s$analyses[[1]]$id, "ANA1")
  expect_equal(s$analyses[[1]]$description, "20 rnorm")
  expect_equal(s$analyses[[1]]$func, function() { rnorm(20) })

  s <- study() %>%
    add_analysis("A1", rnorm(10), description = "10 random numbers") %>%
    update_analysis(1, "rnorm(20)", type = "text")

  expect_equal(s$analyses[[1]]$id, "A1")
  expect_equal(s$analyses[[1]]$description, "10 random numbers")
  expect_equal(s$analyses[[1]]$func, function() { rnorm(20) })
  code <- output_custom_code(s) %>% trimws()
  expect_equal(code, "rnorm(20)")

  # check new analysis is in the right environment
  s <- study() %>%
    add_hypothesis("H1") %>%
    add_data("i", iris) %>%
    add_analysis("A1", t.test(i$Sepal.Width)) %>%
    add_criterion("p1", "p.value", "<", 0.5) %>%
    add_analysis("A2", t.test(i$Sepal.Length)) %>%
    add_criterion("p1", "p.value", "<", 0.5) %>%
    study_analyse()
  env1 <- attr(s, "env")

  s2 <- update_analysis(s, "A2", t.test(i$Petal.Width)) %>%
    study_analyse()
  env2 <- attr(s2, "env")

  expect_equal(env1, env2)
  expect_equal(s2$analyses[[2]]$func, function() {t.test(i$Petal.Width)})

  # change returns
  s <- study() %>%
    add_analysis("A1", { a = 1; b = 2 }, list("a")) %>%
    study_analyse()

  expect_equal(s$analyses[[1]]$results, list(a = 1))

  s2 <- s %>%
    update_analysis("A1", return = list("b")) %>%
    study_analyse()

  expect_equal(s2$analyses[[1]]$results, list(b = 2))
})

