# output_info ----
test_that("info", {
  desc <- "This is my test study"
  other <- "Other info"
  s <- study("Test Study", description = desc, other = other) %>%
    add_author("DeBruine", "Lisa", "0000-0002-7523-5539", c("ana", "dat", "sof")) %>%
    add_author("Lakens", "Daniel", "0000-0002-0247-239X", c("val", "dra"))

  # suppress annoying cat() from format_output in test window
  scienceverse_options(verbose = FALSE)
  op <- output_info(s) %>% strsplit("\n") %>% `[[`(1)
  scienceverse_options(verbose = TRUE)

  expect_equal(op[[1]], "## Test Study")
  expect_equal(op[[3]], desc)
  expect_equal(op[[5]], "* other: Other info")
  expect_equal(op[[7]], "### Authors")
  expect_equal(op[[9]], "* **DeBruine, Lisa** ([0000-0002-7523-5539](https://orcid.org/0000-0002-7523-5539)): Data curation, Formal analysis, Software")
  expect_equal(op[[10]], "* **Lakens, Daniel** ([0000-0002-0247-239X](https://orcid.org/0000-0002-0247-239X)): Validation, Writing - original draft")

  # html
  scienceverse_options(verbose = FALSE)
  op <- output_info(s, output = "html") %>% strsplit("\n") %>% `[[`(1)
  scienceverse_options(verbose = TRUE)

  expect_equal(op[[1]], "<h2>Test Study</h2>")
  expect_equal(op[[3]], "<p>This is my test study</p>")
  expect_equal(op[[6]], "<li>other: Other info</li>")
  expect_equal(op[[9]], "<h3>Authors</h3>")
  expect_equal(op[[12]], "<li><strong>DeBruine, Lisa</strong> (<a href=\"https://orcid.org/0000-0002-7523-5539\">0000-0002-7523-5539</a>): Data curation, Formal analysis, Software</li>")

  # plain text
  scienceverse_options(verbose = FALSE)
  op <- output_info(s, output = "text") %>% strsplit("\n") %>% `[[`(1)
  scienceverse_options(verbose = TRUE)

  expect_equal(op[[1]], "Test Study")
  expect_equal(op[[3]], desc)
  expect_equal(op[[5]], "* other: Other info")
  expect_equal(op[[7]], "Authors")
})

# output_hypotheses ----
test_that("hypotheses", {
  s <- study() %>%
    add_hypothesis("H1") %>%
    add_analysis("A1", t.test(1:10, 2:11)) %>%
    add_criterion("C1", result = "p.value", operator = "<", comparator = 0.05)

  scienceverse_options(verbose = FALSE)
  op <- output_hypotheses(s) %>% strsplit("\n") %>% `[[`(1)
  scienceverse_options(verbose = TRUE)

  expect_equal(op[[1]], "## Hypotheses")
  expect_equal(op[[3]], "### Hypothesis 1: H1")
  expect_equal(op[[7]], "#### Criteria" )
})

test_that("hypotheses header_lvl", {
  s <- study() %>%
    add_hypothesis("H1") %>%
    add_analysis("A1", t.test(1:10, 2:11)) %>%
    add_criterion("C1", result = "p.value", operator = "<", comparator = 0.05)

  scienceverse_options(verbose = FALSE)
  op <- output_hypotheses(s, header_lvl = 3) %>%
    strsplit("\n") %>% `[[`(1)
  scienceverse_options(verbose = TRUE)

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

  scienceverse_options(verbose = FALSE)
  op <- output_hypotheses(s) %>% strsplit("\n") %>% `[[`(1)
  scienceverse_options(verbose = TRUE)

  expect_equal(op[[1]], "## Hypotheses")
  expect_equal(op[[3]], "### Hypothesis 1: H1")
  expect_equal(op[[7]], "#### Criteria" )
  expect_equal(op[[32]], "### Hypothesis 2: H2")
})

# output_analyses ----
test_that("analyses", {
  s <- study() %>%
    add_analysis("A1", t.test(1:10, 2:11))

  scienceverse_options(verbose = FALSE)
  op <- output_analyses(s) %>% strsplit("\n") %>% `[[`(1)
  scienceverse_options(verbose = TRUE)

  expect_equal(op[[1]], "## Analyses")
  expect_equal(op[[3]], "### Analysis 1: A1 {#A1}")
  expect_equal(op[[5]], "<pre>t.test(1:10, 2:11)</pre>")
})

test_that("analyses header_lvl", {
  s <- study() %>%
    add_analysis("A1", t.test(1:10, 2:11))

  scienceverse_options(verbose = FALSE)
  op <- output_analyses(s, header_lvl = 3) %>%
    strsplit("\n") %>% `[[`(1)
  scienceverse_options(verbose = TRUE)

  expect_equal(op[[1]], "### Analyses")
  expect_equal(op[[3]], "#### Analysis 1: A1 {#A1}")
  expect_equal(op[[5]], "<pre>t.test(1:10, 2:11)</pre>")
})

test_that("multiple analyses", {
  s <- study() %>%
    add_analysis("A1", t.test(1:10, 2:11)) %>%
    add_analysis("A2", cor(rnorm(10), rnorm(10)))

  scienceverse_options(verbose = FALSE)
  op <- output_analyses(s, header_lvl = 3) %>%
    strsplit("\n") %>% `[[`(1)
  scienceverse_options(verbose = TRUE)

  expect_equal(op[[1]], "### Analyses")
  expect_equal(op[[3]], "#### Analysis 1: A1 {#A1}")
  expect_equal(op[[5]], "<pre>t.test(1:10, 2:11)</pre>" )
  expect_equal(op[[7]], "#### Analysis 2: A2 {#A2}")
  expect_equal(op[[9]], "<pre>cor(rnorm(10), rnorm(10))</pre>" )
})

test_that("analyses with results", {
  scienceverse_options(verbose = FALSE)
  s <- study() %>%
    add_analysis("A1", t.test(1:10, 2:11)) %>%
    study_analyse()

  op <- output_analyses(s, header_lvl = 3) %>%
    strsplit("\n") %>% `[[`(1)

  op_T <- output_analyses(s, header_lvl = 3, results = TRUE) %>%
    strsplit("\n") %>% `[[`(1)

  op_F <- output_analyses(s, header_lvl = 3, results = FALSE) %>%
    strsplit("\n") %>% `[[`(1)
  scienceverse_options(verbose = TRUE)

  expect_equal(op[[16]], "    * mean of x: `5.5`")
  expect_equal(op[[17]], "    * mean of y: `6.5`")
  expect_equal(op[[19]], "    * difference in means: `0`")
  expect_equal(op[[23]], "* data.name: `1:10 and 2:11`")

  expect_equal(op_T, op)
  expect_equal(op_F, op_T[1:length(op_F)])
  expect_equal(length(op_F), 6L)
})

# output_results ----
test_that("results", {
  s <- study() %>%
    add_hypothesis("H1") %>%
    add_analysis("A1", t.test(1:10, 2:11)) %>%
    add_criterion("C1", result = "p.value", operator = "<", comparator = 0.05) %>%
    add_eval("c", "C1") %>%
    add_eval("f", "!C1") %>%
    study_analyse()

  scienceverse_options(verbose = FALSE)
  op <- output_results(s) %>% strsplit("\n") %>% `[[`(1)
  scienceverse_options(verbose = TRUE)

  expect_equal(op[[1]], "## Results")
  expect_equal(op[[3]], "### Hypothesis 1: H1" )
  expect_equal(op[[8]], "The result was p.value = 0.470 (<span style=\"color:red;\">FALSE</span>)  ")
})

test_that("results header_lvl", {
  s <- study() %>%
    add_hypothesis("H1") %>%
    add_analysis("A1", t.test(1:10, 2:11)) %>%
    add_criterion("C1", result = "p.value", operator = "<", comparator = 0.05) %>%
    add_eval("c", "C1") %>%
    add_eval("f", "!C1") %>%
    study_analyse()

  scienceverse_options(verbose = FALSE)
  op <- output_results(s, header_lvl = 1) %>%
    strsplit("\n") %>% `[[`(1)
  scienceverse_options(verbose = TRUE)

  expect_equal(op[[1]], "# Results")
  expect_equal(op[[3]], "## Hypothesis 1: H1" )
})

test_that("results digits", {
  s <- study() %>%
    add_hypothesis("H1") %>%
    add_analysis("A1", t.test(1:10, 2:11)) %>%
    add_criterion("C1", result = "p.value", operator = "<", comparator = 0.05) %>%
    add_eval("c", "C1") %>%
    add_eval("f", "!C1") %>%
    study_analyse()

  scienceverse_options(verbose = FALSE)
  op <- output_results(s, header_lvl = 1, digits = 4) %>%
    strsplit("\n") %>% `[[`(1)
  scienceverse_options(verbose = TRUE)

  expect_equal(op[[8]], "The result was p.value = 0.4697 (<span style=\"color:red;\">FALSE</span>)  ")

  scienceverse_options(verbose = FALSE)
  op <- output_results(s, header_lvl = 1, digits = 2) %>%
    strsplit("\n") %>% `[[`(1)
  scienceverse_options(verbose = TRUE)

  expect_equal(op[[8]], "The result was p.value = 0.47 (<span style=\"color:red;\">FALSE</span>)  ")

  scienceverse_options(verbose = FALSE)
  op <- output_results(s, header_lvl = 1) %>%
    strsplit("\n") %>% `[[`(1)
  scienceverse_options(verbose = TRUE)

  expect_equal(op[[8]], "The result was p.value = 0.470 (<span style=\"color:red;\">FALSE</span>)  ")


})


# output_data ----
test_that("data", {
  s <- study()

  scienceverse_options(verbose = FALSE)
  op <- output_data(s) %>% strsplit("\n") %>% `[[`(1)
  scienceverse_options(verbose = TRUE)

  expect_equal(op[[1]], "## Data")
  expect_equal(op[[3]], "No data")

  s <- study() %>% add_data("iris", iris)

  scienceverse_options(verbose = FALSE)
  op <- output_data(s) %>% strsplit("\n") %>% `[[`(1)
  scienceverse_options(verbose = TRUE)

  expect_equal(op[[1]], "## Data")
  expect_equal(op[[3]], "### iris")
  expect_equal(op[[7]], "* Sepal.Length (float)")
  expect_equal(op[[13]], "    * setosa")
})
