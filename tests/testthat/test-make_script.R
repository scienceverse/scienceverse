test_that("default", {
  study <- study()
  x <- make_script(study) %>% strsplit("\n") %>% `[[`(1)
  expect_equal(x[1], "---")
  expect_equal(x[2], "title: \"Demo Study\"")
  expect_equal(x[3], "author: \"\"")
  expect_equal(x[4], format(Sys.Date(), "date: \"%d/%m/%Y\""))
  expect_equal(x[5], "output:")
  expect_equal(x[6], "  html_document:")
  expect_equal(x[7], "    toc: true")
  expect_equal(x[8], "    toc_float: true")
  expect_equal(x[9], "---")

  x <- make_script(study, use_rmarkdown = FALSE) %>%
    strsplit("\n") %>% `[[`(1)
  expect_equal(x[1], "# Code for Demo Study")
  expect_equal(x[2], "# Authors: ")
  expect_equal(x[3], format(Sys.Date(), "# Created %d/%m/%Y"))
})

test_that("write", {
  study <- study()
  if (file.exists("testfile.R")) file.remove("testfile.R")
  if (file.exists("testfile.Rmd")) file.remove("testfile.Rmd")

  # no suffix, default use_rmarkdown
  make_script(study, "testfile")
  fname <- "testfile.Rmd"
  expect_true(file.exists(fname))
  x <- readLines(fname)
  expect_equal(x[1], "---")
  expect_equal(x[2], "title: \"Demo Study\"")
  expect_equal(x[3], "author: \"\"")
  expect_equal(x[4], format(Sys.Date(), "date: \"%d/%m/%Y\""))
  expect_equal(x[5], "output:")
  if (file.exists(fname)) file.remove(fname)

  # .R suffix, default use_rmarkdown
  make_script(study, "testfile.R")
  fname <- "testfile.R"
  expect_true(file.exists(fname))
  x <- readLines(fname)
  expect_equal(x[1], "# Code for Demo Study")
  expect_equal(x[2], "# Authors: ")
  expect_equal(x[3], format(Sys.Date(), "# Created %d/%m/%Y"))
  if (file.exists(fname)) file.remove(fname)

  # no suffix, FALSE use_rmarkdown
  make_script(study, "testfile", use_rmarkdown = FALSE)
  fname <- "testfile.R"
  expect_true(file.exists(fname))
  x <- readLines(fname)
  expect_equal(x[1], "# Code for Demo Study")
  if (file.exists(fname)) file.remove(fname)

  # other suffix, TRUE use_rmarkdown
  make_script(study, "testfile.rmarkdown", use_rmarkdown = TRUE)
  fname <- "testfile.rmarkdown.Rmd"
  expect_true(file.exists(fname))
  x <- readLines(fname)
  expect_equal(x[2], "title: \"Demo Study\"")
  if (file.exists(fname)) file.remove(fname)

  # bad caps suffix, default use_rmarkdown
  make_script(study, "testfile.rmD")
  fname <- "testfile.Rmd"
  expect_true(file.exists(fname))
  x <- readLines(fname)
  expect_equal(x[2], "title: \"Demo Study\"")
  if (file.exists(fname)) file.remove(fname)
})


test_that("data and analysis", {

  study <- study() %>%
    add_data("mycars", mtcars)%>%
    add_analysis("A1", cor.test(mycars$mpg, mycars$disp))

  if (dir.exists("data")) unlink("data", recursive = TRUE)

  x_default <- make_script(study)
  x_rmd <- make_script(study, use_rmarkdown = TRUE)
  x_r <- make_script(study, use_rmarkdown = FALSE)
  x_ext <- make_script(study, data_path = "data")
  x_int <- make_script(study, data_path = NULL)
  x_cb <- make_script(study, show_codebook = TRUE)
  x_nocb <- make_script(study, show_codebook = FALSE)

  expect_equal(x_default, x_rmd)
  expect_equal(x_default, x_ext)
  expect_equal(x_default, x_cb)
  expect_false(isTRUE(all.equal(x_rmd, x_r)))
  expect_false(isTRUE(all.equal(x_ext, x_int)))
  expect_false(isTRUE(all.equal(x_cb, x_nocb)))

  expect_true(dir.exists("data"))
  expect_true(file.exists("data/mycars_data.tsv"))
  expect_true(file.exists("data/mycars_data.json"))

  if (dir.exists("data")) unlink("data", recursive = TRUE)
})

test_that("complex", {
  vardesc <- list(
    description = list(
      mpg = "Miles/(US) gallon",
      cyl = "Number of cylinders",
      disp = "Displacement (cu.in.)",
      hp = "Gross horsepower",
      drat = "Rear axle ratio",
      wt = "Weight (1000 lbs)",
      qsec = "1/4 mile time",
      vs = "Engine",
      am = "Transmission",
      gear = "Number of forward gears",
      carb = "Number of carburetors"
    ),
    # min and max values can be set manually or from data
    # min and max are often outside the observed range
    minValue = list(mpg = 0, cyl = min(mtcars$cyl)),
    maxValue = list(cyl = max(mtcars$cyl)),
    dataType = list(
      cyl = "integer",
      hp = "integer",
      vs = "integer",
      am = "integer",
      gear = "integer",
      carb = "integer"
    ),
    # supply levels to mark factors
    levels = list(
      vs = c("0" = "V-shaped", "1" = "straight"),
      am = c("0" = "automatic", "1" = "manual")
    )
  )
study <- study() %>%
  add_author("DeBruine", "Lisa", orcid = "0000-0002-7523-5539") %>%
  add_author("Lakens", "Daniel", orcid = "0000-0002-0247-239X") %>%
  add_data("mycars", mtcars, vardesc = vardesc) %>%
  add_analysis("A1", cor.test(mycars$mpg, mycars$wt)) %>%
  add_analysis("A2", t.test(rnorm(100))) %>%
  add_analysis("A3", x <- cor.test(mycars$mpg, mycars$wt),
               return = list(p = "x$p.value",
                             t = "x$statistic"
               )) %>%
  study_analyse()

  x_default <- make_script(study)
  x_rmd <- make_script(study, use_rmarkdown = TRUE)
  x_r <- make_script(study, use_rmarkdown = FALSE)
  x_ext <- make_script(study, data_path = "data")
  x_int <- make_script(study, data_path = NULL)
  x_cb <- make_script(study, show_codebook = TRUE)
  x_nocb <- make_script(study, show_codebook = FALSE)

  expect_equal(x_default, x_rmd)
  expect_equal(x_default, x_ext)
  expect_equal(x_default, x_cb)
  expect_false(isTRUE(all.equal(x_rmd, x_r)))
  expect_false(isTRUE(all.equal(x_ext, x_int)))
  expect_false(isTRUE(all.equal(x_cb, x_nocb)))

  if (file.exists("test.R")) file.remove("test.R")
  if (file.exists("test.Rmd")) file.remove("test.Rmd")
  if (file.exists("test.html")) file.remove("test.html")
  if (dir.exists("data")) unlink("data", recursive = TRUE)

  make_script(study, "test.R", data_path = NULL)
  make_script(study, "test.Rmd")
  rmarkdown::render("test.Rmd", quiet = TRUE)

  expect_true(file.exists("test.html"))

  if (file.exists("test.R")) file.remove("test.R")
  if (file.exists("test.Rmd")) file.remove("test.Rmd")
  if (file.exists("test.html")) file.remove("test.html")
  if (dir.exists("data")) unlink("data", recursive = TRUE)
})

test_that("levels", {
  s <- study() %>%
    add_hypothesis("H1", "First H") %>%
    add_analysis("A1", t.test(rnorm(10))) %>%
    add_criterion("p", "p.value", "<", .05) %>%
    add_eval("c", "p") %>%
    add_eval("f", "!p") %>%
    add_data("D1", cars) %>%
    study_analyse()

  default <- make_script(s)
  h1 <- make_script(s, header_lvl = 1)
  h2 <- make_script(s, header_lvl = 2)
  h3 <- make_script(s, header_lvl = 3)

  expect_equal(default, h2)

  expect_equal(grep("\n# Data\n", h1), 1)
  expect_equal(grep("\n## D1\n", h1), 1)
  expect_equal(grep("\n# Analysis 1: A1", h1), 1)
  expect_equal(grep("\n## Stored Results\n", h1), 1)

  expect_equal(grep("\n## Data\n", h2), 1)
  expect_equal(grep("\n### D1\n", h2), 1)
  expect_equal(grep("\n## Analysis 1: A1", h2), 1)
  expect_equal(grep("\n### Stored Results\n", h2), 1)

  expect_equal(grep("\n### Data\n", h3), 1)
  expect_equal(grep("\n#### D1\n", h3), 1)
  expect_equal(grep("\n### Analysis 1: A1", h3), 1)
  expect_equal(grep("\n#### Stored Results\n", h3), 1)
})


test_that("stored results", {
  s <- study() %>%
    add_analysis("A1", list(a = 1, b = "b", c = c(1, 2))) %>%
    study_analyse()

  x <- make_script(s) %>% strsplit("\n") %>% `[[`(1)
  quoteres <- c("* a: `1`",
                "* b: `b`",
                "* c: ",
                "  * `1`",
                "  * `2`")

  expect_equal(x[21:25], quoteres)

  x <- make_script(s, use_rmarkdown = FALSE) %>% strsplit("\n") %>% `[[`(1)
  quoteres <- c("# * a: 1",
                "# * b: b",
                "# * c: ",
                "#   * 1",
                "#   * 2")
  expect_equal(x[13:17], quoteres)
})
