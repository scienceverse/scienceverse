grobid_dir <- system.file("grobid", package="scienceverse")

test_that("error", {
  expect_true(is.function(study_from_xml))

  # invalid file type
  expect_error(study_from_xml("no.exist"))
  expect_error(study_from_xml("iris.json"))

  # non-grobid XML
  filename <- tempfile(fileext = "xml")
  xml2::read_html("<p>Hello</p>") |>
    xml2::write_xml(filename, options = "as_xml")
  expect_error(study_from_xml(filename))
  expect_error(study_from_xml(filename, xml_type = "grobid"))
  expect_error(study_from_xml(filename, xml_type = "huh"))
})

test_that("basics", {
  filename <- file.path(grobid_dir, "incest.pdf.tei.xml")
  s <- study_from_xml(filename)
  expect_equal(class(s), c("scivrs_study", "list"))

  title <- "Having other-sex siblings predicts moral attitudes to sibling incest, but not parent-child incest"
  expect_equal(s$name, "incest.pdf.tei.xml")
  expect_equal(s$info$title, title)
  lisa <- author(
   surname = "Debruine", given = "Lisa",
   orcid = NULL,
   #orcid = "0000-0002-7523-5539",
   email = "lisa.debruine@glasgow.ac.uk"
  )
  expect_equal(s$authors[[1]], lisa)

  ben <- author(
    surname = "Jones", given = "Benedict",
    orcid = NULL,
    email = NULL
  )
  expect_equal(s$authors[[2]], ben)

  expect_equal(substr(s$info$description, 1, 5), "Moral")

  expect_equal(nrow(s$full_text), 89)
})

test_that("iteration", {
  expect_error(study_from_xml("input-data"),
               "There are no xml files in the directory input-data",
               fixed = TRUE)

  s <- study_from_xml(grobid_dir)

  expect_equal(length(s), 3)
  expect_s3_class(s[[1]], "scivrs_study")
  expect_s3_class(s[[2]], "scivrs_study")
  expect_s3_class(s[[3]], "scivrs_study")

  expect_equal(s[[1]]$name, "eyecolor.pdf.tei.xml")
  expect_equal(s[[2]]$name, "incest.pdf.tei.xml")
  expect_equal(s[[3]]$name, "prereg.pdf.tei.xml")

  expect_equal(s[[1]]$info$title, "Positive sexual imprinting for human eye color")
  expect_equal(s[[2]]$info$title, "Having other-sex siblings predicts moral attitudes to sibling incest, but not parent-child incest")
  expect_equal(s[[3]]$info$title, "Will knowledge about more efficient study designs increase the willingness to pre-register?")

  # search full text
  sig <- search_full_text(s, "significant")
  expect_equal(nrow(sig), 13)

  equal <- search_full_text(s, "=", section = "results")
  classes <- as.character(unique(equal$section_class))
  expect_equal(classes, "results")
})

test_that("search_full_text", {
  filename <- file.path(grobid_dir, "prereg.pdf.tei.xml")
  s <- study_from_xml(filename)
  alpha <- search_full_text(s, "alpha")

  expect_true(all(grepl("alpha", alpha$text)))
  expect_equal(nrow(alpha), 3)

  alpha_intro <- search_full_text(s, "alpha", "intro")
  expect_equal(nrow(alpha_intro), 2)

})
