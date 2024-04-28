test_that("error", {
  expect_true(is.function(study_from_xml))

  expect_error(study_from_xml("no.exist"))

  expect_error(study_from_xml("iris.json"))

  filename <- "xml/2022-06120-001.xml"
  expect_error(study_from_xml(filename))
  expect_error(study_from_xml(filename, xml_type = "grobid"))
  expect_error(study_from_xml(filename, xml_type = "huh"))
})

test_that("basics", {
  filename <- "xml/grobid/DeBruine - 2002 - Facial resemblance enhances trust.pdf.tei.xml"
  s <- study_from_xml(filename)
  expect_equal(class(s), c("scivrs_study", "list"))
  expect_equal(s$name, "Facial resemblance enhances trust")
  expect_equal(s$info$title, "Facial resemblance enhances trust")
  lisa <- author(
   surname = "Debruine", given = "Lisa"#,
   #orcid = "0000-0002-7523-5539"
  )
  expect_equal(s$authors[[1]], lisa)

  expect_equal(substr(s$info$abstract, 1, 9), "Organisms")
})

test_that("iteration", {
  expect_error(study_from_xml("input-data"),
               "There are no xml files in the directory input-data",
               fixed = TRUE)

  s <- study_from_xml("xml/grobid")
  expect_equal(length(s), 2)
  expect_s3_class(s[[1]], "scivrs_study")
  expect_s3_class(s[[2]], "scivrs_study")
  expect_equal(s[[1]]$name, "Facial resemblance enhances trust")
  expect_equal(s[[2]]$name, "Movement synchrony and perceived entitativity")
})
