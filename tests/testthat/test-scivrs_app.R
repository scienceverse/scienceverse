test_that("exists", {
  expect_true(is.function(scivrs_app))

  expect_equal(scivrs_app, scienceverse::scivrs_app)

  expect_error(scivrs_app(1))

  skip("requires manual testing")

  # do not load anything, just save the default study
  s <- scivrs_app()

  expect_equal(s$name, "")
  expect_equivalent(s$authors, list())
  expect_s3_class(s$authors, "list")
  expect_s3_class(s$authors, "scivrs_authors")
  expect_equal(s$hypotheses, list())
  expect_equal(s$methods, list())
  expect_equal(s$data, list())
  expect_equal(s$analyses, list())
  expect_s3_class(s, "list")
  expect_s3_class(s, "scivrs_study")

  # click the demo study button then save
  d <- scivrs_app()

  expect_equal(d$name, "kingames")
  expect_equivalent(d$authors[[1]]$email, "lisa.debruine@glasgow.ac.uk")
  expect_equivalent(d$authors[[2]]$name$given, "Daniël")
  expect_equal(d$hypotheses[[1]]$id, "self_pref")
  expect_equal(d$methods[[1]]$id, "stimuli")
  expect_s3_class(d$data[[1]]$codebook, "psychds_codebook")
  expect_equal(d$analyses[[1]]$id, "trust")
  expect_s3_class(d, "list")
  expect_s3_class(d, "scivrs_study")

  # load from object and save
  d2 <- scivrs_app(d)

  expect_equal(d2$name, "kingames")
  expect_equivalent(d2$authors[[1]]$email, "lisa.debruine@glasgow.ac.uk")
  expect_equivalent(d2$authors[[2]]$name$given, "Daniël")
  expect_equal(d2$hypotheses[[1]]$id, "self_pref")
  expect_equal(d2$methods[[1]]$id, "stimuli")
  expect_s3_class(d2$data[[1]]$codebook, "psychds_codebook")
  expect_equal(d2$analyses[[1]]$id, "trust")
  expect_s3_class(d2, "list")
  expect_s3_class(d2, "scivrs_study")
})
