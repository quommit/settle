testthat::context("Survey object creation tests")

testthat::test_that("Constructor stops when filename is not a string", {
  testthat::expect_error(survey(NULL), err_msg$path_not_valid)
  testthat::expect_error(survey(0), err_msg$path_not_valid)
})

testthat::test_that("Constructor stops when file is missing", {
  testthat::expect_error(survey("missing"), err_msg$file_missing)
})

testthat::test_that(paste("Constructor stops when file extension",
                          "does not match supported format"), {
  testthat::expect_error(survey("survey.notsupported"), 
                         err_msg$format_not_supported)
})

testthat::test_that(paste("Constructor stops when document schema",
                          "is missing"), {
  testthat::expect_error(survey("survey.nullschema.yml"),
                         err_msg$schema_is_missing)
  testthat::expect_error(survey("survey.nullschema.yml"),
                         err_msg$schema_is_missing)
})

testthat::test_that(paste("Constructor stops when document schema",
                          "is not supported"), {
  testthat::expect_error(survey("survey.badschema.yml"),
                         err_msg$schema_not_supported)
})