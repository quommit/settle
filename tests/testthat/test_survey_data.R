testthat::context("Survey data retrieval tests")

testthat::test_that("Method get_records retrieves hypertable data frame", {
  records <- get_records(tagarina_croplands)
  testthat::expect_is(records, "data.frame")
  testthat::expect_equal(records, tagarina_croplands$body$hypertable)
})

testthat::test_that("Method get_recordcount retrieves the right length", {
  count <- get_recordcount(tagarina_croplands)
  testthat::expect_equal(count, 460)
})

testthat::test_that("Method get_mapcatalogue retrieves data frame", {
  records <- get_mapcatalogue(tagarina_croplands)
  testthat::expect_is(records, "data.frame")
})

testthat::test_that("Method get_mapcatalogue retrieves consistent data", {
  first_geom_name <- names(tagarina_croplands$header$geometries)[1]
  first_geom_type <- class(tagarina_croplands$header$geometries[[1]])[1]
  records <- get_mapcatalogue(tagarina_croplands)
  testthat::expect_equal(grep(first_geom_name, records$mapname), 1)
  testthat::expect_true(1 %in% grep(first_geom_type, records$featuretype))
})

testthat::test_that("Method get_landholders retrieves character vector", {
  landholders <- get_landholders(tagarina_croplands)
  testthat::expect_is(landholders, "character")
})

testthat::test_that("Method get_landholders returns no duplicates", {
  landholders <- get_landholders(tagarina_croplands)
  testthat::expect_equal(anyDuplicated(landholders), 0)
})

testthat::test_that("Method nlandholders retrieves the rigth length", {
  count <- nlandholders(tagarina_croplands)
  testthat::expect_equal(count, 42)
})

testthat::test_that("Method get_agglevel_names retrieves character vector", {
  placenames <- get_agglevel_names(tagarina_croplands, 1)
  testthat::expect_is(placenames, "character")
})

testthat::test_that("Method get_agglevel_names returns no duplicates", {
  placenames <- get_agglevel_names(tagarina_croplands, 1)
  testthat::expect_equal(anyDuplicated(placenames), 0)
})

testthat::test_that("Method nagglevels retrieves the rigth length", {
  count <- nagglevels(tagarina_croplands)
  testthat::expect_equal(count, 2)
})

testthat::test_that("Method nplots retrieves the rigth length", {
  count <- nplots(tagarina_croplands)
  testthat::expect_equal(count, 64)
})