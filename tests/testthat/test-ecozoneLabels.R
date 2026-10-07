## ecozoneLabels(): the local zone where there is one, else the plain ecodistrict.

test_that("local zones win; elsewhere the plain ecodistrict, with no elevation suffix", {
  source(testthat::test_path("..", "..", "R", "localEcozones.R"), local = TRUE)
  out <- ecozoneLabels(local = c("BC_CWH", NA, NA, "AB_Montane", NA),
                       national = c("ED1", "ED2", "ED3", "ED4", NA))
  expect_identical(out, c("BC_CWH", "ED2", "ED3", "AB_Montane", NA))
  expect_false(any(grepl("_.*m$", out[c(2, 3)])))
})

test_that("elevation bands are gone", {
  source(testthat::test_path("..", "..", "R", "localEcozones.R"), local = TRUE)
  expect_false("elevationBand" %in% names(formals(localEcozones)))
  expect_false("elevationBand" %in% names(formals(ecozoneLabels)))
})
