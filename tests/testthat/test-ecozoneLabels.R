## ecozoneLabels(): the local zone where there is one, else ecodistrict + elevation band.

test_that("local zones win; elsewhere ecodistrict and elevation band", {
  source(testthat::test_path("..", "..", "R", "localEcozones.R"), local = TRUE)
  out <- ecozoneLabels(local = c("BC_CWH", NA, NA, NA),
                       national = c("ED1", "ED2", "ED3", NA),
                       elevation = c(100, 700, NA, 900), elevationBand = 500)
  expect_identical(out, c("BC_CWH", "ED2_500-1000m", "ED3", NA))
})

test_that("elevationBand = NA leaves ecodistricts whole", {
  source(testthat::test_path("..", "..", "R", "localEcozones.R"), local = TRUE)
  expect_identical(ecozoneLabels(local = c(NA, "AB_Montane"), national = c("ED7", "ED8"),
                                 elevation = NULL, elevationBand = NA),
                   c("ED7", "AB_Montane"))
})
