## Every external fetch is in reproducible's download ledger. The network is mocked.

test_that("zonesBC() records the BEC record in the download ledger", {
  source(testthat::test_path("..", "..", "R", "localEcozones.R"), local = TRUE)
  ## stand-in for the bcdata query
  bcBec <- function(box, level) {
    sf::st_sf(ZONE = "CWH", geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))), crs = 3005))
  }
  dest <- withr::local_tempdir()
  reproducible::clearUrlLog()
  withr::local_options(reproducible.urlLog = NULL)
  aoi <- terra::vect("POLYGON ((-125 50, -124 50, -124 51, -125 51, -125 50))", crs = "EPSG:4326")
  zonesBC(aoi, dest)
  log <- reproducible::prepInputsLog()
  expect_length(log, 1L)
  expect_equal(log[[1]]$url, bcBecRecordUrl)
  expect_equal(log[[1]]$fn, "zonesBC")
})

test_that("zonesAB() fetches the Natural Subregions with prepInputs(), which records them", {
  source(testthat::test_path("..", "..", "R", "localEcozones.R"), local = TRUE)
  dest <- withr::local_tempdir()
  seen <- list()
  testthat::local_mocked_bindings(
    prepInputs = function(url, targetFile, destinationPath, fun, ...) {
      seen <<- list(url = url, targetFile = targetFile, destinationPath = destinationPath)
      v <- terra::vect("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))", crs = "EPSG:4326")
      v$NSRNAME <- "Montane"
      v
    },
    .package = "reproducible")
  out <- zonesAB(NULL, dest)
  expect_match(seen$url, "^https://geospatial[.]alberta[.]ca/.*natural_subregions_alberta_2005")
  expect_equal(seen$targetFile, "natural_subregions_alberta_2005.geojson")
  expect_equal(seen$destinationPath, dest)
  expect_equal(out$label, "Montane")
})
