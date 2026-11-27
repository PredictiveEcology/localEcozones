## studyAreaGrid(): the grid of the study area, whether it is a SpatVector or sf (terra::vect() fails on a SpatVector)

test_that("a SpatVector study area and its sf give the same grid, at rasterToMatch's resolution and crs", {
  source(testthat::test_path("..", "..", "R", "localEcozones.R"), local = TRUE)
  rtm <- terra::rast(terra::ext(0, 10000, 0, 10000), resolution = 250, crs = "EPSG:3005")
  sa <- terra::vect("POLYGON ((1000 1000, 6000 1000, 6000 4000, 1000 4000, 1000 1000))", crs = "EPSG:3005")
  g <- studyAreaGrid(sa, rtm)
  expect_s4_class(g, "SpatRaster")
  expect_equal(terra::res(g), terra::res(rtm))
  expect_true(terra::same.crs(g, rtm))
  expect_equal(sum(!is.na(terra::values(g))), (5000 / 250) * (3000 / 250))
  skip_if_not_installed("sf")
  expect_equal(terra::values(studyAreaGrid(sf::st_as_sf(sa), rtm)), terra::values(g))
})
