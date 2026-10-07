defineModule(sim, list(
  name = "localEcozones",
  description = paste("The `ecoregionLayer` for LandR: the provincial eco-zonation where one exists (BC: BEC",
                      "zones; Alberta: Natural Subregions), national ecodistricts",
                      "elsewhere. BEC zones and Natural Subregions follow elevation, so species parameters estimated by",
                      "Biomass_borealDataPrep per ecoregion are not shared between valley and subalpine."),
  keywords = c("ecoregion", "BEC", "Natural Subregions", "LandR"),
  authors = c(person("Eliot", "McIntire", email = "eliotmcintire@gmail.com", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(localEcozones = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list(),
  reqdPkgs = list("SpaDES.core", "bcdata", "dplyr", "geodata", "sf", "terra",
                  "PredictiveEcology/reproducible@development (>= 3.2.1.9061)"), # postProcessTo(rasterize)
  parameters = bindrows(
    defineParameter("bcLevel", "character", "ZONE", NA, NA,
                    "BEC level: 'ZONE' (e.g. CWH, MH, ESSF), 'SUBZONE' or 'MAP_LABEL'."),
    defineParameter("abLevel", "character", "NSRNAME", NA, NA,
                    paste("Alberta level: 'NSRNAME' (Natural Subregion; the Rocky Mountain region is split",
                          "into Alpine, Subalpine and Montane) or 'NRNAME' (Natural Region)."))
  ),
  inputObjects = bindrows(
    expectsInput("rasterToMatch_biomassParam", "SpatRaster",
                 "Grid of the area used to estimate species parameters; used first."),
    expectsInput("rasterToMatchLarge", "SpatRaster", "Used if rasterToMatch_biomassParam is missing."),
    expectsInput("studyArea_biomassParam", "SpatVector",
                 "Used (at the resolution of rasterToMatch) if neither raster above exists."),
    expectsInput("studyAreaLarge", "SpatVector", "Used if studyArea_biomassParam is missing."),
    expectsInput("rasterToMatch", "SpatRaster", "Template raster; the last fallback.")
  ),
  outputObjects = bindrows(
    createsOutput("ecoregionLayer", "sf",
                  paste("One polygon per ecoregion; field `ecoregion` (and the same in `ECODISTRIC`, which",
                        "Biomass_borealDataPrep uses by default), e.g. 'BC_CWH', 'AB_Subalpine',",
                        "'ED1012_500-1000m'."))
  )
))

doEvent.localEcozones <- function(sim, eventTime, eventType) {
  switch(eventType, init = {}, warning(noEventWarning(sim)))
  invisible(sim)
}

.inputObjects <- function(sim) {
  ## made here, before Biomass_borealDataPrep's .inputObjects would make the national default.
  ## It must cover the area the species parameters are estimated on (the *_biomassParam / *Large
  ## objects), which can be larger than rasterToMatch. The output is polygons, so the grid only
  ## sets the resolution of their edges.
  rtm <- Find(Negate(is.null), list(sim$rasterToMatch_biomassParam, sim$rasterToMatchLarge))
  if (is.null(rtm)) {
    sa <- Find(Negate(is.null), list(sim$studyArea_biomassParam, sim$studyAreaLarge))
    if (!is.null(sa) && !is.null(sim$rasterToMatch)) {
      sa <- terra::project(terra::vect(sa), terra::crs(sim$rasterToMatch))
      rtm <- terra::rasterize(sa, terra::rast(terra::ext(sa), resolution = terra::res(sim$rasterToMatch),
                                              crs = terra::crs(sim$rasterToMatch)))
    } else {
      rtm <- sim$rasterToMatch
    }
  }
  if (is.null(rtm)) stop("localEcozones needs rasterToMatch_biomassParam, rasterToMatchLarge, ",
                         "studyArea_biomassParam or studyAreaLarge (with rasterToMatch), or rasterToMatch")
  if (!suppliedElsewhere("ecoregionLayer", sim, where = "user")) {
    dPath <- getOption("reproducible.destinationPathShared", inputPath(sim))
    sim$ecoregionLayer <- localEcozones(rtm, destinationPath = dPath, bcLevel = P(sim)$bcLevel,
                                        abLevel = P(sim)$abLevel) |>
      reproducible::Cache(.functionName = "localEcozones")
  }
  invisible(sim)
}
