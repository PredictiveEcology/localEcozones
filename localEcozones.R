defineModule(sim, list(
  name = "localEcozones",
  description = paste("The `ecoregionLayer` for LandR: the provincial eco-zonation where one exists (BC: BEC",
                      "zones; Alberta: Natural Subregions), national ecodistricts split by elevation band",
                      "elsewhere. All of them follow elevation, so species parameters estimated by",
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
                          "into Alpine, Subalpine and Montane) or 'NRNAME' (Natural Region).")),
    defineParameter("elevationBand", "numeric", 500, 0, NA,
                    paste("Width (m) of the elevation bands that split national ecodistricts where there is",
                          "no local zonation. NA: no split."))
  ),
  inputObjects = bindrows(
    expectsInput("rasterToMatch_biomassParam", "SpatRaster",
                 "Grid of the area used to estimate species parameters. Falls back to `rasterToMatch`."),
    expectsInput("rasterToMatch", "SpatRaster", "Template raster.")
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
  ## made here, before Biomass_borealDataPrep's .inputObjects would make the national default
  rtm <- if (!is.null(sim$rasterToMatch_biomassParam)) sim$rasterToMatch_biomassParam else sim$rasterToMatch
  if (is.null(rtm)) stop("localEcozones needs rasterToMatch_biomassParam or rasterToMatch")
  if (!suppliedElsewhere("ecoregionLayer", sim, where = "user")) {
    dPath <- getOption("reproducible.destinationPathShared", inputPath(sim))
    sim$ecoregionLayer <- localEcozones(rtm, destinationPath = dPath, bcLevel = P(sim)$bcLevel,
                                        abLevel = P(sim)$abLevel, elevationBand = P(sim)$elevationBand) |>
      reproducible::Cache(.functionName = "localEcozones")
  }
  invisible(sim)
}
