## Local eco-zonation for LandR's `ecoregionLayer`: the provincial zonation where we have one,
## national ecodistricts split by elevation band elsewhere. Each source is one function returning
## polygons with a `label` column; add a province by adding a function to `localZonations()`.

## Provinces that have a local zonation, and the function that gets it.
localZonations <- function() {
  list(`British Columbia` = zonesBC, Alberta = zonesAB)
}

## BC: Biogeoclimatic Ecosystem Classification (BEC), BC Data Catalogue. `level` is a BEC field:
## "ZONE" (e.g. CWH, MH, ESSF), "SUBZONE" or "MAP_LABEL" (zone + subzone + variant).
zonesBC <- function(aoi, destinationPath, level = "ZONE") {
  ## computed before the query: bcdata cannot translate a geometry built inside filter()
  box <- sf::st_as_sfc(sf::st_bbox(sf::st_transform(sf::st_as_sf(aoi), 3005)))
  v <- bcdata::bcdc_query_geodata("WHSE_FOREST_VEGETATION.BEC_BIOGEOCLIMATIC_POLY") |>
    bcdata::filter(bcdata::INTERSECTS(box)) |>
    bcdata::select(dplyr::all_of(unique(c("ZONE", level)))) |>
    bcdata::collect()
  if (identical(level, "SUBZONE")) v$label <- paste0(v$ZONE, v$SUBZONE) else v$label <- v[[level]]
  terra::vect(v[, "label"])
}

## Alberta: Natural Regions and Subregions of Alberta (2005), Open Government Licence - Alberta.
## `level` "NSRNAME" (subregion; follows elevation in the Rocky Mountain region: Alpine,
## Subalpine, Montane) or "NRNAME" (region).
zonesAB <- function(aoi, destinationPath, level = "NSRNAME") {
  f <- file.path(destinationPath, "natural_subregions_alberta_2005.geojson")
  if (!file.exists(f)) {
    url <- paste0("https://geospatial.alberta.ca/titan/rest/services/biota/",
                  "natural_subregions_alberta_2005/FeatureServer/0/query?",
                  "where=1%3D1&outFields=NSRNAME,NRNAME&outSR=4326&f=geojson")
    utils::download.file(url, f, mode = "wb", quiet = TRUE)
  }
  v <- terra::vect(f)
  v$label <- v[[level]][[1]]
  v[, "label"]
}

## Elsewhere: national ecodistricts (as Biomass_borealDataPrep's default), as a raster of ids on
## the grid of rasterToMatch.
zonesNational <- function(rasterToMatch, destinationPath) {
  reproducible::prepInputs(
    url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
    targetFile = "ecodistricts.shp", alsoExtract = "similar",
    destinationPath = destinationPath, fun = "terra::vect",
    to = rasterToMatch, rasterize = list(field = "ECODISTRIC"), verbose = -1)
}

## Label of each cell: the local zone (already prefixed by province, e.g. "BC_CWH") where there is
## one, else ecodistrict + elevation band (`elevationBand` m wide; NA: no bands).
ecozoneLabels <- function(local, national, elevation, elevationBand) {
  band <- if (is.na(elevationBand)) "" else {
    lo <- floor(elevation / elevationBand) * elevationBand
    ifelse(is.na(lo), "", paste0("_", lo, "-", lo + elevationBand, "m"))
  }
  out <- ifelse(is.na(local), paste0(national, band), local)
  out[is.na(local) & is.na(national)] <- NA_character_
  out
}

provinceAbbrev <- c(`British Columbia` = "BC", Alberta = "AB")

## The ecoregionLayer (sf): one polygon per label on the grid of rasterToMatch.
localEcozones <- function(rasterToMatch, destinationPath, bcLevel = "ZONE", abLevel = "NSRNAME",
                          elevationBand = 500) {
  aoi <- terra::as.polygons(terra::ext(rasterToMatch), crs = terra::crs(rasterToMatch))
  provinces <- geodata::gadm("CAN", level = 1, path = destinationPath)
  provinces <- terra::project(provinces, terra::crs(rasterToMatch))
  provinces <- provinces[terra::relate(provinces, aoi, "intersects")[, 1]]
  provR <- reproducible::postProcessTo(provinces, rasterToMatch, rasterize = list(field = "NAME_1"),
                                       verbose = -1)
  province <- as.character(terra::values(provR, dataframe = TRUE)[[1]])

  local <- rep(NA_character_, terra::ncell(rasterToMatch))
  zonations <- localZonations()
  for (p in intersect(names(zonations), unique(stats::na.omit(province)))) {
    level <- if (p == "British Columbia") bcLevel else abLevel
    z <- zonations[[p]](aoi = aoi, destinationPath = destinationPath, level = level)
    zr <- reproducible::postProcessTo(z, rasterToMatch, rasterize = list(field = "label"), verbose = -1)
    lab <- as.character(terra::values(zr, dataframe = TRUE)[[1]])
    ## the provincial layer defines its own extent; the province outline (coarse at the coast)
    ## only stops it spilling into a neighbouring province
    use <- !is.na(lab) & is.na(local) & (province %in% p | is.na(province))
    local[use] <- paste0(provinceAbbrev[[p]], "_", lab[use])
  }

  ## ecodistricts and elevation only where there is no local zone
  national <- terra::values(zonesNational(rasterToMatch, destinationPath))[, 1]
  national <- ifelse(is.na(national), NA_character_, paste0("ED", national))
  elevation <- if (is.na(elevationBand)) NULL else {
    dem <- geodata::elevation_30s(country = "CAN", path = destinationPath)
    terra::values(reproducible::postProcessTo(dem, rasterToMatch, method = "bilinear", verbose = -1))[, 1]
  }
  labels <- ecozoneLabels(local, national, elevation, elevationBand)
  labels[is.na(terra::values(rasterToMatch)[, 1])] <- NA_character_

  r <- terra::rast(rasterToMatch, nlyrs = 1)
  f <- factor(labels)
  r[] <- as.integer(f)
  levels(r) <- data.frame(ID = seq_along(levels(f)), ecoregion = levels(f))
  v <- sf::st_as_sf(terra::as.polygons(r, dissolve = TRUE))
  ## Biomass_borealDataPrep (LandR::prepEcoregions) uses the ECODISTRIC field when
  ## ecoregionLayerField is not set, so the same label is put there too
  v$ECODISTRIC <- v$ecoregion
  v
}
