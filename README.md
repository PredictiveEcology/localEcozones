# localEcozones

SpaDES module that makes LandR's `ecoregionLayer` from the local eco-zonation where one exists:

- British Columbia: BEC zones (`bcLevel`, default `"ZONE"`), from the BC Data Catalogue via `bcdata`.
- Alberta: Natural Subregions (`abLevel`, default `"NSRNAME"`), Open Government Licence - Alberta.
- Elsewhere: national ecodistricts split into `elevationBand` (default 500 m) bands.

All three follow elevation, so `Biomass_borealDataPrep` does not apply valley parameters to the subalpine.
Add a province by adding a function to `localZonations()` in `R/localEcozones.R`.
