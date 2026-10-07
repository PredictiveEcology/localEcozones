# localEcozones

SpaDES module that makes LandR's `ecoregionLayer` from the local eco-zonation where one exists:

- British Columbia: BEC zones (`bcLevel`, default `"ZONE"`), from the BC Data Catalogue via `bcdata`.
- Alberta: Natural Subregions (`abLevel`, default `"NSRNAME"`), Open Government Licence - Alberta.
- Elsewhere: the plain national ecodistrict (no elevation bands: zones shift with latitude, so a province-wide band is wrong).

BEC zones and Natural Subregions follow elevation, so `Biomass_borealDataPrep` does not apply valley parameters to the subalpine.
Add a province by adding a function to `localZonations()` in `R/localEcozones.R`.
