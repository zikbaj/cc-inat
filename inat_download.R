library(rgbif)
library(tidyverse)

# get keys for occurrence download
insecta_key <- name_backbone(name = "Insecta", rank = "class")$usageKey
inat_dataset <- datasets(query = "iNaturalist Research-grade Observations")
inat_key <- inat_dataset$data$key[1]

wkt_eastern_us <- "POLYGON((-95 24, -66 24, -66 48, -95 48, -95 24))"

occ_count(taxonKey = insecta_key,
          datasetKey = inat_key,
          geometry = wkt_eastern_us,
          year = "2015, 2026")
# download small sample for testing
inat_insecta_sample <- occ_data(
  taxonKey = insecta_key,
  datasetKey = inat_key,
  geometry = wkt_eastern_us,
  year = "2015, 2026",
  hasCoordinate = TRUE,
  limit = 1000)$data %>%
  select(decimalLatitude, decimalLongitude, acceptedTaxonKey, acceptedScientificName, order, family, genus, species, taxonRank, stateProvince, eventDate)

# full insecta download
user <- "zikiyik"
pwd <- "cc-inat"
email <- "benjjacosta@gmail.com"

inat_insecta <- occ_download(
  pred("taxonKey", insecta_key),
  pred("datasetKey", inat_key),
  pred("hasCoordinate", TRUE),
  pred("geometry", wkt_eastern_us),
  pred_in("year", c(2015:2026)),
  user = user, pwd = pwd, email = email,
  format = "SIMPLE_CSV")
