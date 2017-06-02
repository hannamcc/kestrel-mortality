#BBL Regions for reading locations more easily in data files
bblregions <- read_csv("/Users/hannakim/Desktop/kestrel/DATA/bblregions.csv")

region_levs <- c(0:99, "CA", "US")
bblregions$REGION <- parse_factor(bblregions$REGION, levels = region_levs)

save(bblregions, file="data/bblregions.RData")

#Encounter data file
encounter_data <- read_csv("/Users/hannakim/Desktop/kestrel/DATA/from_bbl_may_2017/AMKE_encounterdb_201705111648.csv")

save(encounter_data, file = "data/AMKE_encounterdb_201705111648.RData")

#Banding data file
banding_data <- read_csv("/Users/hannakim/Desktop/kestrel/DATA/from_bbl_may_2017/AMKE_bandingdb_201705111647.csv")

save(banding_data, file = "data/AMKE_bandingdb_201705111647.RData")
