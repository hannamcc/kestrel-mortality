library(tidyverse)
library(stringr)
library(geosphere)
#Should update this because can't have null values in if statements -> write removal of nulls into functions, so it isn't necessary to do that ahead of time -> screws up parsing. read about nulls in ifs


# ----- read-parse-bbl -----
read_bbl <- function(file, with_encs = TRUE){
     
     region_levs <- c(0:99, "CA", "US")
     
     if(with_encs == TRUE){
          bbl_data <- read_csv(file, col_types = cols(
               B_LON_10_MIN_BLK = col_integer(),
               BAND_NUM = col_integer(),
               BANDING_DATE = col_date("%m/%d/%Y"),
               BANDING_DAY = col_integer(),
               BANDING_MONTH = col_integer(),
               BEARING = col_character(),
               E_10_MIN_BLOCK = col_integer(),
               E_ENC_COUNTY_ORIGINAL = col_integer(),
               E_HOW_OBTAINED_CODE = col_integer(),
               E_LAT_10_MIN_BLK = col_integer(),
               E_LON_10_MIN_BLK = col_integer(),
               E_PRESENT_CONDITION_CODE = col_integer(),
               E_STATE_CODE = col_integer(),
               ENCOUNTER_DATE = col_date("%m/%d/%Y"),
               ENCOUNTER_DAY = col_integer(),
               ENCOUNTER_MONTH = col_integer(),
               B_REGION = col_integer(),
               E_REGION = col_integer(),
               ORIGINAL_BAND = col_integer()))
          
          bbl_data$B_REGION <- parse_factor(bbl_data$B_REGION, levels = region_levs)
          bbl_data$E_REGION <- parse_factor(bbl_data$E_REGION, levels = region_levs) #re-parse as factors; do int 1st to remove leading 0s
     } else{
          bbl_data <- read_csv(file, col_types = cols(
               LAT_10_MIN_BLK = col_integer(),
               LON_10_MIN_BLK = col_integer(),
               BAND_NUM = col_integer(),
               BANDING_DATE = col_date("%m/%d/%Y"),
               BANDING_DAY = col_integer(),
               BANDING_MONTH = col_integer(),
               REGION = col_integer()))
          
          bbl_data$REGION <- parse_factor(bbl_data$REGION, levels = region_levs)
     }
     
     bbl_data <- mutate(bbl_data, ID = seq.int(nrow(bbl_data))) %>%
          select(ID, everything())
}

# ----- translate-codes -----
translate_sex <- function(df, var, lump_encounters = TRUE){
     result <-as_vector(df[,var]) #create vector of info to be updated
     
     #bbl_codes <- c(0, 4, 5, 6, 7)
     #output_codes <- c("U", "M", "F", "(E)M", "(E)F")
     
     if(lump_encounters == TRUE){
          for (i in 1:nrow(df)){
               if(result[i] == 0){result[i] <- "U"} else if(result[i] == 4 || result[i] == 6){result[i] <- "M"} else if(result[i] == 5 || result[i] == 7){result[i] <- "F"} #is there a more elegant way to do this?
          }
     } else {
          #separate out EF, EM (sexed upon recapture)
          for (i in 1:nrow(df)){
               if(result[i] == 0){result[i] <- "U"} else if(result[i] == 4){result[i] <- "M"} else if(result[i] == 5){result[i] <- "F"} else if(result[i] == 6){result[i] <- "EM"} else if(result[i] == 7){result[i] <- "EF"}
          }
     }
     
     sex_levs <- c("U", "M", "F", "EM", "EF")
     df$sex <- parse_factor(result, levels = sex_levs)
     return(df)
}

calc_disp_type <- function(df, var, num = TRUE){
     result <-as_vector(df[,var])
     
     if(num == TRUE){
          for(i in 1:nrow(df)){
               if(result[i] == 2 || result[i] == 3 || result[i] == 4){result[i] <- "N"} else if(result[i] == 0){result[i] <- "U"} else if(result[i] == 1 || result[i] >= 5){result[i] <- "B"}
          }
     } else{
          for(i in 1:nrow(df)){
               if(result[i] == "HY" || result[i] == "J" || result[i] == "L"){result[i] <- "N"} else if(result[i] == "U"){result[i] <- "U"} else if(result[i] == "AHY" || result[i] == "SY" || result[i] == "ASY" || result[i] == "TY" || result[i] == "ATY"){result[i] <- "B"}
          }
     }
     
     disp_levs <- c("U", "N", "B")
     df$disp_type <- parse_factor(result, levels = disp_levs)
     return(df)
}

translate_age <- function(df, var, group = FALSE){
     result <-as_vector(df[,var])
     
     if(group == FALSE){
          for(i in 1:nrow(df)){
               if(result[i] == 0) {result[i] <- "U"} else if (result[i] == 1) {result[i] <- "AHY"} else if (result[i] == 2) {result[i] <- "HY"} else if (result[i] == 3) {result[i] <- "J"} else if (result[i] == 4) {result[i] <- "L"} else if (result[i] == 5) {result[i] <- "SY"} else if (result[i] == 6) {result[i] <- "ASY"} else if (result[i] == 7) {result[i] <- "TY"} else if (result[i] == 8) {result[i] <- "ATY"}
          }
     } else{
          for(i in 1:nrow(df)){
               if(result[i] == 0) {result[i] <- "U"} else if (result[i] == 1 || result[i] >= 5) {result[i] <- "AHY"} else if (result[i] >= 2 && result[i] <= 4) {result[i] <- "HY"}
          }
     }
     
     age_levs <- c("U", "AHY", "HY", "J", "L", "SY", "ASY", "TY", "ATY")
     df$b_age <- parse_factor(result, levels = age_levs)
     return(df)
}

calc_flyways_four <- function(df, var, lon_col, encounter = FALSE, chr_codes = TRUE, int_codes = FALSE){
     result <-as_vector(df[,var])
     longitude <- as_vector(df[,lon_col]) #vector of longitudes to use to calculate the flyways (data frame column to reference) 
     
     #Flyways from J. Heath, B. Pauli mortality study (longitude boundaries)
     
     fw4 <- c(-141, -110)
     fw3 <- c(-110, -90)
     fw2 <- c(-90, -82.7)
     fw1 <- c(-82.7, -52)
     
     for(i in 1:nrow(df)){
          
          #calculate numeric flyway codes, store them in result
          #Change Alaska to Pacific
          #Change Canada and Mexico
          if(result[i] == 5){
               result[i] <- 4
               } else if (result[i] == 6){
                    if(longitude[i] >= fw4[1] && longitude[i] <= fw4[2]){
                         result[i] <- 4} else if (longitude[i] > fw3[1] && longitude[i] <= fw3[2]){
                              result[i] <- 3} else if (longitude[i] > fw2[1] && longitude[i] <= fw2[2]){
                                   result[i] <- 2} else if (longitude[i] > fw1[1] && longitude[i] <= fw1[2]){
                                        result[i] <- 1} else {result[i] <- NA}
               } else if (result[i] == 7){
                    if(longitude[i] >= fw4[1] && longitude[i] <= fw4[2]){
                         result[i] <- 4} else if (longitude[i] > fw3[1]){
                              result[i] <- 3} else {result[i] <- NA}
               } else {result[i] <- NA}
     }
     
     if(int_codes == TRUE){ #Add the integer codes as a column
          int_fw_levs <- c(1, 2, 3, 4, "NA")
          if(encounter == TRUE){
               df$E_FLYWAY <- parse_factor(result, levels = int_fw_levs)
          } else{ df$B_FLYWAY <- parse_factor(result, levels = int_fw_levs)}
     }
     
     if(chr_codes == TRUE){
          #use result (full of new int codes) to calculate chr codes, put into result
          for(i in 1:nrow(df)){
               if(result[i] == 1){
                    result[i] <- "Atlantic"
               } else if(result[i] == 2){
                    result[i] <- "Mississippi"
               } else if (result[i] == 3) {
                    result[i] <- "Central"
               } else if(result[i] == 4) {
                    result[i] <- "Pacific"
               } else {
                    result[i] <- "NA"
               }
          }
          
          #add chr result column
          chr_fw_levs <- c("Pacific", "Central", "Mississippi", "Atlantic", NA)
          if(encounter == TRUE){
               df$E_FLYWAY_CHR <- parse_factor(result, levels = chr_fw_levs)
          } else{ df$B_FLYWAY_CHR <- parse_factor(result, levels = chr_fw_levs)}
     }
     
     return(df)
}

translate_region <- function(){ #(df, var) inputs once the commented line below works
     region_levs <- c(0:99, "CA", "US")
          
     bblregions <- read_csv("data/bblregions.csv")
     bblregions$REGION <-parse_factor(bblregions$REGION, levels = region_levs)
     
     #df <- left_join(df, bblregions, by = c(var = "REGION"))
     #get this working
     
     return(bblregions)
} #this one doesn't work yet

# ----- inspect data by a column -----

inspect_bbl <- function(data_object, by, columns){
     
     inspect_matrix <- data_object %>%
          filter(!is.na(data_object[, by])) %>%
          select(one_of(columns))
     
     print(inspect_matrix, n = Inf, width = Inf)
     View(inspect_matrix)
}

# ----- calculate distance and bearing -----
#These functions aren't great yet because they require the variable names to be in very specific/unaltered format & the code is not great
calc_dist_km <- function(df){
     output <- mutate(df, dist_km = 0)
     
     b_loc_matrix <- select(df, B_LON_DECIMAL_DEGREES, B_LAT_DECIMAL_DEGREES)
     e_loc_matrix <- select(df, E_LON_DECIMAL_DEGREES, E_LAT_DECIMAL_DEGREES)
     
     output$dist_km = (distGeo(b_loc_matrix, e_loc_matrix)) / 1000
     
     output
}

calc_bearing_degs <- function(data_object){
     output <- mutate(data_object, bearing_degs = 0)
     
     b_loc_matrix <- select(data_object, B_LON_DECIMAL_DEGREES, B_LAT_DECIMAL_DEGREES)
     e_loc_matrix <- select(data_object, E_LON_DECIMAL_DEGREES, E_LAT_DECIMAL_DEGREES)
     
     output$bearing_degs = bearing(b_loc_matrix, e_loc_matrix)
     output
}

# ----- Write out specific columns -----
# Not sure if this function is going to stay, but I'll keep it here for now
# It is definitely nice not to have to specify all of the columns, but it's also sort of dumb
write_bbl <- function(data_object, dispersal_type = NULL, file_name){
     final_data <- data_object %>%
          select(ID, band_number = BAND_NUM, sex, b_age, b_status, b_date = BANDING_DATE, b_month = BANDING_MONTH, b_day = BANDING_DAY, b_year = BANDING_YEAR, e_date = ENCOUNTER_DATE, e_month = ENCOUNTER_MONTH, e_day = ENCOUNTER_DAY, e_year = ENCOUNTER_YEAR, e_status, e_who, disp_type, b_precision = B_COORD_PRECISION, b_lat = B_LAT_DECIMAL_DEGREES, b_lon = B_LON_DECIMAL_DEGREES, b_region = B_REGION, b_location, e_precision = E_COORD_PRECISION, e_lat = E_LAT_DECIMAL_DEGREES, e_lon = E_LON_DECIMAL_DEGREES, e_region = E_REGION, e_location, bbl_dist = DISTANCE, bbl_bearing = BEARING, same_block = SAME_10_MIN_BLOCK) 
     
     if(dispersal_type == "B" || dispersal_type == "breeding"){
          final_data <- filter(final_data, disp_type == "B")
     } else if(dispersal_type == "N" || dispersal_type == "natal"){
          final_data <- filter(final_data, disp_type == "N")
     }
     
     write_csv(final_data, "file_name")
}