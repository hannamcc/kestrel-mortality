library(tidyverse)
library(stringr)

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
     df$disp_type <- parse_factor(result, levels = disp_levels)
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
     longitude <- as_vector(df[,lon_col]) #vector of longitudes to use to calculate the flyways
     
     #Flyways from J. Heath, B. Pauli mortality study (longitude boundaries)
     #fw4 <- c(-136, -110)
     fw4 <- c(-141, -110)
     fw3 <- c(-110, -90)
     fw2 <- c(-90, -82.7)
     #fw1 <- c(-82.7, -59)
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
               } 
     }
     
     if(int_codes == TRUE){ #Add the integer codes as a column
          int_fw_levs <- c(1, 2, 3, 4, NA)
          if(encounter == TRUE){
               df$e_flyway <- parse_factor(result, levels = int_fw_levs)
          } else{ df$b_flyway <- parse_factor(result, levels = int_fw_levs)}
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
          chr_fw_levs <- c("Pacific", "Central", "Mississippi", "Atlantic")
          if(encounter == TRUE){
               df$e_flyway_chr <- parse_factor(result, levels = chr_fw_levs)
          } else{ df$b_flyway_chr <- parse_factor(result, levels = chr_fw_levs)}
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
}
