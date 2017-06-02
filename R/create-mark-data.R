source("R/bbl-functions.R")

### Read in encounter data and banding data from BBL (downloaded from ftp May 2017)
encounters <- read_bbl("data/AMKE_encounterdb_201705111648.csv") %>%
     select(BAND_NUM, B_SEX_CODE, B_AGE_CODE, B_FLYWAY_CODE, BANDING_YEAR, E_HOW_OBTAINED_CODE, E_PRESENT_CONDITION_CODE, ENCOUNTER_YEAR, B_REGION, B_LON_DECIMAL_DEGREES)

all_bands <- read_bbl("data/AMKE_bandingdb_201705111647.csv", with_encs = FALSE) %>%
     select(BAND_NUM, B_SEX_CODE = SEX_CODE, B_AGE_CODE = AGE_CODE, B_FLYWAY_CODE = FLYWAY_CODE, BANDING_YEAR, B_REGION = REGION, B_LON_DECIMAL_DEGREES = LON_DECIMAL_DEGREES)

### Filter encounter data to dead only to eliminate multiple records for same band
recoveries <- filter(encounters, E_PRESENT_CONDITION_CODE == 3 | E_PRESENT_CONDITION_CODE == 4 | E_PRESENT_CONDITION_CODE == 5) #removes n = 2015
recoveries %>% filter(duplicated(.[["BAND_NUM"]])) # verify no duplicates
     #Important to keep in mind here that all of the bands that were encountered alive and removed from this set will "reappear" (bands only, not encounters) once I join this set with the banding records, as individuals with no "recovery" information

### Join encounters to banding records
bands_and_recoveries <- full_join(all_bands, recoveries, by = c("BAND_NUM"))

## double-check everything lines up between the duplicated columns
bands_and_recoveries %>%
     filter(!is.na(ENCOUNTER_YEAR)) %>%
     filter(B_SEX_CODE.x != B_SEX_CODE.y | B_AGE_CODE.x != B_AGE_CODE.y | B_FLYWAY_CODE.x != B_FLYWAY_CODE.y | BANDING_YEAR.x != BANDING_YEAR.y | B_REGION.x != B_REGION.y | B_LON_DECIMAL_DEGREES.x != B_LON_DECIMAL_DEGREES.y)

### After verifying columns match up for all records, eliminate duplicate columns
unfiltered_data <- select(bands_and_recoveries, BAND_NUM, SEX = B_SEX_CODE.x, AGE = B_AGE_CODE.x, FLYWAY = B_FLYWAY_CODE.x, BANDING_YEAR = BANDING_YEAR.x, B_REGION = B_REGION.x, B_LON = B_LON_DECIMAL_DEGREES.x, ENCOUNTER_YEAR, E_HOW_OBTAINED = E_HOW_OBTAINED_CODE, E_CONDITION = E_PRESENT_CONDITION_CODE)

### Filter data
filtered_data <- unfiltered_data %>%
     filter(SEX != 0) %>%  # remove unknown sex
     filter(AGE != 0) %>%  # remove unknown age
     filter(FLYWAY != 8 & FLYWAY != 9) %>% # remove Caribbean & South American records (left Mexico in)
     filter(FLYWAY!= 7) %>% # optional: remove mexico
     filter(BANDING_YEAR >= 1967 & BANDING_YEAR <= 2015) %>% 
     filter(is.na(ENCOUNTER_YEAR) | (ENCOUNTER_YEAR >= 1967 & ENCOUNTER_YEAR <= 2015)) %>% # filter to dates of study
     filter(is.na(E_HOW_OBTAINED) | !(AGE == 4 & E_HOW_OBTAINED == 30)) #remove locals died in nest, removes n = 19

     #n = 267118 / 266990 remaining (Mx, no Mx)
     
data_noHY <- filter(filtered_data, AGE != 02) # removes n = 73871 

### Transform some columns ----
# Sex (into M and F)
data_chr_traits <- translate_sex(filtered_data, "SEX")
data_chr_noHY <- translate_sex(data_noHY, "SEX")

# Age (into HY and AHY categories)
data_chr_traits <- translate_age(data_chr_traits, "AGE", group = TRUE) 
data_chr_noHY <- translate_age(data_chr_noHY, "AGE", group = TRUE)

#Flyway (into 4)
data_chr_traits <- calc_flyways_four(data_chr_traits, "FLYWAY", "B_LON", chr_codes = FALSE, int_codes = TRUE) 
data_chr_noHY <- calc_flyways_four(data_chr_noHY, "FLYWAY", "B_LON", chr_codes = FALSE, int_codes = TRUE)
# I changed the flyways because there were 208 records that occurred in Canada outside of the farthest east and farthest west specified by longitude. Check on this choice. (this could account for some of the larger sample size of mine)

### Proof the data, join regions in here to make sure the flyways look good 
bblregions <- translate_region() 
data_chr_region <-left_join(data_chr_traits, bblregions, by = c("B_REGION" = "REGION"))
flyways <- group_by(data_chr_region, FLYWAY, b_flyway, location)     
check_flyways <- summarise(flyways, count = n()) #flyways look good

#### Descriptive stats ----
group_by(data_chr_region, sex) %>%
     summarise(n())
group_by(data_chr_region, b_age) %>%
     summarise(n()) #A ton of HY
filter(filtered_data, AGE == 2) #if Julie, Ben didn't use HY (only locals), that would explain the discrepancy

### After proofing data, get rid of unneccessary columns
amke_individuals <- select(data_chr_traits, BAND_NUM, sex, age_class = b_age, flyway = FLYWAY, b_year = BANDING_YEAR, e_year = ENCOUNTER_YEAR)
amke_indivs_noHY <- select(data_chr_noHY, BAND_NUM, sex, age_class = b_age, flyway = FLYWAY, b_year = BANDING_YEAR, e_year = ENCOUNTER_YEAR)

### ----- Function: make MARK string -----
make_ldld <- function(df, study_first_yr, study_last_yr){
     
     reference_year1 <- study_first_yr
     reference_year2 <-study_last_yr
     
     study_length <- (reference_year2 - reference_year1) + 1
     
     #Create a string of 0's of length 2 * study length
     ch <- ""
     m = 0
     while(m < 2 * study_length){
          ch <- str_c(ch, "0")
          m <- m + 1
     }

     band_position <- vector(mode = "integer", length = nrow(df))
     enc_position <- vector(mode = "integer", length = nrow(df))
     result <- vector(mode = "character", length = nrow(df))
     
     for(i in 1:nrow(df)){result[i] = ch} #fill result with character strings
     for(i in 1:nrow(df)){
          band_position[i] <- 2*(df$b_year[i] - (reference_year1-1))-1 # create a vector of all the places the band "1" will go
     }
     for(i in 1:nrow(df)){
          str_sub(result[i], start = band_position[i], end = band_position[i]) <- "1" # update the strings in result with band "1"
     }
     for(i in 1:nrow(df)){
          if(!is.na(df$e_year[i])){
               enc_position[i] <- 2*(df$e_year[i] - (reference_year1-1)) # create a vector of all the places the enc "1" will go
          } else{
               enc_position[i] <- NA
          }
     }
     for(i in 1:nrow(df)){
          if(!is.na(enc_position[i])){
               str_sub(result[i], start = enc_position[i], end = enc_position[i]) <- "1"
          }
     }
     
     result <- str_c("\"", result, "\"") #add " to the ends of the string so excel doesn't eat the 0s
     df$ch <- result
     return(df)
} # updated version of function from first script (looks weird but much faster)

# Test function with small data set:
test_df <- amke_individuals %>%
     filter(b_year == 1967) %>%
     filter(sex == "F" & flyway == 4)
test_ldld <- make_ldld(test_df, 1967, 2015)
View(test_ldld)
# Seems to work

### Make mark string in dataset
amke_mark <- make_ldld(amke_individuals, 1967, 2015)
View(amke_mark)

#And again for no-HYs
amke_noHY_mark <- make_ldld(amke_indivs_noHY, 1967, 2015)

### Group into rows w/ frequencies
grouped_amke <- group_by(amke_mark, ch, sex, age_class, flyway)
final <- summarise(grouped_amke, freq = n())
View(final)
sum(final$freq)

#And again for no-HYs
grouped_amke_noHY <- group_by(amke_noHY_mark, ch, sex, age_class, flyway)
final_noHY <- summarise(grouped_amke_noHY, freq = n())

write_csv(final, "mark-data/LD_amke_may2017.csv")
write_csv(final_noHY, "mark-data/LD_amke_noHY_may2017.csv")

# I don't know why I'm ending up with more possible combos in the final document than Ben

#-------------- Re-filter following Julie's email ---------------
# Julie kept some of the Alive/Unknown present condition codes and then filtered those down based on how obtained:
recoveries2 <- filter(encounters, E_PRESENT_CONDITION_CODE != 06 & E_PRESENT_CONDITION_CODE != 07 & E_PRESENT_CONDITION_CODE != 08) #removes n = 1508
recoveries2 <- filter(recoveries2, E_PRESENT_CONDITION_CODE == 03 | E_PRESENT_CONDITION_CODE == 04 | E_PRESENT_CONDITION_CODE == 05 | (E_HOW_OBTAINED_CODE != 03 & E_HOW_OBTAINED_CODE != 21 & E_HOW_OBTAINED_CODE != 28 & E_HOW_OBTAINED_CODE != 29 & E_HOW_OBTAINED_CODE != 33 & E_HOW_OBTAINED_CODE != 36 & E_HOW_OBTAINED_CODE != 37 & E_HOW_OBTAINED_CODE != 46 & E_HOW_OBTAINED_CODE != 47 & E_HOW_OBTAINED_CODE != 53 & E_HOW_OBTAINED_CODE != 56 & E_HOW_OBTAINED_CODE != 66 & E_HOW_OBTAINED_CODE != 97 & E_HOW_OBTAINED_CODE != 98 ))
# removes n = 433, but the way this is set up now, all of these bands will stay in the set as non-mortality records, since they're iffy, they should maybe come out all together once the sets are joined

recoveries2 %>% filter(duplicated(.[["BAND_NUM"]])) # verify no duplicates, but there is one:

filter(recoveries2, BAND_NUM == 126346015) %>% # One duplicate - both encounters report the bird dead, but in different columns
     select(ENCOUNTER_YEAR, E_HOW_OBTAINED_CODE, E_PRESENT_CONDITION_CODE)
recoveries2 <- filter(recoveries2, BAND_NUM != 126346015 | E_PRESENT_CONDITION_CODE ==05) #remove one of the duplicates

#Join encounters and bands before filter anything else!
bands_and_recoveries2 <- full_join(all_bands, recoveries2, by = c("BAND_NUM")) 

unfiltered_data2 <- select(bands_and_recoveries2, BAND_NUM, SEX = B_SEX_CODE.x, AGE = B_AGE_CODE.x, FLYWAY = B_FLYWAY_CODE.x, BANDING_YEAR = BANDING_YEAR.x, B_REGION = B_REGION.x, B_LON = B_LON_DECIMAL_DEGREES.x, ENCOUNTER_YEAR, E_HOW_OBTAINED = E_HOW_OBTAINED_CODE, E_CONDITION = E_PRESENT_CONDITION_CODE)

filtered_data2 <- unfiltered_data2 %>%
     filter(SEX != 0) %>%  # remove unknown sex
     filter(AGE != 0) %>%  # remove unknown age
     filter(FLYWAY != 8 & FLYWAY != 9) %>% # remove Caribbean & South American records (left Mexico in)
     filter(FLYWAY!= 7) %>% #optional: remove mexico
     filter(BANDING_YEAR >= 1967 & BANDING_YEAR <= 2015) %>% 
     filter(is.na(ENCOUNTER_YEAR) | (ENCOUNTER_YEAR >= 1967 & ENCOUNTER_YEAR <= 2015)) %>%  # filter to dates of study
     filter(is.na(E_HOW_OBTAINED) | !(AGE == 04 & E_HOW_OBTAINED == 30))

#n = 267115 / 266987 remaining

data_noHY2 <- filter(filtered_data2, AGE != 02) # removes n = 73871

### Join the two different sets of filtered data to see what's different (my original and following Julie's email)
compare_sets <- full_join(filtered_data, filtered_data2, by = c("BAND_NUM"))
comparehy_sets <- full_join(filteredhy_data, filteredhy_data2, by = c("BAND_NUM"))
filter(compare_sets, is.na(BANDING_YEAR.x)) #0
filter(compare_sets, is.na(BANDING_YEAR.y)) #3: 178385863, 162335894, 178317149 (are in the data i originally created by not julie's, don't know why)

filter(comparehy_sets, is.na(BANDING_YEAR.x)) #0
filter(comparehy_sets, is.na(BANDING_YEAR.y)) #3: same as above
#just need to decide whether to keep those three or not, then just use one data set


