# Data set creation update
# June 5, 2017

# Things that this update includes (reflected in Readme-v2)
# 1. Check that there are no HY designated birds between Jan 1 & Apr 15 (there were 96, changed to AHY)
# 2. Create 2 pre-join filtered encounter data sets:
     # 1 that is what I have been making, the dead encounters only for inclusion in the analysis
     # 1 that is 'for removal' encounter set, so that when the sets are joined these records can be removed completely
# 3. Update years to include 2016

##------------ Script -------------

source("R/bbl-functions.R")

### Read in encounter data and banding data from BBL (downloaded from ftp May 2017)
encounters <- read_bbl("data/AMKE_encounterdb_201705111648.csv") %>%
     select(BAND_NUM, B_SEX_CODE, B_AGE_CODE, B_FLYWAY_CODE, BANDING_YEAR, E_HOW_OBTAINED_CODE, E_PRESENT_CONDITION_CODE, ENCOUNTER_YEAR, B_REGION, B_LON_DECIMAL_DEGREES)

all_bands <- read_bbl("data/AMKE_bandingdb_201705111647.csv", with_encs = FALSE) %>%
     select(BAND_NUM, B_SEX_CODE = SEX_CODE, B_AGE_CODE = AGE_CODE, B_FLYWAY_CODE = FLYWAY_CODE, BANDING_YEAR, B_REGION = REGION, B_LON_DECIMAL_DEGREES = LON_DECIMAL_DEGREES, BANDING_MONTH, BANDING_DAY)

#------------ Create dataframe of dead encounters for inclusion -----------
## Using only the 'dead' encounter codes for more conservative approach (includes 50 fewer encounters in final data set than Julie's criteria)

recoveries <- filter(encounters, E_PRESENT_CONDITION_CODE == 3 | E_PRESENT_CONDITION_CODE == 4 | E_PRESENT_CONDITION_CODE == 5) #removes n = 2015
recoveries %>% filter(duplicated(.[["BAND_NUM"]])) # verify no duplicates


#------------ Create dataframe of encounters to remove from records -------
# Create a dataframe of the records that we don't want included at all

removals <- encounters %>%
     filter(E_PRESENT_CONDITION_CODE != 3 & E_PRESENT_CONDITION_CODE != 4 & E_PRESENT_CONDITION_CODE != 5) %>% # don't include the dead encounters we want, n=3484
     filter(E_PRESENT_CONDITION_CODE != 6 & E_PRESENT_CONDITION_CODE != 7 & E_PRESENT_CONDITION_CODE != 8) %>% # don't include the birds that were encountered alive and released, n=1508
     filter(!((E_PRESENT_CONDITION_CODE == 0 | E_PRESENT_CONDITION_CODE == 1 | E_PRESENT_CONDITION_CODE == 2) & E_HOW_OBTAINED_CODE == 66)) # unknown present condition but obtained by trapping/banding and releasing, n = 67

# ----- Inspect remaining: -----
filter(removals, E_PRESENT_CONDITION_CODE == 0 | E_PRESENT_CONDITION_CODE == 1 | E_PRESENT_CONDITION_CODE == 2) %>% # unknown present conditions, n = 271
     group_by(E_HOW_OBTAINED_CODE) %>%
     summarise(n())

filter(removals, E_PRESENT_CONDITION_CODE == 9 | E_PRESENT_CONDITION_CODE == 10 | E_PRESENT_CONDITION_CODE == 11) %>% # alive, captivity, n = 203
     group_by(E_HOW_OBTAINED_CODE) %>%
     summarise(n())
#these should all come out of data, keep in removals

filter(removals, E_PRESENT_CONDITION_CODE == 12 | E_PRESENT_CONDITION_CODE == 13 | E_PRESENT_CONDITION_CODE == 14) %>% # alive, unknown, n = 33
     group_by(E_HOW_OBTAINED_CODE) %>%
     summarise(n())
#these should all come out of data, keep in removals


# ----- Duplicate band records ------
## Check and see if birds that have a weird alive encounter record later appear as a dead bird. In this case, take the alive record out of the removals set because we want these to stay in data as dead band recoveries

View(inner_join(recoveries, removals, by = c("BAND_NUM"))%>%
          select(BAND_NUM, ENCOUNTER_YEAR.x, ENCOUNTER_YEAR.y, E_PRESENT_CONDITION_CODE.x, E_PRESENT_CONDITION_CODE.y, E_HOW_OBTAINED_CODE.x, E_HOW_OBTAINED_CODE.y)) # n = 13, might want to look further at the ones where the two encounters don't occur in the same year to determine which year to use (this looks like place BBL needs to continue to clean up, it's weird there are two different records for some of these)

### Remove these records from removals
removals <- anti_join(removals, recoveries, by = c("BAND_NUM"))

# ----- Add a column to make it easy to remove all of these from joined data set -----
# n = 427 to be totally removed from data: unknown or alive present condition with ambiguous how obtained codes

removals <- mutate(removals, remove = "Yes")

# -------------- Join bands, recoveries, and removals ------------------
### Join encounters to banding records
bands_and_recoveries <- full_join(all_bands, recoveries, by = c("BAND_NUM"))

## Double-check everything lines up between the duplicated columns & remove duplicate columns ------
bands_and_recoveries %>%
     filter(!is.na(ENCOUNTER_YEAR)) %>%
     filter(B_SEX_CODE.x != B_SEX_CODE.y | B_AGE_CODE.x != B_AGE_CODE.y | B_FLYWAY_CODE.x != B_FLYWAY_CODE.y | BANDING_YEAR.x != BANDING_YEAR.y | B_REGION.x != B_REGION.y | B_LON_DECIMAL_DEGREES.x != B_LON_DECIMAL_DEGREES.y) 

### After verifying columns match up for all records, eliminate duplicate columns
bands_and_recoveries <- bands_and_recoveries %>%
     select(BAND_NUM, SEX = B_SEX_CODE.x, AGE = B_AGE_CODE.x, FLYWAY = B_FLYWAY_CODE.x, BANDING_YEAR = BANDING_YEAR.x, B_REGION = B_REGION.x, B_LON = B_LON_DECIMAL_DEGREES.x, ENCOUNTER_YEAR, E_HOW_OBTAINED = E_HOW_OBTAINED_CODE, E_CONDITION = E_PRESENT_CONDITION_CODE) 


### Join removals to banding records & remove the removals from data -------
bands_and_recoveries <- full_join(bands_and_recoveries, removals, by = c("BAND_NUM"))  

removals %>% filter(duplicated(.[["BAND_NUM"]]))
bands_and_recoveries %>% filter(duplicated(.[["BAND_NUM"]])) #removals adds some duplicates, but removing the removals will remove the duplicates

## double-check everything lines up between the duplicated columns
bands_and_recoveries %>%
     filter(!is.na(ENCOUNTER_YEAR.x) & !is.na(ENCOUNTER_YEAR.y)) %>%
     filter(B_SEX_CODE.x != B_SEX_CODE.y | B_AGE_CODE.x != B_AGE_CODE.y | B_FLYWAY_CODE.x != B_FLYWAY_CODE.y | BANDING_YEAR.x != BANDING_YEAR.y | B_REGION.x != B_REGION.y | B_LON_DECIMAL_DEGREES.x != B_LON_DECIMAL_DEGREES.y)

### Remove the records marked 'remove'
bands_and_recoveries <- filter(bands_and_recoveries, is.na(remove))

### Eliminate duplicate/empty columns
unfiltered_data <- bands_and_recoveries %>%
     select(BAND_NUM, SEX, AGE, FLYWAY, BANDING_YEAR = BANDING_YEAR.x, B_REGION = B_REGION.x, B_LON, ENCOUNTER_YEAR = ENCOUNTER_YEAR.x, E_HOW_OBTAINED, E_CONDITION) 


#------------- Filter data ------------------

### Filter data *Update to include 2016
filtered_data <- unfiltered_data %>%
     filter(SEX != 0) %>%  # remove unknown sex
     filter(AGE != 0) %>%  # remove unknown age
     filter(FLYWAY != 8 & FLYWAY != 9) %>% # remove Caribbean & South American records (left Mexico in)
     filter(FLYWAY!= 7) %>% # optional: remove Mexico
     filter(BANDING_YEAR >= 1967 & BANDING_YEAR <= 2016) %>% 
     filter(is.na(ENCOUNTER_YEAR) | (ENCOUNTER_YEAR >= 1967 & ENCOUNTER_YEAR <= 2016)) %>% # filter to dates of study
     filter(is.na(E_HOW_OBTAINED) | !(AGE == 4 & E_HOW_OBTAINED == 30)) #remove locals died in nest, removes n = 14

# n = 272349 
# n = 2538 encounter records

##-------Proof the HY codes---------
hatch_years <- left_join(filtered_data, all_bands, by = c("BAND_NUM")) %>%
     select(BAND_NUM, SEX, AGE, BANDING_YEAR.x, BANDING_MONTH, BANDING_DAY) %>%
     filter(AGE == 02)

filter(hatch_years, is.na(BANDING_MONTH)) # n = 0
filter(hatch_years, BANDING_MONTH > 12) # n = 0
HY_to_AHY <- filter(hatch_years, BANDING_MONTH < 4 | (BANDING_MONTH == 4 & BANDING_DAY < 15)) # n = 96

## Update the 96 bad HY codes to AHY. -----
errant_HY <- as_vector(HY_to_AHY[,"BAND_NUM"])
band_vector <- as_vector(filtered_data[,"BAND_NUM"])
result <- as_vector(filtered_data[,"AGE"])

for(i in 1:nrow(filtered_data)){
     if(band_vector[i] %in% errant_HY){
          result[i] <- 1
     }
}

filtered_data <- mutate(filtered_data, AGE_update = result) %>%
     select(BAND_NUM, SEX, AGE, AGE_update, everything()) #proof new age column

filtered_data <- select(filtered_data, -AGE)
filtered_data <- rename(filtered_data, AGE = AGE_update)

# Remove HYs -----
data_noHY <- filter(filtered_data, AGE != 02) # removes n = 74610 

### Transform some columns -------
# Sex (into M and F)
data_chr_traits <- translate_sex(filtered_data, "SEX")
data_chr_noHY <- translate_sex(data_noHY, "SEX")

# Age (into HY and AHY categories)
data_chr_traits <- translate_age(data_chr_traits, "AGE", group = TRUE) 
data_chr_noHY <- translate_age(data_chr_noHY, "AGE", group = TRUE)

#Flyway (into 4)
data_chr_traits <- calc_flyways_four(data_chr_traits, "FLYWAY", "B_LON", chr_codes = FALSE, int_codes = TRUE) 
data_chr_noHY <- calc_flyways_four(data_chr_noHY, "FLYWAY", "B_LON", chr_codes = FALSE, int_codes = TRUE)


### Proof the data / descriptive stats -------
## Join regions in here to make sure the flyways look good 
bblregions <- translate_region() 
data_chr_region <-left_join(data_chr_traits, bblregions, by = c("B_REGION" = "REGION"))
flyways <- group_by(data_chr_region, FLYWAY, b_flyway, location)     
check_flyways <- summarise(flyways, count = n()) #flyways look good

### Descriptive stats 
group_by(data_chr_region, sex) %>%
     summarise(n())
group_by(data_chr_region, b_age) %>%
     summarise(n()) #A ton of HY
filter(filtered_data, AGE == 2) 


### After proofing data, get rid of unneccessary columns ----
amke_individuals <- select(data_chr_traits, BAND_NUM, sex, age_class = b_age, flyway = FLYWAY, b_year = BANDING_YEAR, e_year = ENCOUNTER_YEAR)
amke_indivs_noHY <- select(data_chr_noHY, BAND_NUM, sex, age_class = b_age, flyway = FLYWAY, b_year = BANDING_YEAR, e_year = ENCOUNTER_YEAR)

## Data + ggplot2 ----


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
test_ldld <- make_ldld(test_df, 1967, 2016)
View(test_ldld)
# Seems to work

### Make mark string in dataset -----
amke_mark <- make_ldld(amke_individuals, 1967, 2016)
View(amke_mark)

#And again for no-HYs
amke_noHY_mark <- make_ldld(amke_indivs_noHY, 1967, 2016)

### Group into rows w/ frequencies
grouped_amke <- group_by(amke_mark, ch, sex, age_class, flyway)
final <- summarise(grouped_amke, freq = n())
View(final)
sum(final$freq)

#And again for no-HYs
grouped_amke_noHY <- group_by(amke_noHY_mark, ch, sex, age_class, flyway)
final_noHY <- summarise(grouped_amke_noHY, freq = n())

write_csv(final, "mark-data/LD_amke_6june2017.csv")
write_csv(final_noHY, "mark-data/LD_amke_noHY_6june2017.csv")
