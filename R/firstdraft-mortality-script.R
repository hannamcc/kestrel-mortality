# This is what we want the data to look like at the end:

library(tidyverse)
data <- read_csv("LD_rmark_red_year.csv")
View(data)

#----Filter and clean up individuals data----
#Because of laziness I reused some functions from my own BBL stuff, so in order for this script to run, must save it and "use_bbl.R" in same working directory, set the working directory, run the source line. 

setwd()
source("use_bbl.R")


#Read in data, clean up, and make combine-able 
encounters <- bbl_read("from_bbl_may_2017_nothing_important/AMKE_encounterdb_201705111648.csv")

(encs_edit <- bbl_pretty(encounters, specify_cols = TRUE, cols = c("BAND_NUM", "B_SEX_CODE", "B_AGE_CODE", "B_FLYWAY_CODE", "BANDING_YEAR", "E_HOW_OBTAINED_CODE", "E_PRESENT_CONDITION_CODE", "ENCOUNTER_YEAR", "B_REGION", "B_LON_DECIMAL_DEGREES"), calc_cols = FALSE))

all_bands <- bbl_read("from_bbl_may_2017_nothing_important/AMKE_bandingdb_201705111647.csv", with_encs = FALSE)
(bands_edit <- bbl_pretty(all_bands, specify_cols = TRUE, cols = c("BAND_NUM", "SEX_CODE", "AGE_CODE", "FLYWAY_CODE", "BANDING_YEAR", "REGION", "LON_DECIMAL_DEGREES"), calc_cols = FALSE))
bands_edit$BAND_NUM <- parse_integer(bands_edit$BAND_NUM)
bands_edit$REGION <- parse_integer(bands_edit$REGION)
bands_edit

(bands_edit <- bands_edit %>%
     select(BAND_NUM, B_SEX_CODE = SEX_CODE, B_AGE_CODE = AGE_CODE, B_FLYWAY_CODE = FLYWAY_CODE, BANDING_YEAR, B_REGION = REGION, B_LON_DECIMAL_DEGREES = LON_DECIMAL_DEGREES))


# Filter and edit
encs_edit1 <- encs_edit %>%
     filter(B_SEX_CODE != 0) %>% #eliminate unknowns
     filter(E_PRESENT_CONDITION_CODE == 3 | E_PRESENT_CONDITION_CODE == 4 | E_PRESENT_CONDITION_CODE == 5) 
     #Only keep birds encountered dead
View(encs_edit1)
#Shouldn't be any duplicates now because birds don't die twice
encs_edit1 %>% filter(duplicated(.[["BAND_NUM"]])) 

bands_edit1 <- filter(bands_edit, B_SEX_CODE != 0)

#Combine banding and encounter records after filtering to cut down on the number of duplicates to wrangle
everything <- full_join(bands_edit1, encs_edit1, by = c("BAND_NUM")) %>% # important note: DID NOT do any making sure that the sex/age/etc lines up correctly between the two sets
     select(BAND_NUM, SEX = B_SEX_CODE.x, AGE = B_AGE_CODE.x, FLYWAY = B_FLYWAY_CODE.x, BANDING_YEAR = BANDING_YEAR.x, B_REGION = B_REGION.x, B_LON = B_LON_DECIMAL_DEGREES.x, ENCOUNTER_YEAR = ENCOUNTER_YEAR.y, E_HOW_OBTAINED = E_HOW_OBTAINED_CODE.y, E_CONDITION = E_PRESENT_CONDITION_CODE.y)

View(everything)

#Create new column and update it to be only HY or AHY
# 2, 3, 4 = HY
# All the rest = AHY
# Remove 0 = unknown

everything_edit <- mutate(everything, AGE_CLASS = "X") %>%
     filter(AGE != 0) #remove unknowns 

for(i in 1:nrow(everything_edit)){
     if (everything_edit$AGE[i] == 2 | everything_edit$AGE[i] == 3 | everything_edit$AGE[i] == 4){everything_edit$AGE_CLASS[i] <- "HY"} else {everything_edit$AGE_CLASS[i] <- "AHY"} 
}

#Edit the Canada, Mexico flyways into the 4
everything_edit1 <- filter(everything_edit, FLYWAY != 8 & FLYWAY != 9) #Remove South American and Carribean

#Change Alaska to Pacific
#Change Canada and Mexico

fw4 <- c(-136, -110)
fw3 <- c(-110, -90) #Changed the boundaries (from J. Heath, B. Pauli) so that they overlap. Can't deal with the little gap
fw2 <- c(-90, -82.7)
fw1 <- c(-82.7, -59)

for(i in 1:nrow(everything_edit1)){
     if(everything_edit1$FLYWAY[i] == 5){
          everything_edit1$FLYWAY[i] <- 4} else if (everything_edit1$FLYWAY[i] == 6){
               if(everything_edit1$B_LON[i] >= fw4[1] && everything_edit$B_LON <= fw4[2]){
                    everything_edit1$FLYWAY[i] <- 4} else if (everything_edit1$B_LON[i] > fw3[1] && everything_edit$B_LON <= fw3[2]){
                         everything_edit1$FLYWAY[i] <- 3} else if (everything_edit1$B_LON[i] > fw2[1] && everything_edit$B_LON <= fw2[2]){
                              everything_edit1$FLYWAY[i] <- 2} else if (everything_edit1$B_LON[i] > fw1[1] && everything_edit$B_LON <= fw1[2]){
                                   everything_edit1$FLYWAY[i] <- 1}
          } else if (everything_edit1$FLYWAY[i] == 7){
               if(everything_edit1$B_LON[i] >= fw4[1] && everything_edit$B_LON <= fw4[2]){
                    everything_edit1$FLYWAY[i] <- 4} else if (everything_edit2$B_LON[i] > fw3[1]){
                         everything_edit1$FLYWAY[i] <- 3}
          }
     }
                                                                                   
                                                                                   
View(everything_edit1)

#Change the sex to a character
everything_edit1 <- mutate(everything_edit1, SEX1 = "X")
for (i in 1:nrow(everything_edit1)){
     if(everything_edit1$SEX[i] == 4 || everything_edit1$SEX[i] == 6){everything_edit1$SEX1[i] <- "M"} else if(everything_edit1$SEX[i] == 5 || everything_edit1$SEX[i] == 7){everything_edit1$SEX1[i] <- "F"}
}

View(everything_edit1)

# Years to include in study: 1967-2015
everything_edit2 <- everything_edit1 %>%
     filter(BANDING_YEAR >= 1967 & BANDING_YEAR <= 2015) %>%
     filter(is.na(ENCOUNTER_YEAR) | (ENCOUNTER_YEAR >= 1967 & ENCOUNTER_YEAR <= 2015))
#Some data proofing/descriptive statistics now would be a good idea but I haven't done any

#Let's pretend I proofed the data and everything looks good
amke_individuals <- select(everything_edit2, BAND_NUM, sex = SEX1, age_class = AGE_CLASS, flyway = FLYWAY, b_year = BANDING_YEAR, e_year = ENCOUNTER_YEAR)
write_csv(amke_individuals, "individuals_mort_22_05_2017.csv")
View(amke_individuals)

#----Convert to LDLDLD format----
library(stringr)

make_ldld <- function(input, study_first_yr, study_last_yr){
     #for this function to work, banding year column must be named b_year, encounter year column must be named e_year
     reference_year1 <- study_first_yr
     reference_year2 <-study_last_yr
     
     study_length <- (reference_year2 - reference_year1) + 1
     
     #Create a string of 0's of length 98 (generalize for any study length)
     ch <- ""
     m = 0
     while(m < 2 * study_length){
          ch <- str_c(ch, "0")
          m <- m + 1
     }
     
     output <- mutate(input, ch = ch) #add the character string as a column in individuals data frame     
     
     for(i in 1:nrow(output)){
          # For each record, apply a function that does the following:
          # 1. use b_year to calculate integer position (1 to 98)
          # 2. use str_sub() to locate this position in ch and replace the character in this position with a 1
          # 3. repeat process for e_year
          
          # 1.
          band_position <- 2*(output$b_year[i] - (reference_year1-1))-1
          # 2.
          str_sub(output$ch[i], start = band_position, end = band_position) <- "1"
          # 3.
          if(!is.na(output$e_year[i])){
               enc_position <- 2*(output$e_year[i] - (reference_year1-1))
               str_sub(output$ch[i], start = enc_position, end = enc_position) <- "1"
          }
     }
     
     output
} #creates the ldld string

# Test function with small data set:
test_df <- amke_individuals %>%
     filter(b_year == 1967) %>%
     filter(sex == "F" & flyway == 4)
test_ldld <- make_ldld(test_df, 1967, 2015)
View(test_ldld)
     # Seems to work

amke_ldld <- make_ldld(amke_individuals, 1967, 2015)
View(amke_ldld)

#----Group into data with type/freq----
grouped_amke <- group_by(amke_ldld, ch, sex, age_class, flyway)
what_we_want <- summarise(grouped_amke, freq = n())
View(what_we_want)
sum(what_we_want$freq)

what_we_want$ch <- str_c("\"", what_we_want$ch, "\"") #add " to the ends of the string so excel doesn't eat the 0s
write_csv(what_we_want, "LD_amke_may2017.csv")
