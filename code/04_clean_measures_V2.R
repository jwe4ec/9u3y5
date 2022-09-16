# --------------------------------------------------------------------------- #
# Clean demographics, bias, and COVID anxiety measures for TET and GIDI
# Author: Tylar N. Schmitt 
# Inclusion criteria for the final sample: 
# 1. Participants who are from the United States 
# 2. Participants who have complete and valid COVID-19 Anxiety data 
# 3. Participants who endorsed race as Black/African origin or White/European 
# OR ethnicity as Hispanic/Latinx or not Hipanic/Latinx

# --------------------------------------------------------------------------- #
# Set working directory to parent analyses folder ---- 
# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# Check R version and load packages ----
# --------------------------------------------------------------------------- #

# Load custom functions
source("./2022.08.04 Code/00_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day
groundhog_day <- version_control()

# Load packages with groundhog
groundhog.library(dplyr, groundhog_day)

# --------------------------------------------------------------------------- #
# Import demographics data ----
# --------------------------------------------------------------------------- #
dem_TET <- read.csv("./data/raw/demographics_TET.csv")

dem_race_TET <- read.csv("./data/raw/demographics_race_TET.csv")

dem_GIDI <- read.csv("./data/raw/demographics_GIDI.csv")

dem_race_GIDI <- read.csv("./data/raw/demographics_race_GIDI.csv")

# --------------------------------------------------------------------------- #
# Check for multiple rows per participant_id ---- 
# --------------------------------------------------------------------------- #
n_occur_demo <- data.frame(table(dem_TET$participant_id))
n_occur_demo[n_occur_demo$Freq > 1,] # no duplicates

n_occur_race <- data.frame(table(dem_race_TET$participant_id))
n_occur_race[n_occur_race$Freq > 1,] # lots of  duplicates

n_occur_demo <- data.frame(table(dem_GIDI$participant_id))
n_occur_demo[n_occur_demo$Freq > 1,] # no duplicates

n_occur_race <- data.frame(table(dem_race_GIDI$participant_id))
n_occur_race[n_occur_race$Freq > 1,] # lots of  duplicates

# --------------------------------------------------------------------------- #
# Create a data set that combines racial identities endorsed by same p ---- 
# --------------------------------------------------------------------------- #
dem_r2_TET <- dem_race_TET %>% 
  group_by(participant_id) %>%
  summarise(race = paste(race, collapse = ","))

dem_r2_GIDI <- dem_race_GIDI %>% 
  group_by(participant_id) %>%
  summarise(race = paste(race, collapse = ","))

# --------------------------------------------------------------------------- #
# Merge the tables ---- 
# --------------------------------------------------------------------------- #
dem_merge_TET <- merge(dem_TET, dem_r2_TET, by = 'participant_id', all = TRUE)
dem_merge_GIDI <- merge(dem_GIDI, dem_r2_GIDI, 
                        by = 'participant_id', all = TRUE)

# --------------------------------------------------------------------------- #
# Select the variables of interest ---- 
# --------------------------------------------------------------------------- #
dem_interest_TET <- dem_merge_TET %>%
  select(participant_id, country, ethnicity, race, gender, birth_year)

dem_interest_GIDI <- dem_merge_GIDI %>%
  select(participant_id, country, ethnicity, race, gender, birth_year)

# --------------------------------------------------------------------------- #
# Merge the  TET and GIDI demographics tables ---- 
# --------------------------------------------------------------------------- #
dem_both <- rbind(dem_interest_TET, dem_interest_GIDI)

# --------------------------------------------------------------------------- #
# Import bias data ----
# --------------------------------------------------------------------------- #
BB_TET <-      read.csv("./data/raw/bbsiq_TET.csv")
BB_GIDI <-     read.csv("./data/raw/bbsiq_GIDI.csv")
RR_TET <-      read.csv("./data/raw/rr_TET.csv")
RR_GIDI <-     read.csv("./data/raw/rr_GIDI.csv")

# --------------------------------------------------------------------------- #
# Include participants who completed the measures during the "preTest" ---- 
# --------------------------------------------------------------------------- #
BB_TET <- BB_TET %>% 
  filter(session == "preTest")

BB_GIDI <- BB_GIDI %>% 
  filter(session == "preTest")

RR_TET <- RR_TET %>% 
  filter(session == "preTest")

RR_GIDI <- RR_GIDI %>% 
  filter(session == "preTest")

# --------------------------------------------------------------------------- #
# Select variables of interest ---- 
# --------------------------------------------------------------------------- #
BB_TET_interest <- subset(BB_TET, select = c(participant_id, visitors_bored, 
                                             breath_suffocate,vision_illness,
                                             shop_irritating, lightheaded_faint, 
                                             smoke_house, friend_incompetent,
                                             chest_heart, jolt_burglar,
                                             party_boring,heart_wrong,
                                             confused_outofmind,urgent_died,
                                             dizzy_ill))
BB_GIDI_interest <- subset(BB_GIDI, select = c(participant_id, visitors_bored, 
                                               breath_suffocate,vision_illness,
                                               shop_irritating, lightheaded_faint, 
                                               smoke_house, friend_incompetent,
                                               chest_heart, jolt_burglar,
                                               party_boring,heart_wrong,
                                               confused_outofmind,urgent_died,
                                               dizzy_ill)) 
RR_TET_interest <- subset(RR_TET, select = c(participant_id, blood_test_ns,
                                             elevator_ns,job_ns,lunch_ns,
                                             meeting_friend_ns, noise_ns,
                                             scrape_ns,shopping_ns,
                                             wedding_ns,blood_test_ps,
                                             elevator_ps,job_ps,lunch_ps,
                                             meeting_friend_ps,noise_ps, 
                                             scrape_ps,shopping_ps,
                                             wedding_ps))
RR_GIDI_interest <- subset(RR_GIDI, select = c(participant_id, blood_test_ns,
                                               elevator_ns,job_ns,lunch_ns,
                                               meeting_friend_ns, noise_ns,
                                               scrape_ns,shopping_ns,
                                               wedding_ns,blood_test_ps,
                                               elevator_ps,job_ps,lunch_ps,
                                               meeting_friend_ps,noise_ps, 
                                               scrape_ps,shopping_ps,
                                               wedding_ps))

# --------------------------------------------------------------------------- #
# Merge the measures for TET and GIDI ---- 
# --------------------------------------------------------------------------- #
BB_merge <- rbind(BB_GIDI_interest, BB_TET_interest)

RR_merge <- rbind(RR_GIDI_interest, RR_TET_interest)

bias_merge <- merge(BB_merge, RR_merge, all = TRUE)

# --------------------------------------------------------------------------- #
# Import clean DASS/OASIS data ---- 
# --------------------------------------------------------------------------- #
DO_cleaned_both <- read.csv("./data/clean/DO_clean_both.csv")

# --------------------------------------------------------------------------- #
# Merge the DASS/OASIS clean data with the demographics data ---- 
# --------------------------------------------------------------------------- #
DO_dem_both <- merge(DO_cleaned_both, dem_both, all = TRUE) 
# 47 extra people --> these are the people that were filtered out for date, so 
# they will be filtered out again

# --------------------------------------------------------------------------- #
# Merge the DO_dem_both table with the bias_merge table ---- 
# --------------------------------------------------------------------------- #
all_merged <- merge(DO_dem_both, bias_merge, all = TRUE) 

# --------------------------------------------------------------------------- #
# Import raw anxiety triggers data (includes COVID-19 anxiety item) ---- 
# --------------------------------------------------------------------------- #
AT_TET <-  read.csv("./data/raw/anxiety_triggers_TET.csv")
AT_GIDI <- read.csv("./data/raw/anxiety_triggers_GIDI.csv")

# --------------------------------------------------------------------------- #
# Select the variables of interest and merge the tables---- 
# --------------------------------------------------------------------------- #
AT_TET_int <- AT_TET %>%
  select(participant_id, coronavirus)

AT_GIDI_int <- AT_GIDI %>%
  select(participant_id, coronavirus)

AT_both <- rbind(AT_TET_int, AT_GIDI_int)

names(AT_both)[names(AT_both)=='coronavirus'] <- "COVID_anx"

# --------------------------------------------------------------------------- #
# Merge the all_merged table with the Anxiety Triggers data ---- 
# --------------------------------------------------------------------------- #
data_to_filt <- merge(all_merged, AT_both, all = TRUE)

# --------------------------------------------------------------------------- #
# Include only participants from the United States ---- 
# --------------------------------------------------------------------------- #
data_to_filt <- data_to_filt %>% 
  filter(country == "United States")

# --------------------------------------------------------------------------- #
# Include only participants who have a valid COVID anxiety score 
# This includes participants with 555 (prefer not to answer), 999 (item not 
# present to participants), NA, or participants who completed the item before 
# the IRB gave approval ---- 
# --------------------------------------------------------------------------- #

# Check amount to remove
sum(is.na(data_to_filt$COVID_anx)) # 10
covid_five <- filter(data_to_filt, COVID_anx == 555)
nrow(covid_five) # 4
covid_nine <- filter(data_to_filt, COVID_anx == 999)
nrow(covid_nine) # 24

data_to_filt <- data_to_filt %>%
  filter(COVID_anx != 555)

data_to_filt <- data_to_filt %>%
  filter(COVID_anx != 999)

# IDs to remove: 2010, 2015, 2017, 2018, 2019,  2023, 2024, 2025
data_to_filt <- data_to_filt %>%
  filter(participant_id != 2010, participant_id != 2015, participant_id != 2017, 
         participant_id != 2018, participant_id != 2019, participant_id != 2023, 
         participant_id != 2024, participant_id != 2025)

# --------------------------------------------------------------------------- #
# Include only participants who are either: 
# Race: Black/African origin or White/European origin OR
# Ethnicity: Hispanic/Latinx or Not Hispanic/Latinx ---- 
# --------------------------------------------------------------------------- #

data_to_filt <- data_to_filt %>% 
  filter(race == "White/European Origin" | race == "Black/African origin" | 
           ethnicity == "Hispanic or Latino" | 
           ethnicity == "Not Hispanic or Latino")

# --------------------------------------------------------------------------- #
# Filter out participants who do not have DASS and OASIS mean scores ---- 
# --------------------------------------------------------------------------- #
data_to_filt <- data_to_filt %>%
  filter(participant_id != 2032, participant_id != 2065, participant_id != 2066, 
         participant_id != 2069, participant_id != 2071)

# --------------------------------------------------------------------------- #
# Rename data set and export into a csv ---- 
# --------------------------------------------------------------------------- #
final_sample <- data_to_filt
write.csv(final_sample, "./data/clean/final_sample.csv")
