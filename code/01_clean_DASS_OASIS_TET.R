# --------------------------------------------------------------------------- #
# Clean DASS and OASIS data sets for TET participants. 
# Author: Tylar N. Schmitt 

# --------------------------------------------------------------------------- #
# Set working directory ---- 
# --------------------------------------------------------------------------- #

# Set your working directory to a parent analyses folder that contains
# sub-folders "code" and "data" (and "data" can have sub-folders for "clean 
# and "raw" data)

# --------------------------------------------------------------------------- #
# Check R version and load packages ---- 
# --------------------------------------------------------------------------- #

# Load custom functions
source("./code/00_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day
groundhog_day <- version_control()

# Load packages with groundhog
groundhog.library(dplyr, groundhog_day)

# --------------------------------------------------------------------------- #
# Import data ---- 
# --------------------------------------------------------------------------- #
DASS_TET <- read.csv("./data/raw/dass21_as_TET.csv")
OA_TET <- read.csv("./data/raw/oa_TET.csv")

# --------------------------------------------------------------------------- #
# Check if any NAs exist in the "session" column ---- 
# --------------------------------------------------------------------------- #
table(DASS_TET$session, useNA = "always") # None
table(OA_TET$session, useNA = "always") # None

# Note: We are including participants that completed the DASS and the OASIS 
# during the "eligible" session only. For the OASIS, we are including people 
# that completed the OASIS during the eligible phase and during the pre-test, 
# since the OASIS was not added as an eligibility screener until 5/11/20 and 
# that decision was made to expand eligibility. 
# We are including participants who completed it from 4/30/20 onward, as that 
# was the date that the COVID-19 question was added to the Anxiety Triggers 
# measure.

# --------------------------------------------------------------------------- #
# Convert "date" variable to preferred date format ----
# Source: 
# https://stackoverflow.com/questions/28335715/r-how-to-filter-subset-a-sequence-of-dates
# --------------------------------------------------------------------------- #
DASS_TET$date <- as.Date(DASS_TET$date, format = "%Y-%m-%d")
OA_TET$date <- as.Date(OA_TET$date, format = "%Y-%m-%d")

# --------------------------------------------------------------------------- #
# Filter data ----
# --------------------------------------------------------------------------- #
DASS_eligible_TET <- DASS_TET %>% 
  filter(session == "ELIGIBLE" & date >= "2020-04-30")

OA_eligible_TET <- OA_TET %>% 
  filter(session == "ELIGIBLE" | session == "preTest" & date >= "2020-04-30")

# --------------------------------------------------------------------------- #
# Identify duplicates ---- 
# --------------------------------------------------------------------------- #
DASS_dups_TET <- data.frame(table(DASS_eligible_TET$participant_id))
OA_dups_TET <- data.frame(table(OA_eligible_TET$participant_id))

# --------------------------------------------------------------------------- #
# Create a column that indicates if Freq > 2 ---- 
# --------------------------------------------------------------------------- #
DASS_dups_TET <- DASS_dups_TET %>% 
  mutate(more_than_2 = Freq > 2)
OA_dups_TET <- OA_dups_TET %>% 
  mutate(more_than_2 = Freq > 2)

# --------------------------------------------------------------------------- #
# Count participants whose Freq > 2 ---- 
# --------------------------------------------------------------------------- #
count(DASS_dups_TET, more_than_2) # number is 5
count(OA_dups_TET, more_than_2) # number is 8 

# --------------------------------------------------------------------------- #
# Change "Var1" to "participant_id" and "participant_id" to an integer ---- 
# --------------------------------------------------------------------------- #
names(DASS_dups_TET)[names(DASS_dups_TET)=="Var1"] <- "participant_id"
DASS_dups_TET$participant_id <- as.integer(
  as.character(DASS_dups_TET$participant_id))

names(OA_dups_TET)[names(OA_dups_TET)=="Var1"] <- "participant_id"
OA_dups_TET$participant_id <- as.integer(
  as.character(OA_dups_TET$participant_id))

# --------------------------------------------------------------------------- #
# Merge the eligible table with the duplicates table ----
# --------------------------------------------------------------------------- #
DASS_join_TET <- merge(DASS_eligible_TET, DASS_dups_TET, 
                         by = "participant_id", all = TRUE)
OA_join_TET <- merge(OA_eligible_TET, OA_dups_TET, 
                         by = "participant_id", all = TRUE)

# --------------------------------------------------------------------------- #
# Create a data set with the unique values ----- 
# --------------------------------------------------------------------------- #
DASS_unq_TET <- DASS_join_TET %>% 
  distinct(participant_id, session, 
           bre, dry, hea, pan, sca, tre, wor, keep.all = TRUE )

OA_unq_TET <- OA_join_TET %>% 
  distinct(participant_id, session, axf, axs, avo, wrk, soc, keep.all = TRUE)

# --------------------------------------------------------------------------- #
# Identify duplicates ---- 
# --------------------------------------------------------------------------- # 
DASS_unq_dups_TET <- data.frame(table(DASS_unq_TET$participant_id))
OA_unq_dups_TET <- data.frame(table(OA_unq_TET$participant_id))

# --------------------------------------------------------------------------- #
# Create a column that indicates if Freq > 2 ---- 
# --------------------------------------------------------------------------- #
DASS_unq_dups_TET <- DASS_unq_dups_TET %>% 
  mutate(more_than_2 = Freq > 2)
OA_unq_dups_TET <- OA_unq_dups_TET %>% 
  mutate(more_than_2 = Freq > 2)

# --------------------------------------------------------------------------- #
# Count participants whose Freq > 2 ---- 
# --------------------------------------------------------------------------- #
count(DASS_unq_dups_TET, more_than_2) # 0 
count(OA_unq_dups_TET, more_than_2) # 3 

# --------------------------------------------------------------------------- #
# Change "Var1" to "participant_id"and "participant_id" to an integer ---- 
# --------------------------------------------------------------------------- #
names(DASS_unq_dups_TET)[names(DASS_unq_dups_TET)=="Var1"] <- "participant_id"
DASS_unq_dups_TET$participant_id <- as.integer(
  as.character(DASS_unq_dups_TET$participant_id))

names(OA_unq_dups_TET)[names(OA_unq_dups_TET)=="Var1"] <- "participant_id"
OA_unq_dups_TET$participant_id <- as.integer(
  as.character(OA_unq_dups_TET$participant_id))

# --------------------------------------------------------------------------- #
# Merge the unique duplicates table with the unique values table --- 
# --------------------------------------------------------------------------- #
DASS_join2_TET <- merge(DASS_unq_TET, DASS_unq_dups_TET, 
                            by = "participant_id", all = TRUE)
OA_join2_TET <- merge(OA_unq_TET, OA_unq_dups_TET, 
                          by = "participant_id", all = TRUE)

# --------------------------------------------------------------------------- #
# Exclude participants that have "TRUE" in more_than_2 column ---- 
# --------------------------------------------------------------------------- #
DASS_excluded_TET <- DASS_join2_TET %>% 
  filter( more_than_2 == "FALSE")
OA_excluded_TET <- OA_join2_TET %>% 
  filter( more_than_2 == "FALSE")

# --------------------------------------------------------------------------- #
# Keep track of excluded IDs ---- 
# --------------------------------------------------------------------------- #
DA_excluded_IDS <- DASS_join2_TET %>%
  filter( more_than_2 == "TRUE") # 0 people
OA_exlcuded_IDS <- OA_join2_TET %>% 
  filter( more_than_2 == "TRUE") # 3 people

# --------------------------------------------------------------------------- #
# Create data set with participants that have 2 unique entries ---- 
# --------------------------------------------------------------------------- #
DASS_2_TET <- DASS_excluded_TET %>% 
  filter(Freq == 2)
OA_2_TET <- OA_excluded_TET %>% 
  filter(Freq == 2)

# --------------------------------------------------------------------------- #
# Code the 555 values as NA ---- 
# --------------------------------------------------------------------------- #
DASS_2_TET[DASS_2_TET==555]<-NA
OA_2_TET[OA_2_TET==555]<-NA

# --------------------------------------------------------------------------- #
# Check how many NAs exist in the data set ---- 
# --------------------------------------------------------------------------- # 
sum(is.na(DASS_2_TET)) # 11 
sum(is.na(OA_2_TET)) # 0 

# --------------------------------------------------------------------------- #
# Calculate the means for the 2 unique values for each participant ----
# Source: https://www.guru99.com/r-aggregate-function.html 
# --------------------------------------------------------------------------- #
DASS_2_TET_means <- DASS_2_TET %>% 
  group_by(participant_id) %>% 
  summarise(bre = mean(bre, na.rm = TRUE), dry = mean(dry, na.rm = TRUE), 
            hea = mean(hea, na.rm = TRUE), pan = mean(pan, na.rm = TRUE), 
            sca = mean(sca, na.rm = TRUE), tre = mean(tre, na.rm = TRUE), 
            wor = mean(wor, na.rm = TRUE))
OA_2_TET_means <- OA_2_TET %>% 
  group_by(participant_id) %>% 
  summarise(axf = mean(axf, na.rm = TRUE), axs = mean(axs, na.rm = TRUE), 
            avo = mean(avo, na.rm = TRUE), wrk = mean(wrk, na.rm = TRUE), 
            soc = mean(soc, na.rm = TRUE))

# --------------------------------------------------------------------------- #
# Calculate the means for the overall mean of the scores ----
# Source: https://stackoverflow.com/questions/10945703/calculate-row-means-on-subset-of-columns 
# --------------------------------------------------------------------------- #
DASS_2_TET_means <- DASS_2_TET_means %>% 
  mutate(mean = rowMeans(DASS_2_TET_means[,c("bre", "dry", "hea",
                                                     "pan", "sca", "tre",
                                                     "wor")], na.rm = TRUE))
OA_2_TET_means <- OA_2_TET_means %>% 
  mutate(mean = rowMeans(OA_2_TET_means[,c("axf", "axs", "avo",
                                                   "wrk", "soc")], na.rm = TRUE))
# --------------------------------------------------------------------------- #
# Create a data set with the participants with 1 unique value ----
# --------------------------------------------------------------------------- #
DASS_1_TET <- DASS_excluded_TET %>% 
  filter(Freq == 1) %>% 
  select(participant_id, bre, dry, hea, pan, sca, tre, wor)

OA_1_TET <- OA_excluded_TET %>% 
  filter(Freq == 1) %>% 
  select(participant_id, axf, axs, avo, wrk, soc)

# --------------------------------------------------------------------------- #
# Code 555 values as NA ----
# --------------------------------------------------------------------------- # 
DASS_1_TET[DASS_1_TET==555]<-NA
OA_1_TET[OA_1_TET==555]<-NA

# --------------------------------------------------------------------------- #
# Count amount of NAs ----
# --------------------------------------------------------------------------- #
sum(is.na(DASS_1_TET)) # 22
sum(is.na(OA_1_TET)) # 8

# --------------------------------------------------------------------------- #
# Calculate the overall mean of the scores ----
# --------------------------------------------------------------------------- #
DASS_1_TET <- DASS_1_TET %>% 
  mutate(mean = rowMeans(DASS_1_TET[,c("bre", "dry", "hea",
                                        "pan", "sca", "tre",
                                        "wor")], na.rm = TRUE))
OA_1_TET <- OA_1_TET %>% 
  mutate(mean = rowMeans(OA_1_TET[,c("axf", "axs", "avo",
                                      "wrk", "soc")], na.rm = TRUE))

# --------------------------------------------------------------------------- #
# Combine the freq == 2 data set with the freq == 1 data set ----
# --------------------------------------------------------------------------- #
DASS_cleaned_TET <- rbind(DASS_1_TET, DASS_2_TET_means)
OA_cleaned_TET <- rbind(OA_1_TET,OA_2_TET_means)

# --------------------------------------------------------------------------- #
# Merge the cleaned data sets ----
# --------------------------------------------------------------------------- #
DO_cleaned_TET <- merge(DASS_cleaned_TET, OA_cleaned_TET, 
                            by = "participant_id", all = TRUE)

# --------------------------------------------------------------------------- #
# Rename mean columns to mean for DASS or mean for OASIS ----
# --------------------------------------------------------------------------- #
names(DO_cleaned_TET)[names(DO_cleaned_TET)=="mean.x"] <- 'mean_DASS'
names(DO_cleaned_TET)[names(DO_cleaned_TET)=="mean.y"] <- 'mean_OASIS'

# --------------------------------------------------------------------------- #
# Export cleaned data set into "clean" folder within "data" folder ----
# --------------------------------------------------------------------------- #

dir.create("./data/clean")

write.csv(DO_cleaned_TET, "./data/clean/DO_clean_TET.csv")
