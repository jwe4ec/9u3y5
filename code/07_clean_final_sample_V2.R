# --------------------------------------------------------------------------- #
# Clean the merged data  for analysis (include demographics, Anxiety Triggers, 
# BBSIQ, and RR data)
# Author: Tylar N. Schmitt 

# --------------------------------------------------------------------------- #
# Set working directory to parent analyses folder ---- 
# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# Check R version and load packages --- 
# --------------------------------------------------------------------------- #

# Load custom functions
source("./code/00_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day
groundhog_day <- version_control()

# Load packages with groundhog
groundhog.library(dplyr, groundhog_day)

# --------------------------------------------------------------------------- #
# Import clean data ---- 
# --------------------------------------------------------------------------- #
final_sample <- read.csv("./data/clean/final_sample.csv")

# --------------------------------------------------------------------------- #
# Create a new data set with the variables of interest ----
# --------------------------------------------------------------------------- #
final_samp_clean <- final_sample %>% 
  select(participant_id, COVID_anx, race, ethnicity, BB_NIBscore, RR_NIBscore, 
         PIBscore)

# --------------------------------------------------------------------------- #
# Export the data set as a csv file --- 
# --------------------------------------------------------------------------- #
write.csv(final_samp_clean, "./data/clean/final_samp_clean.csv")
