# --------------------------------------------------------------------------- #
# Combine DASS and OASIS cleaned data set for TET with DASS and OASIS cleaned 
# data set for GIDI. 
# Author: Tylar N. Schmitt 

# --------------------------------------------------------------------------- #
# Set working directory to parent analyses folder ----
# --------------------------------------------------------------------------- #

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
DO_cleaned_TET <- read.csv("./data/clean/DO_clean_TET.csv")
DO_cleaned_GIDI <- read.csv("./data/clean/DO_clean_GIDI.csv")

# --------------------------------------------------------------------------- #
# Merge data ----
# --------------------------------------------------------------------------- #
DO_cleaned_both <- rbind(DO_cleaned_TET, DO_cleaned_GIDI)

# --------------------------------------------------------------------------- #
# Remove the "X" column ----
# --------------------------------------------------------------------------- #
DO_cleaned_both <- subset(DO_cleaned_both, select = -X)

# --------------------------------------------------------------------------- #
# Export the data set as a csv file ----
# --------------------------------------------------------------------------- # 
write.csv(DO_cleaned_both, "./data/clean/DO_clean_both.csv")
