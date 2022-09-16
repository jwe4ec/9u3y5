# --------------------------------------------------------------------------- #
# Describe demographics for TET and GIDI
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
final_sample <- read.csv("./data/clean/final_sample.csv")

# --------------------------------------------------------------------------- #
# Find frequencies for age, gender, race, and ethnicity --- 
# --------------------------------------------------------------------------- #
dem_age <- filter(final_sample, birth_year != 555) 
dem_age$age <- 2021 - dem_age$birth_year 
mean(dem_age$age, na.rm = T)
sd(dem_age$age, na.rm = T) 
min(dem_age$age, na.rm = T)
max(dem_age$age, na.rm = T)

count(final_sample, gender)

count(final_sample, ethnicity) 

race_AIAN <- filter(final_sample, race == "American Indian/Alaska Native")
nrow(race_AIAN)
race_EA <- filter(final_sample, race == "East Asian") 
nrow(race_EA)
race_SA <- filter(final_sample, race == "South Asian")
nrow(race_SA)
race_NHPI <- filter(final_sample, race == "Native Hawaiian/Pacific Islander")
nrow(race_NHPI)
race_B <- filter(final_sample, race == "Black/African origin")
nrow(race_B)
race_W <- filter(final_sample, race == "White/European origin")
nrow(race_W)
race_OU <- filter(final_sample, race == "Other or Unknown")
nrow(race_OU)
race_PNA <- filter(final_sample, race == "555")
nrow(race_PNA)

race <- count(final_sample, race)
race_more <- filter(race, grepl(",", race));sum(race_more$n) 


