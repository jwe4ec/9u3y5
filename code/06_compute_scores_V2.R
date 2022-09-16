# --------------------------------------------------------------------------- #
# Compute scores for the Anxiety Triggers, BBSIQ, and RR questionnaires
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
# Important final_sample data set --- 
# --------------------------------------------------------------------------- #
final_sample <- read.csv("./data/clean/final_sample.csv")

# --------------------------------------------------------------------------- #
# Check the range and compute the mean scores for each measure ----
# Note: Since Anxiety Triggers is just one item, no need to compute a score
# Questionnaire ranges: 
# 1. COVID_anx = 1-5 
# 2. BBSIQ = 0-4
# 3. RR = 1-4
# --------------------------------------------------------------------------- #

# Note: Since Anxiety Triggers is just one item, no need to compute a score

final_sample[final_sample==555] <- NA

# Anxiety Triggers 
sum(is.na(final_sample$COVID_anx)) # 0 (which is expected as NAs were removed)
# The range for this questionnaire should be 1-5
range(final_sample$COVID_anx, na.rm = TRUE)

# BBSIQ 
sum(is.na(final_sample$breath_suffocate)) # 99
sum(is.na(final_sample$chest_heart)) # 99
sum(is.na(final_sample$confused_outofmind)) # 99
sum(is.na(final_sample$dizzy_ill)) # 99
sum(is.na(final_sample$friend_incompetent)) # 100
sum(is.na(final_sample$heart_wrong)) # 100
sum(is.na(final_sample$jolt_burglar)) # 99
sum(is.na(final_sample$lightheaded_faint)) # 100
sum(is.na(final_sample$party_boring)) # 101
sum(is.na(final_sample$shop_irritating)) # 100
sum(is.na(final_sample$smoke_house)) # 99
sum(is.na(final_sample$urgent_died)) # 100
sum(is.na(final_sample$vision_illness)) # 100
sum(is.na(final_sample$visitors_bored)) # 100

range(final_sample$breath_suffocate, na.rm = TRUE)
range(final_sample$chest_heart, na.rm = TRUE)
range(final_sample$confused_outofmind, na.rm = TRUE)
range(final_sample$dizzy_ill, na.rm = TRUE)
range(final_sample$friend_incompetent, na.rm = TRUE)
range(final_sample$heart_wrong, na.rm = TRUE)
range(final_sample$jolt_burglar, na.rm = TRUE)
range(final_sample$lightheaded_faint, na.rm = TRUE)
range(final_sample$party_boring, na.rm = TRUE)
range(final_sample$shop_irritating, na.rm = TRUE)
range(final_sample$smoke_house, na.rm = TRUE)
range(final_sample$urgent_died, na.rm = TRUE)
range(final_sample$vision_illness, na.rm = TRUE)
range(final_sample$visitors_bored, na.rm = TRUE)

# Calculate the mean score of the BBSIQ items 
final_sample <- final_sample %>%
  mutate(BB_NIBscore = rowMeans(
    final_sample[,c("visitors_bored", "breath_suffocate", "vision_illness",
                        "shop_irritating", "lightheaded_faint", "smoke_house",
                        "friend_incompetent", "chest_heart", "jolt_burglar",
                        "party_boring","heart_wrong","confused_outofmind",
                        "urgent_died","dizzy_ill")], 
    na.rm = TRUE))
sum(is.na(final_sample$BB_NIBscore)) # 99

# Note: for rowMean, value is left as NA if all of the values are NA: 
# https://www.rdocumentation.org/packages/fame/versions/1.03/topics/rowMeans

# RR
sum(is.na(final_sample$blood_test_ps)) # 77
sum(is.na(final_sample$elevator_ps)) # 72
sum(is.na(final_sample$job_ps))  # 72
sum(is.na(final_sample$lunch_ps)) # 74
sum(is.na(final_sample$meeting_friend_ps)) # 71
sum(is.na(final_sample$noise_ps)) # 76
sum(is.na(final_sample$scrape_ps)) # 72
sum(is.na(final_sample$shopping_ps)) # 77
sum(is.na(final_sample$wedding_ps)) # 76

sum(is.na(final_sample$blood_test_ns)) # 74
sum(is.na(final_sample$elevator_ns))  # 70
sum(is.na(final_sample$job_ns)) # 69
sum(is.na(final_sample$lunch_ns)) # 75
sum(is.na(final_sample$meeting_friend_ns)) # 72
sum(is.na(final_sample$noise_ns)) # 72
sum(is.na(final_sample$scrape_ns)) # 73
sum(is.na(final_sample$shopping_ns)) # 75
sum(is.na(final_sample$wedding_ns)) # 73

range(final_sample$blood_test_ns, na.rm = TRUE)
range(final_sample$blood_test_ps, na.rm = TRUE)
range(final_sample$elevator_ns, na.rm = TRUE)
range(final_sample$elevator_ps, na.rm = TRUE)
range(final_sample$job_ns, na.rm = TRUE)
range(final_sample$job_ps, na.rm = TRUE)
range(final_sample$lunch_ns, na.rm = TRUE)
range(final_sample$lunch_ps, na.rm = TRUE)
range(final_sample$meeting_friend_ns, na.rm = TRUE)
range(final_sample$meeting_friend_ps, na.rm = TRUE)
range(final_sample$noise_ns, na.rm = TRUE)
range(final_sample$noise_ps, na.rm = TRUE)
range(final_sample$scrape_ns, na.rm = TRUE)
range(final_sample$scrape_ps, na.rm = TRUE)
range(final_sample$shopping_ns, na.rm = TRUE)
range(final_sample$shopping_ps, na.rm = TRUE)
range(final_sample$wedding_ns, na.rm = TRUE)
range(final_sample$wedding_ps, na.rm = TRUE)

# Calculate the mean score of the RR items 
final_sample <- final_sample %>%
  mutate(RR_NIBscore = rowMeans(final_sample[,c("blood_test_ns", 
                                                    "elevator_ns", "job_ns", 
                                                    "lunch_ns", 
                                                    "meeting_friend_ns", 
                                                    "noise_ns", "scrape_ns", 
                                                    "shopping_ns",
                                                    "wedding_ns")], na.rm = TRUE))%>%
  mutate(PIBscore = rowMeans(final_sample[,c("blood_test_ps", "elevator_ps",
                                                 "job_ps", "lunch_ps", 
                                                 "meeting_friend_ps",
                                                 "noise_ps", "scrape_ps", "shopping_ps", 
                                                 "wedding_ps")], na.rm = TRUE))
sum(is.na(final_sample$RR_NIBscore)) # 67
sum(is.na(final_sample$PIBscore)) # 67

# --------------------------------------------------------------------------- #
# Export clean data into a csv file ----
# --------------------------------------------------------------------------- #
write.csv(final_sample, "./data/clean/final_sample.csv")

