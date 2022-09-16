# --------------------------------------------------------------------------- #
# Run analyses: multiple regression 
# Author: Tylar N. Schmitt and Jeremy W. Eberle

# --------------------------------------------------------------------------- #
# Set working directory to parent analyses folder--- 
# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# Check R version, load packages, and set random seed --- 
# --------------------------------------------------------------------------- #

# Load custom functions

source("./code/00_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# Load packages with groundhog

groundhog.library(dplyr, groundhog_day)
groundhog.library(psych, groundhog_day)
groundhog.library(car, groundhog_day)
groundhog.library(rockchalk, groundhog_day)
groundhog.library(r2glmm, groundhog_day)

# Set random seed

set.seed(1234)

# --------------------------------------------------------------------------- #
# Import data ----
# --------------------------------------------------------------------------- #

final_samp_clean <- read.csv("./data/clean/final_samp_clean.csv")

# --------------------------------------------------------------------------- #
# Effect code race and ethnicity ---- 
# --------------------------------------------------------------------------- #

final_samp_clean$race_code <- NA
final_samp_clean$race_code[final_samp_clean$race == "White/European origin"] <- 
  -1
final_samp_clean$race_code[final_samp_clean$race == "Black/African origin"]  <- 
  1

final_samp_clean$ethnicity_code <- NA
final_samp_clean$ethnicity_code[
  final_samp_clean$ethnicity == "Not Hispanic or Latino"] <- -1
final_samp_clean$ethnicity_code[
  final_samp_clean$ethnicity == "Hispanic or Latino"]     <- 1

table(final_samp_clean$race, useNA = "always")
table(final_samp_clean$race_code, useNA = "always")

table(final_samp_clean$ethnicity, useNA = "always")
table(final_samp_clean$ethnicity_code, useNA = "always")

# --------------------------------------------------------------------------- #
# Define function to compute semipartial r ----
# --------------------------------------------------------------------------- #

# Define function to compute semipartial R-squared estimates and convert to 
# semipartial r

compute_sp_r <- function(model) {
  sp_rsq <- r2beta(model, partial = TRUE)
  sp_rsq <- sp_rsq[sp_rsq$Effect != "Model", ]
  
  sp_r_df <- data.frame(effect          = sp_rsq$Effect,
                        sp_rsq          = sp_rsq$Rsq,
                        sp_rsq_lower.CL = sp_rsq$lower.CL,
                        sp_rsq_upper.CL = sp_rsq$upper.CL,
                        sp_r            = sqrt(sp_rsq$Rsq),
                        sp_r_lower.CL   = sqrt(sp_rsq$lower.CL),
                        sp_r_upper.CL   = sqrt(sp_rsq$upper.CL))
  
  sp_r_df <- rbind(c("(Intercept)", rep(NA, length(sp_r_df) - 1)),
                   sp_r_df)
  
  return(sp_r_df)
}

# --------------------------------------------------------------------------- #
# Define function to create table ----
# --------------------------------------------------------------------------- #

# Define function to create table

create_tbl <- function(model, confint, sp_r) {
  # Combine coefficients, CIs, and semipartial rs
  
  effect <- row.names(summary(model)$coefficients)
  coefficients <- summary(model)$coefficients
  CIs <- confint
  r_sp <- as.numeric(sp_r[, "sp_r"])
  
  tbl <- as.data.frame(cbind(effect, coefficients, CIs, r_sp))
  row.names(tbl) <- 1:nrow(tbl)
  
  # Round columns
  
  two_digit_vars   <- c("Estimate", "Std. Error", "t value", "2.5 %", "97.5 %", "r_sp")
  three_digit_vars <- "Pr(>|t|)"
  
  tbl[, two_digit_vars]   <- round(tbl[, two_digit_vars], 2)
  tbl[, three_digit_vars] <- round(tbl[, three_digit_vars], 3)
  
  # Rename columns
  
  dendf <- as.numeric(summary(model)$fstatistic["dendf"])
  
  names(tbl)[names(tbl) == "Estimate"]   <- "b"
  names(tbl)[names(tbl) == "Std. Error"] <- "SE"
  names(tbl)[names(tbl) == "t value"]    <- paste0("t(", dendf, ")")
  names(tbl)[names(tbl) == "Pr(>|t|)"]   <- "p"
  
  # Format CI
  
  tbl$CI_95 <- paste0("[", tbl$`2.5 %`, ", ", tbl$`97.5 %`, "]")
  tbl[, c("2.5 %", "97.5 %")] <- NULL
  
  tbl_lm1_mc <- tbl[, c(names(tbl)[names(tbl) != "r_sp"], "r_sp")]
  
  return(tbl)
}

# --------------------------------------------------------------------------- #
# TODO: Consider defining additional functions ----
# --------------------------------------------------------------------------- #

# TODO: Consider debugging function to run lm (mean-centering continuous predictor).
# Currently a bug when passing "lm" to "meanCenter" ("fml" not found).

# run_lm_mc <- function(data, bias_var, grp_var) {
#   fml <- paste0("COVID_anx ~ ", bias_var, " + ", 
#                                 grp_var, " + ", 
#                                 bias_var, "*", grp_var)
#   fml <- as.formula(fml)
#   
#   lm <- lm(fml, data)
#   lm_mc <- meanCenter(lm, terms = bias_var)
#   
#   return(lm_mc)
# }

# TODO: Consider defining function to run all other functions for given model.
# Currently does not return model objects.

# run_all <- function(model) {
#   print(summary(model))
#   confint <- as.data.frame(confint(model))
#   sp_r <- compute_sp_r(model = model)
#   tbl <- create_tbl(model = model, confint = confint, sp_r = sp_r)
#   write.csv(tbl, 
#             paste0("./output/tbl_", deparse(substitute(model)), ".csv"), 
#             row.names = FALSE)
# }

# --------------------------------------------------------------------------- #
# Run models and use functions to obtain result objects ----
# --------------------------------------------------------------------------- #

# Run models

lm_1 <- lm(COVID_anx ~ BB_NIBscore + race_code + BB_NIBscore*race_code, 
           final_samp_clean)
lm_2 <- lm(COVID_anx ~ RR_NIBscore + race_code + RR_NIBscore*race_code, 
           final_samp_clean)
lm_3 <- lm(COVID_anx ~ PIBscore    + race_code + PIBscore*race_code, 
           final_samp_clean)
lm_4 <- lm(COVID_anx ~ BB_NIBscore + ethnicity_code + BB_NIBscore*ethnicity_code, 
           final_samp_clean)
lm_5 <- lm(COVID_anx ~ RR_NIBscore + ethnicity_code + RR_NIBscore*ethnicity_code, 
           final_samp_clean)
lm_6 <- lm(COVID_anx ~ PIBscore    + ethnicity_code + PIBscore*ethnicity_code, 
           final_samp_clean)


# TODO: Rerun models, mean-centering continuous predictor
lm_mc_1 <- meanCenter(lm_1, terms = "BB_NIBscore")
lm_mc_2 <- meanCenter(lm_2, terms = "RR_NIBscore")
lm_mc_3 <- meanCenter(lm_3, terms = "PIBscore")
lm_mc_4 <- meanCenter(lm_4, terms = "BB_NIBscore")
lm_mc_5 <- meanCenter(lm_5, terms = "RR_NIBscore")
lm_mc_6 <- meanCenter(lm_6, terms = "PIBscore")

# TODO: Obtain summaries

summary_lm_mc_1 <- summary(lm_mc_1)
summary_lm_mc_2 <- summary(lm_mc_2)
summary_lm_mc_3 <- summary(lm_mc_3)
summary_lm_mc_4 <- summary(lm_mc_4)
summary_lm_mc_5 <- summary(lm_mc_5)
summary_lm_mc_6 <- summary(lm_mc_6)

# TODO: Compute CIs

confint_lm_mc_1 <- as.data.frame(confint(lm_mc_1))
confint_lm_mc_2 <- as.data.frame(confint(lm_mc_2))
confint_lm_mc_3 <- as.data.frame(confint(lm_mc_3))
confint_lm_mc_4 <- as.data.frame(confint(lm_mc_4))
confint_lm_mc_5 <- as.data.frame(confint(lm_mc_5))
confint_lm_mc_6 <- as.data.frame(confint(lm_mc_6))

# TODO: Compute semipartial rs

sp_r_lm_mc_1 <- compute_sp_r(lm_mc_1)
sp_r_lm_mc_2 <- compute_sp_r(lm_mc_2)
sp_r_lm_mc_3 <- compute_sp_r(lm_mc_3)
sp_r_lm_mc_4 <- compute_sp_r(lm_mc_4)
sp_r_lm_mc_5 <- compute_sp_r(lm_mc_5)
sp_r_lm_mc_6 <- compute_sp_r(lm_mc_6)

# TODO: Create tables

tbl_lm_mc_1 <- create_tbl(lm_mc_1, confint_lm_mc_1, sp_r_lm_mc_1)
tbl_lm_mc_2 <- create_tbl(lm_mc_2, confint_lm_mc_2, sp_r_lm_mc_2)
tbl_lm_mc_3 <- create_tbl(lm_mc_3, confint_lm_mc_3, sp_r_lm_mc_3)
tbl_lm_mc_4 <- create_tbl(lm_mc_4, confint_lm_mc_4, sp_r_lm_mc_4)
tbl_lm_mc_5 <- create_tbl(lm_mc_5, confint_lm_mc_5, sp_r_lm_mc_5)
tbl_lm_mc_6 <- create_tbl(lm_mc_6, confint_lm_mc_6, sp_r_lm_mc_6)

# TODO: Export tables

write.csv(tbl_lm_mc_1, "./output/tbl_lm_mc_1.csv", row.names = FALSE)
write.csv(tbl_lm_mc_2, "./output/tbl_lm_mc_2.csv", row.names = FALSE)
write.csv(tbl_lm_mc_3, "./output/tbl_lm_mc_3.csv", row.names = FALSE)
write.csv(tbl_lm_mc_4, "./output/tbl_lm_mc_4.csv", row.names = FALSE)
write.csv(tbl_lm_mc_5, "./output/tbl_lm_mc_5.csv", row.names = FALSE)
write.csv(tbl_lm_mc_6, "./output/tbl_lm_mc_6.csv", row.names = FALSE)

# --------------------------------------------------------------------------- #
# Save data and results ----
# --------------------------------------------------------------------------- #

# Save data

final_samp_clean2 <- final_samp_clean

save(final_samp_clean2, file = "./data/clean/final_samp_clean2.RData")

# Save all result objects in list

n_models <- 6

model_object_names <- c(paste0("lm_mc_", 1:n_models),
                        paste0("confint_lm_mc_", 1:n_models),
                        paste0("sp_r_lm_mc_", 1:n_models),
                        paste0("summary_lm_mc_", 1:n_models),
                        paste0("tbl_lm_mc_", 1:n_models))

res <- lapply(model_object_names, function(x) get(x))
names(res) <- model_object_names

save(res, file = "./output/res.RData")