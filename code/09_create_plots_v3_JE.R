# --------------------------------------------------------------------------- #
# Create Figures
# Author: Tylar N. Schmitt and Jeremy W. Eberle

# --------------------------------------------------------------------------- #
# Set working directory to parent analyses folder--- 
# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# Check R version and load packages --- 
# --------------------------------------------------------------------------- #

# Load custom functions

source("./code/00_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# Load packages with groundhog

pkgs <- c("effects", "ggplot2")
groundhog.library(pkgs, groundhog_day)

# --------------------------------------------------------------------------- #
# Import data and results ----
# --------------------------------------------------------------------------- #

load(file = "./data/clean/clean_merge2.RData")
clean_merge <- clean_merge2

load(file = "./output/res.RData")

# --------------------------------------------------------------------------- #
# Prepare effects objects ----
# --------------------------------------------------------------------------- #

# Compute mean and five-number summary for mean-centered bias variable

summary <- summary(res$lm_mc_1$model$BB_NIBscorec)

# Compute effects objects using summary

eff_lm1_bias <- Effect(focal.predictors = "BB_NIBscorec", mod = res$lm_mc_1,
                       xlevels = list(BB_NIBscorec = summary))
eff_lm1_grp <- Effect(focal.predictors = "race_code", mod = res$lm_mc_1)
eff_lm1_inter <- Effect(focal.predictors = c("BB_NIBscorec", "race_code"), mod = res$lm_mc_1,
                        xlevels = list(BB_NIBscorec = summary))

eff_lm1_bias  <- as.data.frame(eff_lm1_bias)
eff_lm1_grp   <- as.data.frame(eff_lm1_grp)
eff_lm1_inter <- as.data.frame(eff_lm1_inter)

# Restrict effects objects involving categorical predictors to actual codings
# for those categorical predictors

eff_lm1_grp <- eff_lm1_grp[eff_lm1_grp$race_code %in% c(-1, 1), ]
eff_lm1_inter <- eff_lm1_inter[eff_lm1_inter$race_code %in% c(-1, 1), ]

# Convert continuous predictors to factors

eff_lm1_grp$race_code <- factor(eff_lm1_grp$race_code,
                                levels = c(-1, 1),
                                labels = c("White/European origin", "Black/African origin"))
eff_lm1_inter$race_code <- factor(eff_lm1_inter$race_code,
                                  levels = c(-1, 1),
                                  labels = c("White/European origin", "Black/African origin"))

# --------------------------------------------------------------------------- #
# Create main effect plot for bias ----
# --------------------------------------------------------------------------- #

plot_eff_lm1_bias <- ggplot(data = eff_lm1_bias,
                            aes(x = BB_NIBscorec, y = fit)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = fit - se, ymax = fit + se),
                width = .3) +
  labs(title = "Main Effect of Neg. Interp. Bias on COVID-19 Anxiety",
       x = "Mean-Centered Neg. Interp. Bias (BBSIQ)",
       y = "COVID-19 Anxiety") +
  scale_y_continuous(breaks = 1:5,
                     limits = c(1, 5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

# --------------------------------------------------------------------------- #
# Create main effect plot for race ----
# --------------------------------------------------------------------------- #

plot_eff_lm1_race <- ggplot(data = eff_lm1_grp,
                            aes(x = race_code, y = fit, group = 1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = fit - se, ymax = fit + se),
                width = .3) +
  labs(title = "Main Effect of Race on COVID-19 Anxiety",
       x = "Race",
       y = "COVID-19 Anxiety") +
  scale_y_continuous(breaks = 1:5,
                     limits = c(1, 5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

# --------------------------------------------------------------------------- #
# Create interaction plot ----
# --------------------------------------------------------------------------- #

plot_eff_lm1_inter <- ggplot(data = eff_lm1_inter,
                             aes(x = BB_NIBscorec, y = fit, 
                                 group = race_code,
                                 color = race_code,
                                 linetype = race_code)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = fit - se, ymax = fit + se),
                width = .3) +
  labs(title = "Neg. Bias-By-Race Interaction Effect on COVID Anxiety",
       x = "Mean-Centered Neg. Interp. Bias (BBSIQ)",
       y = "COVID-19 Anxiety") +
  scale_linetype_manual(name = "Race",
                        values = c("White/European origin" = "longdash",
                                   "Black/African origin" = "solid")) +
  scale_color_manual(name = "Race",
                     values = c("White/European origin" = "#1b9e77", 
                                "Black/African origin" = "#7570b3")) +
  scale_y_continuous(breaks = 1:5,
                     limits = c(1, 5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.key.width = unit(2, "cm")) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(legend.position = c(0.7, 0.2))

# --------------------------------------------------------------------------- #
# Save plots ----
# --------------------------------------------------------------------------- #

dir.create("./output/plots")

ggsave("./output/plots/plot_eff_lm1_bias.png",
       plot = plot_eff_lm1_bias,
       width = 5, height = 5)
ggsave("./output/plots/plot_eff_lm1_race.png",
       plot = plot_eff_lm1_race,
       width = 5, height = 5)
ggsave("./output/plots/plot_eff_lm1_inter.png",
       plot = plot_eff_lm1_inter,
       width = 5, height = 5)