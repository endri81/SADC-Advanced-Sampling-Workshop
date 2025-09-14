# Day 4: Complex Survey Designs - Exercises
# SADC Survey Sampling Workshop
#
# This script contains the solutions for all exercises from Day 4.
# It is designed to be run from top to bottom in RStudio.

# -----------------------------------------------------------------------------
# 1. SETUP: LOAD LIBRARIES AND DATA
# -----------------------------------------------------------------------------

# Load necessary libraries
library(tidyverse)
library(survey)

# Load the datasets
# Make sure the data files are in a subfolder named "02-Data"
# relative to your current working directory.
multiphase_data <- read.csv("02-Data/multiphase_data.csv")
rotating_panel_data <- read.csv("02-Data/rotating_panel_data.csv")
household_data <- read.csv("02-Data/complex_household_data.csv")
domain_data <- read.csv("02-Data/domain_estimation_data.csv")
panel_data <- read.csv("02-Data/longitudinal_panel_data.csv")


# -----------------------------------------------------------------------------
# EXERCISE 1: Multi-Phase Sampling
# -----------------------------------------------------------------------------
# Task: Design a two-phase survey where:
# - Phase 1: Screen 5000 people for diabetes risk
# - Phase 2: Detailed health assessment for high-risk individuals
# - High risk = 30% of population, sample 80% of high risk, 20% of low risk

# Simulate phase 1 data
set.seed(2024)
n_phase1 <- 5000

phase1_ex <- data.frame(
  id = 1:n_phase1,
  age = sample(18:80, n_phase1, replace = TRUE),
  bmi = rnorm(n_phase1, 27, 5),
  family_history = rbinom(n_phase1, 1, 0.3)
)

# Determine risk status (simplified)
phase1_ex$high_risk <- with(phase1_ex,
                            (age > 45) + (bmi > 30) + family_history >= 2)

# Phase 2 selection probabilities
phase1_ex$phase2_prob <- ifelse(phase1_ex$high_risk, 0.8, 0.2)
phase1_ex$selected_phase2 <- rbinom(n_phase1, 1, phase1_ex$phase2_prob)

# Calculate weights
phase1_ex$phase1_weight <- 1 # Assumes SRS from a much larger population
phase1_ex$phase2_weight <- 1 / phase1_ex$phase2_prob
phase1_ex$final_weight <- phase1_ex$phase1_weight * phase1_ex$phase2_weight

# Print summary of results
cat("--- Exercise 1 Results ---\n")
print(paste("Phase 1 sample size:", nrow(phase1_ex)))
print(paste("High risk prevalence in Phase 1:", round(mean(phase1_ex$high_risk), 4)))
print(paste("Phase 2 sample size:", sum(phase1_ex$selected_phase2)))
print(paste("Phase 2 high risk count:", sum(phase1_ex$selected_phase2 & phase1_ex$high_risk)))
cat("\n")


# -----------------------------------------------------------------------------
# EXERCISE 2: Rotating Panel Design
# -----------------------------------------------------------------------------
# Task: Calculate the overlap between waves in the rotating panel
# and estimate income change between consecutive waves.

# Function to calculate overlap and change
calculate_wave_change <- function(data, wave1, wave2) {
  hh_wave1 <- unique(data$household_id[data$wave == wave1])
  hh_wave2 <- unique(data$household_id[data$wave == wave2])
  
  common_hh <- intersect(hh_wave1, hh_wave2)
  overlap_pct <- length(common_hh) / length(hh_wave1) * 100
  
  matched_data <- data %>%
    filter(household_id %in% common_hh, wave %in% c(wave1, wave2)) %>%
    select(household_id, wave, income) %>%
    pivot_wider(names_from = wave, values_from = income, names_prefix = "wave_")
  
  col1 <- paste0("wave_", wave1)
  col2 <- paste0("wave_", wave2)
  matched_data$change <- matched_data[[col2]] - matched_data[[col1]]
  
  return(list(
    overlap_pct = overlap_pct,
    n_matched = nrow(matched_data),
    mean_change = mean(matched_data$change, na.rm = TRUE),
    se_change = sd(matched_data$change, na.rm = TRUE) / sqrt(nrow(matched_data))
  ))
}

# Calculate for waves 5 to 6
result_ex2 <- calculate_wave_change(rotating_panel_data, 5, 6)

cat("--- Exercise 2 Results ---\n")
print(result_ex2)
cat("\n")


# -----------------------------------------------------------------------------
# EXERCISE 3: Complex Design with Stratification and Clustering
# -----------------------------------------------------------------------------
# Task: Create a stratified cluster design and compare estimates
# with and without accounting for the design.

# Create complex survey design object
complex_ex <- svydesign(
  ids = ~psu_id,
  strata = ~stratum,
  weights = ~final_weight,
  data = household_data,
  nest = TRUE
)

# Design-based estimate
design_mean <- svymean(~income, complex_ex)

# Naive estimate (ignoring design)
naive_mean <- mean(household_data$income)
naive_se <- sd(household_data$income) / sqrt(nrow(household_data))

# Design effect
deff_ex <- SE(design_mean)^2 / naive_se^2

# Print summary of results
cat("--- Exercise 3 Results ---\n")
cat("Design-based estimate:\n")
print(design_mean)
cat("\nNaive estimate (ignoring design):\n")
print(paste("Mean:", round(naive_mean, 0), "SE:", round(naive_se, 0)))
cat("\n")
print(paste("Design Effect (DEFF):", round(deff_ex, 2)))
cat("\n")


# -----------------------------------------------------------------------------
# EXERCISE 4: Domain Estimation
# -----------------------------------------------------------------------------
# Task: Calculate estimates for small domains (districts) and identify
# which domains have unreliable estimates (CV > 30%).

# Create survey design object
domain_design_ex <- svydesign(
  ids = ~psu,
  strata = ~stratum,
  weights = ~weight,
  data = domain_data
)

# District-level estimates (small domains)
district_est <- svyby(
  ~income,
  ~district,
  domain_design_ex,
  svymean,
  vartype = c("se", "cv")
)

# Identify unreliable estimates
unreliable <- district_est %>%
  filter(cv > 0.30) %>%
  arrange(desc(cv))

# Print summary of results
cat("--- Exercise 4 Results ---\n")
print(paste("Total number of districts:", nrow(district_est)))
print(paste("Number of unreliable districts (CV > 30%):", nrow(unreliable)))
cat("\nMost unreliable districts:\n")
print(head(unreliable, 5))
cat("\n")


# -----------------------------------------------------------------------------
# EXERCISE 5: Panel Data Analysis
# -----------------------------------------------------------------------------
# Task: Analyze employment transitions and calculate
# the probability of finding a job for unemployed persons.

# Prepare transition data
transitions_ex <- panel_data %>%
  arrange(person_id, wave) %>%
  group_by(person_id) %>%
  mutate(
    employed_prev = lag(employed),
    wave_prev = lag(wave)
  ) %>%
  ungroup() %>%
  filter(!is.na(employed_prev),
         wave == wave_prev + 1)  # Filter for CONSECUTIVE waves only

# Create transition matrix
trans_matrix <- with(transitions_ex,
                     table(Previous = employed_prev,
                           Current = employed))

# Print summary of results
cat("--- Exercise 5 Results ---\n")
cat("Employment Transition Matrix:\n")
print(trans_matrix)
cat("\n")

# NOTE: The provided data has no consecutive wave transitions.
# The robust code below checks for this and prints a message instead of an error.
if (nrow(trans_matrix) > 0) {
  # Calculate probabilities
  trans_prob <- prop.table(trans_matrix, margin = 1)
  cat("Transition Probabilities:\n")
  print(round(trans_prob, 3))
  
  # Calculate key metrics if dimensions allow
  cat("\nKey Metrics:\n")
  if (all(dim(trans_prob) >= c(1, 2))) {
    cat("P(Find job | Unemployed):", round(trans_prob[1, 2], 3), "\n")
  } else {
    cat("Could not calculate P(Find job | Unemployed) due to matrix dimensions.\n")
  }
} else {
  cat("Result: No consecutive wave transitions were found in the data.\n")
}
cat("\n")