# ============================================================================
# Script: create_cluster_data.R
# Purpose: Create comprehensive dataset for Day 3 - Multi-stage & Cluster Sampling
# Location: 03-Scripts/create_cluster_data.R
# Author: Survey Sampling Workshop
# Date: 2024
# 
# INSTRUCTIONS FOR PARTICIPANTS:
# 1. Open this script in RStudio
# 2. Set working directory to main workshop folder
# 3. Run entire script (Ctrl+Shift+Enter or Cmd+Shift+Enter)
# 4. Creates 'cluster_survey_data.rds' in 02-Data folder
# ============================================================================

# Clear workspace
rm(list = ls())

# Load required packages
cat("Loading required packages...\n")
library(tidyverse)
library(MASS)  # For mvrnorm

# Check folder structure
if (!dir.exists("02-Data")) {
  cat("Creating 02-Data folder...\n")
  dir.create("02-Data", recursive = TRUE)
}

# Set seed for reproducibility
set.seed(2024)
cat("Random seed set to 2024\n\n")

# ============================================================================
# PART 1: Create PSU (Primary Sampling Unit) Frame
# ============================================================================

cat("Creating PSU frame...\n")

# Create 500 PSUs (villages/enumeration areas)
n_psu <- 500

psu_frame <- data.frame(
  psu_id = 1:n_psu,
  
  # Geographic distribution
  province = sample(c("North", "South", "East", "West"), 
                    n_psu, replace = TRUE, 
                    prob = c(0.25, 0.35, 0.20, 0.20)),
  
  # Urban/Rural classification
  area = sample(c("Urban", "Rural"), 
                n_psu, replace = TRUE, 
                prob = c(0.40, 0.60)),
  
  # Size (number of households)
  stringsAsFactors = FALSE
)

# Assign realistic PSU sizes
# Urban PSUs larger than rural
psu_frame$size <- ifelse(
  psu_frame$area == "Urban",
  round(rlnorm(sum(psu_frame$area == "Urban"), log(800), 0.4)),
  round(rlnorm(sum(psu_frame$area == "Rural"), log(400), 0.5))
)

# Ensure minimum size
psu_frame$size <- pmax(psu_frame$size, 50)

# Add distance from capital (affects cost)
psu_frame$distance_km <- round(rexp(n_psu, 1/100) + 
                                 ifelse(psu_frame$area == "Urban", 0, 50))

# Add accessibility (affects response rate and cost)
psu_frame$accessibility <- factor(
  sample(c("Easy", "Moderate", "Difficult"), 
         n_psu, replace = TRUE,
         prob = c(0.50, 0.35, 0.15))
)

# PSU-level characteristics that affect outcomes
psu_frame$psu_income_mean <- 40000 + 
  ifelse(psu_frame$area == "Urban", 20000, 0) +
  rnorm(n_psu, 0, 5000)

psu_frame$psu_education_mean <- 10 + 
  ifelse(psu_frame$area == "Urban", 2, 0) +
  rnorm(n_psu, 0, 1)

cat(paste("Created", n_psu, "PSUs with total of", 
          sum(psu_frame$size), "households\n"))

# ============================================================================
# PART 2: Create Household-Level Data Within PSUs
# ============================================================================

cat("\nCreating household-level data within PSUs...\n")

# Function to generate households within a PSU
generate_households <- function(psu_info) {
  n_hh <- psu_info$size
  
  # PSU-level random effect (creates clustering)
  psu_effect_income <- rnorm(1, 0, 5000)
  psu_effect_education <- rnorm(1, 0, 1)
  psu_effect_electricity <- rnorm(1, 0, 0.3)
  
  households <- data.frame(
    # Identifiers
    psu_id = psu_info$psu_id,
    household_id = 1:n_hh,
    
    # Geographic
    province = psu_info$province,
    area = psu_info$area,
    distance_km = psu_info$distance_km,
    
    # Household characteristics
    household_size = pmin(12, rpois(n_hh, 3.5) + 1),
    
    # Income with PSU clustering effect
    income = round(rlnorm(n_hh, 
                          log(psu_info$psu_income_mean + psu_effect_income), 
                          0.4)),
    
    # Education with PSU clustering
    education_years = pmin(20, 
                           round(psu_info$psu_education_mean + 
                                   psu_effect_education + 
                                   rnorm(n_hh, 0, 2))),
    
    # Binary outcomes with PSU clustering
    has_electricity = rbinom(n_hh, 1, 
                             plogis(qlogis(0.7) + 
                                      psu_effect_electricity +
                                      ifelse(psu_info$area == "Urban", 0.5, -0.3))),
    
    has_internet = rbinom(n_hh, 1,
                          plogis(qlogis(0.4) + 
                                   psu_effect_electricity * 0.5 +
                                   ifelse(psu_info$area == "Urban", 0.8, -0.5))),
    
    # Employment
    employed_adults = pmin(household_size, 
                           rbinom(n_hh, household_size, 0.45)),
    
    # Health
    health_insurance = rbinom(n_hh, 1, 
                              ifelse(psu_info$area == "Urban", 0.6, 0.3)),
    
    chronic_illness = rbinom(n_hh, 1, 0.15),
    
    # Additional variables
    dwelling_type = sample(c("Formal", "Informal", "Traditional"),
                           n_hh, replace = TRUE,
                           prob = if(psu_info$area == "Urban") 
                             c(0.80, 0.15, 0.05) else c(0.50, 0.20, 0.30)),
    
    water_access = sample(c("Piped", "Public_Tap", "Well", "Other"),
                          n_hh, replace = TRUE,
                          prob = if(psu_info$area == "Urban")
                            c(0.70, 0.20, 0.08, 0.02) else c(0.30, 0.30, 0.30, 0.10)),
    
    stringsAsFactors = FALSE
  )
  
  return(households)
}

# Generate all households
all_households <- list()

pb <- txtProgressBar(min = 0, max = n_psu, style = 3)
for (i in 1:n_psu) {
  all_households[[i]] <- generate_households(psu_frame[i, ])
  setTxtProgressBar(pb, i)
}
close(pb)

# Combine all households
cluster_survey_data <- do.call(rbind, all_households)

# Add unique household ID across entire population
cluster_survey_data$hh_id <- 1:nrow(cluster_survey_data)

cat(paste("\nCreated", nrow(cluster_survey_data), "total households\n"))

# ============================================================================
# PART 3: Calculate Intraclass Correlation (ICC)
# ============================================================================

cat("\nCalculating clustering effects (ICC)...\n")

# Function to calculate ICC
calculate_icc <- function(data, outcome_var, cluster_var) {
  # One-way ANOVA approach
  formula <- as.formula(paste(outcome_var, "~", cluster_var))
  anova_result <- aov(formula, data = data)
  
  # Extract mean squares
  ms_between <- summary(anova_result)[[1]]["Mean Sq"][1, 1]
  ms_within <- summary(anova_result)[[1]]["Mean Sq"][2, 1]
  
  # Average cluster size
  cluster_sizes <- table(data[[cluster_var]])
  n_bar <- mean(cluster_sizes)
  
  # Calculate ICC
  icc <- (ms_between - ms_within) / (ms_between + (n_bar - 1) * ms_within)
  
  return(max(0, icc))  # Ensure non-negative
}

# Calculate ICC for key variables
icc_income <- calculate_icc(cluster_survey_data, "income", "psu_id")
icc_education <- calculate_icc(cluster_survey_data, "education_years", "psu_id")
icc_electricity <- calculate_icc(cluster_survey_data, "has_electricity", "psu_id")

cat(paste("ICC for income:", round(icc_income, 4), "\n"))
cat(paste("ICC for education:", round(icc_education, 4), "\n"))
cat(paste("ICC for electricity:", round(icc_electricity, 4), "\n"))

# ============================================================================
# PART 4: Add Stratification Variables
# ============================================================================

cat("\nAdding stratification variables...\n")

# Create strata based on province and area
cluster_survey_data$stratum <- paste(cluster_survey_data$province, 
                                     cluster_survey_data$area, 
                                     sep = "_")

# Add PSU-level information
cluster_survey_data <- cluster_survey_data %>%
  left_join(psu_frame %>% 
              select(psu_id, accessibility, psu_size = size),
            by = "psu_id")

# ============================================================================
# PART 5: Create Sampling Weights (for exercises)
# ============================================================================

cat("\nCreating sampling weight variables...\n")

# These will be used in exercises
cluster_survey_data$base_weight <- 1  # Placeholder
cluster_survey_data$psu_prob <- NA    # To be calculated
cluster_survey_data$hh_prob <- NA     # To be calculated
cluster_survey_data$final_weight <- NA # To be calculated

# ============================================================================
# PART 6: Summary Statistics
# ============================================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("DATASET CREATION COMPLETE!\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

cat("Summary Statistics:\n")
cat(paste(rep("-", 40), collapse = ""), "\n")

# PSU-level summary
cat("\nPSU Frame Summary:\n")
cat("Total PSUs:", nrow(psu_frame), "\n")
cat("Urban PSUs:", sum(psu_frame$area == "Urban"), "\n")
cat("Rural PSUs:", sum(psu_frame$area == "Rural"), "\n")
cat("Average PSU size:", round(mean(psu_frame$size)), "households\n")
cat("Min PSU size:", min(psu_frame$size), "\n")
cat("Max PSU size:", max(psu_frame$size), "\n")

# Household-level summary
cat("\nHousehold Data Summary:\n")
cat("Total households:", nrow(cluster_survey_data), "\n")
cat("Average income:", round(mean(cluster_survey_data$income)), "\n")
cat("Electricity access:", round(mean(cluster_survey_data$has_electricity) * 100, 1), "%\n")
cat("Internet access:", round(mean(cluster_survey_data$has_internet) * 100, 1), "%\n")

# Stratum distribution
cat("\nStratum Distribution:\n")
print(table(cluster_survey_data$stratum))

# ============================================================================
# PART 7: Save Datasets
# ============================================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("Saving datasets...\n")

# Save PSU frame
saveRDS(psu_frame, file = "02-Data/psu_frame.rds")
write.csv(psu_frame, file = "02-Data/psu_frame.csv", row.names = FALSE)

# Save household data
saveRDS(cluster_survey_data, file = "02-Data/cluster_survey_data.rds")

# Save a smaller sample for quick exercises
set.seed(2024)
sample_indices <- sample(1:nrow(cluster_survey_data), 5000)
cluster_survey_sample <- cluster_survey_data[sample_indices, ]
saveRDS(cluster_survey_sample, file = "02-Data/cluster_survey_sample.rds")

cat("Datasets saved to:\n")
cat("  - 02-Data/psu_frame.rds (PSU frame)\n")
cat("  - 02-Data/psu_frame.csv (PSU frame CSV)\n")
cat("  - 02-Data/cluster_survey_data.rds (Full population)\n")
cat("  - 02-Data/cluster_survey_sample.rds (5000 HH sample)\n")

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("SUCCESS! Data ready for Day 3 exercises\n")
cat("Clustering effects successfully built into data\n")
cat("You can now close this script and open Day 3 slides\n")
cat(paste(rep("=", 60), collapse = ""), "\n")