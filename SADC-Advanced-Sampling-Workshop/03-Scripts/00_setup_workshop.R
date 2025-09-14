# ============================================================================
# Script: 00_setup_workshop.R
# Purpose: Initial setup and verification for workshop
# Location: Place in main workshop folder (same level as folders)
# Author: Survey Sampling Workshop
# Date: 2024
#
# INSTRUCTIONS FOR PARTICIPANTS:
# 1. Place this script in your main workshop folder
# 2. Set your working directory to the workshop folder
# 3. Run this entire script
# 4. It will create folders, data, and verify everything works
# ============================================================================

cat(paste(rep("=", 60), collapse = ""), "\n")
cat("SURVEY SAMPLING WORKSHOP - SETUP SCRIPT\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# Step 1: Check and create folder structure ----------------------------------
cat("Step 1: Checking folder structure...\n")

folders <- c("01-Slides", "02-Data", "03-Scripts", 
             "04-Exercises", "05-Documentation", "06-Outputs")

for (folder in folders) {
  if (!dir.exists(folder)) {
    dir.create(folder)
    cat("  Created:", folder, "\n")
  } else {
    cat("  Found:", folder, "\n")
  }
}

# Step 2: Check R packages ----------------------------------------------------
cat("\nStep 2: Checking required packages...\n")

required_packages <- c("tidyverse", "survey", "knitr", "kableExtra", "xaringan")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("  Installing:", pkg, "\n")
    install.packages(pkg)
  } else {
    cat("  ✓", pkg, "installed\n")
  }
}

# Step 3: Create the main dataset --------------------------------------------
cat("\nStep 3: Creating workshop data...\n")

if (!file.exists("02-Data/sadc_survey_data.rds")) {
  cat("  Creating SADC survey data...\n")
  source("03-Scripts/create_sadc_data.R")
} else {
  cat("  ✓ Data already exists\n")
}

# Step 4: Verify data ---------------------------------------------------------
cat("\nStep 4: Verifying data...\n")

if (file.exists("02-Data/sadc_survey_data.rds")) {
  test_data <- readRDS("02-Data/sadc_survey_data.rds")
  cat("  ✓ Data loaded successfully\n")
  cat("  Observations:", nrow(test_data), "\n")
  cat("  Variables:", ncol(test_data), "\n")
  rm(test_data)
} else {
  cat("  ✗ Data file not found - please check!\n")
}

# Step 5: Test functions ------------------------------------------------------
cat("\nStep 5: Testing sampling functions...\n")

if (file.exists("03-Scripts/sampling_functions.R")) {
  source("03-Scripts/sampling_functions.R")
  cat("  ✓ Functions loaded successfully\n")
} else {
  cat("  ✗ Functions script not found\n")
}

# Step 6: Set working directory info -----------------------------------------
cat("\nStep 6: Working directory information...\n")
cat("  Current working directory:\n")
cat("  ", getwd(), "\n")
cat("\n  Files in working directory:\n")
main_files <- list.files(pattern = "\\.(R|Rmd)$")
if (length(main_files) > 0) {
  for (f in main_files[1:min(5, length(main_files))]) {
    cat("    -", f, "\n")
  }
}

# Final message ---------------------------------------------------------------
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("SETUP COMPLETE!\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("\nYou are ready to start the workshop!\n")
cat("\nNext steps:\n")
cat("1. Open '01-Slides/Day2_Part1_Slides.Rmd'\n")
cat("2. Click 'Knit' to generate the slides\n")
cat("3. Or run chunks interactively in RStudio\n")
cat("\nRemember:\n")
cat("- Working directory should be:", getwd(), "\n")
cat("- All paths in slides are relative to this location\n")
cat(paste(rep("=", 60), collapse = ""), "\n")