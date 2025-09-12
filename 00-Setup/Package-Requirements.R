#===============================================================================
# SADC Advanced Sampling Methods Workshop
# Day 00: R Package Requirements
# Author: Dr. Endri Raço, PhD
# Date: November 2025
#
# Purpose:        To check for and install all R packages required for the
#                 workshop. This script ensures a consistent and reproducible
#                 environment for all participants.
#
# Instructions:   Run this entire script from within RStudio. You can do this
#                 by opening the file and clicking the "Source" button, or by
#                 typing source('00-Setup/Package-Requirements.R') in the console.
#===============================================================================

# --- 1. Define List of Required Packages ---
#-------------------------------------------------------------------------------
# A vector of package names needed for data manipulation, survey analysis,
# spatial data, reading files, and generating reports/slides.

required_packages <- c(
  "tidyverse",    # Core suite for data science (dplyr, ggplot2, etc.)
  "survey",       # The essential package for complex survey analysis
  "srvyr",        # A 'tidyverse'-friendly wrapper for the 'survey' package
  "readxl",       # To read data from Excel files (e.g., data dictionaries)
  "sf",           # For handling simple features (modern spatial data)
  "xaringan",     # For creating and rendering the workshop slide decks
  "knitr"         # General-purpose package for dynamic report generation
)

# --- 2. Intelligent Installation Logic ---
#-------------------------------------------------------------------------------
# This loop iterates through each package. It first checks if the package is
# already installed. If not, it proceeds to install it from CRAN. This
# prevents unnecessary re-installations.

cat("--- Checking and Installing Required Packages ---\n\n")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    cat(paste0("📦 Package '", pkg, "' not found. Installing now...\n"))
    install.packages(pkg, dependencies = TRUE)
  } else {
    cat(paste0("✔️ Package '", pkg, "' is already installed.\n"))
  }
}

# --- 3. Final Confirmation ---
#-------------------------------------------------------------------------------
# A final message to the user confirming that the setup is complete.

cat("\n--- Setup Complete! ---\n")
cat("All required packages have been checked and installed.\n")
cat("Your R environment is now ready for the SADC workshop! 🎉\n")