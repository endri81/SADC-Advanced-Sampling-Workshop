# Day 4: Complex Survey Designs - Setup Script
# SADC Survey Sampling Workshop
# Run this script first to set up environment and generate all data

# Clear workspace
rm(list = ls())

# Set working directory structure
main_dir <- getwd()
dirs <- c("00-Setup", "01-Slides", "02-Data", "03-Scripts", 
          "04-Exercises", "05-Documentation", "06-Outputs")

# Create directories
for(dir in dirs) {
  if(!dir.exists(dir)) {
    dir.create(dir)
  }
}

# Load required packages
packages <- c("tidyverse", "survey", "sampling", "knitr", "kableExtra",
              "ggplot2", "gridExtra", "lubridate", "haven", "readxl")

# Install missing packages
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))

# Set global options
options(
  scipen = 999,
  digits = 4,
  survey.lonely.psu = "adjust",
  survey.adjust.domain.lonely = TRUE
)

# Set seed for reproducibility
set.seed(2024)

# Define color palette for consistent plotting
workshop_colors <- c(
  primary = "#2E86AB",
  secondary = "#A23B72",
  success = "#73AB84",
  warning = "#F18F01",
  danger = "#C73E1D",
  info = "#6C757D"
)

# Print session info
cat("=== Day 4 Setup Complete ===\n")
cat("R Version:", R.version.string, "\n")
cat("Working Directory:", getwd(), "\n")
cat("Date:", format(Sys.Date(), "%B %d, %Y"), "\n")
cat("Time:", format(Sys.time(), "%H:%M:%S"), "\n")
cat("\nPackages loaded successfully!\n")
cat("\nFolder structure created:\n")
print(dirs)

# Save setup completion flag
save(workshop_colors, file = "00-Setup/setup_complete.RData")
cat("\nSetup saved to 00-Setup/setup_complete.RData\n")
cat("\n=== Ready for Day 4! ===\n")