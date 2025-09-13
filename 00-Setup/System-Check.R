#===============================================================================
# SADC Advanced Sampling Methods Workshop
# Day 00: System Verification Script
# Author: Dr. Endri Raço, PhD
# Date: November 12, 2025
#
# Purpose:        To perform a final check on the user's system to ensure R,
#                 RStudio, and key packages are correctly installed and ready
#                 for the workshop.
#
# Instructions:   After running the package installer, run this script by
#                 clicking the "Source" button.
#===============================================================================

# --- 1. Initialization & Welcome ---
#-------------------------------------------------------------------------------
cat("--- SADC Workshop System Check ---\n\n")
cat("This script will verify your R environment.\n\n")

# A flag to track if all checks pass.
all_checks_passed <- TRUE

# --- 2. Check R and RStudio Versions ---
#-------------------------------------------------------------------------------
cat("1. Checking R and RStudio Versions...\n")

# Check R Version
r_version <- R.version.string
cat(paste0("   ✔️ R Version: ", r_version, "\n"))

# Check RStudio Version (requires the 'rstudioapi' package, which is standard)
if (require("rstudioapi") && rstudioapi::isAvailable()) {
  rstudio_version <- rstudioapi::getVersion()
  cat(paste0("   ✔️ RStudio Version: ", rstudio_version, "\n\n"))
} else {
  cat("   ⚠️ Could not determine RStudio version. This is usually not critical.\n\n")
}

# --- 3. Check Key Packages ---
#-------------------------------------------------------------------------------
cat("2. Checking for essential R packages...\n")

# List of the most critical packages to check
critical_packages <- c("tidyverse", "survey", "sf")

for (pkg in critical_packages) {
  # We use `requireNamespace` for a quiet check that doesn't attach the package.
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat(paste0("   ✔️ Successfully found package: '", pkg, "'\n"))
  } else {
    cat(paste0("   ❌ FAILED to find package: '", pkg, "'\n"))
    all_checks_passed <- FALSE
  }
}

# --- 4. Final Report ---
#-------------------------------------------------------------------------------
cat("\n--- System Check Complete ---\n\n")

if (all_checks_passed) {
  cat("✅ SUCCESS: Your system appears to be ready for the workshop!\n")
  cat("All essential components were found. Well done! 🎉\n")
} else {
  cat("❌ ACTION REQUIRED: One or more system checks failed.\n")
  cat("Please try re-running the '01-Package-Requirements.R' script.\n")
  cat("If the problem persists, please consult the 'Troubleshooting-Guide.Rmd'.\n")
}