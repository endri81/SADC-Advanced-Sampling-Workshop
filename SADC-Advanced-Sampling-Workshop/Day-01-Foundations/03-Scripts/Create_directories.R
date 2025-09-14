# Create main workshop directory structure
# Run this from your working directory

# Check current working directory
getwd()

# Create main directories
dir.create("SADC-Advanced-Sampling-Workshop", showWarnings = FALSE)
setwd("SADC-Advanced-Sampling-Workshop")  # Move into the main directory

# Create Day 0 Setup directory
dir.create("00-Setup", showWarnings = FALSE)

# Create Day 1 directory structure
dir.create("Day-01-Foundations", showWarnings = FALSE)
dir.create("Day-01-Foundations/01-Slides", showWarnings = FALSE)
dir.create("Day-01-Foundations/02-Data", showWarnings = FALSE)
dir.create("Day-01-Foundations/03-Scripts", showWarnings = FALSE)
dir.create("Day-01-Foundations/04-Exercises", showWarnings = FALSE)
dir.create("Day-01-Foundations/05-Documentation", showWarnings = FALSE)
dir.create("Day-01-Foundations/06-Outputs", showWarnings = FALSE)

# Verify structure was created
list.dirs(recursive = TRUE)