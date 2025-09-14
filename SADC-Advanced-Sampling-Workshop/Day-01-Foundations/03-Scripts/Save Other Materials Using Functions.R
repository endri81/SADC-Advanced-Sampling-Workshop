# Function to safely save text files
save_text_file <- function(content, filepath) {
  # Create directory if it doesn't exist
  dir_path <- dirname(filepath)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Write the file
  writeLines(content, filepath)
  cat("Saved:", filepath, "\n")
}

# Example: Save the Pre-Workshop Checklist
checklist_content <- "# Pre-Workshop Checklist
## SADC Advanced Sampling Methods Workshop - Day 1

### Technical Prerequisites
- [ ] R version 4.3.0 or higher
- [ ] RStudio installed
- [ ] Required packages installed
- [ ] Git (optional)

### Before Workshop
- [ ] Download materials
- [ ] Test R installation
- [ ] Review basic R syntax
"

save_text_file(checklist_content, "00-Setup/Pre-Workshop-Checklist.md")