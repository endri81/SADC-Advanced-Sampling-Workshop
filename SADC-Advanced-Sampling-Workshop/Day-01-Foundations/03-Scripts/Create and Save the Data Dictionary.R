# Create data dictionary
data_dictionary <- data.frame(
  Variable = names(SADC_household_sample),
  Type = sapply(SADC_household_sample, class),
  Description = c(
    "Unique household identifier",
    "Cluster/EA identifier",
    "SADC member country",
    "Urban or Rural classification",
    "Number of household members",
    "Age of household head (years)",
    "Gender of household head",
    "Highest education level of household head",
    "Total household monthly income (local currency units)",
    "Number of employed household members",
    "Access to electricity (1=Yes, 0=No)",
    "Access to clean water (1=Yes, 0=No)",
    "Internet access in household (1=Yes, 0=No)",
    "Any member has health insurance (1=Yes, 0=No)",
    "Distance to nearest health facility (km)",
    "Household owns agricultural land (1=Yes, 0=No)",
    "Number of livestock owned",
    "Date of interview",
    "Survey response status",
    "Sampling weight for analysis"
  ),
  stringsAsFactors = FALSE
)

# Save data dictionary
write.csv(data_dictionary, 
          "Day-01-Foundations/02-Data/SADC_data_dictionary.csv", 
          row.names = FALSE)

cat("\nData dictionary saved successfully!\n")