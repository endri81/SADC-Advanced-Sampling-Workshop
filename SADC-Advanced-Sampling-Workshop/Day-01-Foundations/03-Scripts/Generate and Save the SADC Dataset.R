# Generate SADC Household Sample Data
# Make sure you're in the SADC-Advanced-Sampling-Workshop directory

set.seed(2025)  # For reproducibility

# Generate realistic SADC household sample data
n <- 5000  # Sample size

# Create household data
SADC_household_sample <- data.frame(
  # Identification variables
  household_id = sprintf("HH%05d", 1:n),
  cluster_id = sample(1:250, n, replace = TRUE),
  
  # Geographic variables
  country = sample(c("South Africa", "Botswana", "Namibia", "Zimbabwe", 
                     "Mozambique", "Zambia", "Malawi", "Tanzania"), 
                   n, replace = TRUE, 
                   prob = c(0.25, 0.10, 0.08, 0.12, 0.15, 0.10, 0.10, 0.10)),
  
  region_type = sample(c("Urban", "Rural"), n, replace = TRUE, 
                       prob = c(0.35, 0.65)),
  
  # Household characteristics
  household_size = rpois(n, lambda = 4) + 1,
  
  # Head of household characteristics
  head_age = round(rnorm(n, mean = 45, sd = 15)),
  head_gender = sample(c("Male", "Female"), n, replace = TRUE, 
                       prob = c(0.65, 0.35)),
  head_education = sample(c("No education", "Primary", "Secondary", "Tertiary"), 
                          n, replace = TRUE, 
                          prob = c(0.15, 0.35, 0.35, 0.15)),
  
  # Economic variables
  monthly_income = round(rlnorm(n, meanlog = 8.5, sdlog = 0.8)),
  
  # Employment
  employed_members = NA,  # Will calculate based on household size
  
  # Access to services
  electricity_access = sample(c(0, 1), n, replace = TRUE, prob = c(0.25, 0.75)),
  water_access = sample(c(0, 1), n, replace = TRUE, prob = c(0.20, 0.80)),
  internet_access = sample(c(0, 1), n, replace = TRUE, prob = c(0.60, 0.40)),
  
  # Health indicators
  health_insurance = sample(c(0, 1), n, replace = TRUE, prob = c(0.70, 0.30)),
  distance_health_facility = round(rgamma(n, shape = 2, rate = 0.5), 1),
  
  # Agricultural indicators
  owns_land = sample(c(0, 1), n, replace = TRUE, prob = c(0.55, 0.45)),
  livestock_count = rpois(n, lambda = 2),
  
  # Survey metadata
  interview_date = sample(seq(as.Date("2025-01-01"), 
                              as.Date("2025-03-31"), by = "day"), 
                          n, replace = TRUE),
  response_status = sample(c("Complete", "Partial", "Refused"), n, 
                           replace = TRUE, prob = c(0.85, 0.10, 0.05)),
  
  # Sampling weights (will be calculated properly in exercises)
  sampling_weight = runif(n, min = 50, max = 200)
)

# Calculate employed members based on household size
SADC_household_sample$employed_members <- pmin(
  SADC_household_sample$household_size - 1,
  rbinom(n, size = SADC_household_sample$household_size - 1, prob = 0.4)
)

# Adjust income based on education and region
SADC_household_sample$monthly_income <- round(
  SADC_household_sample$monthly_income * 
    ifelse(SADC_household_sample$head_education == "Tertiary", 1.8,
           ifelse(SADC_household_sample$head_education == "Secondary", 1.3,
                  ifelse(SADC_household_sample$head_education == "Primary", 0.9, 0.7))) *
    ifelse(SADC_household_sample$region_type == "Urban", 1.2, 0.8)
)

# Ensure age is realistic
SADC_household_sample$head_age[SADC_household_sample$head_age < 18] <- 18
SADC_household_sample$head_age[SADC_household_sample$head_age > 95] <- 95

# Add some missing values for realism (3% missing)
missing_vars <- c("monthly_income", "head_education", "distance_health_facility")
for(var in missing_vars) {
  missing_idx <- sample(1:n, size = round(n * 0.03))
  SADC_household_sample[missing_idx, var] <- NA
}

# Check if directory exists, if not create it
if (!dir.exists("Day-01-Foundations/02-Data")) {
  dir.create("Day-01-Foundations/02-Data", recursive = TRUE)
}

# Save the dataset
write.csv(SADC_household_sample, 
          "Day-01-Foundations/02-Data/SADC_household_sample.csv", 
          row.names = FALSE)

cat("Dataset saved successfully!\n")
cat("Location: Day-01-Foundations/02-Data/SADC_household_sample.csv\n")
cat("Rows:", nrow(SADC_household_sample), "\n")
cat("Columns:", ncol(SADC_household_sample), "\n")