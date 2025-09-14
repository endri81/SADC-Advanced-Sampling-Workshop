# Day 4 - Part 4: Domain and Small Area Estimation
# Corresponds to Slides 151-200
# Run after data generation script

library(tidyverse)
library(survey)
library(lme4)

# Load the domain estimation data
domain_data <- read.csv("02-Data/domain_estimation_data.csv")

cat("========================================\n")
cat("PART 4: DOMAIN AND SMALL AREA ESTIMATION\n")
cat("Slides 151-200\n")
cat("========================================\n\n")

# ===== SLIDE 151-155: Domain Overview =====
cat("--- Domain Overview (Slides 151-155) ---\n")

# Examine domain structure
domain_structure <- domain_data %>%
  count(province, district, urban_rural) %>%
  group_by(province) %>%
  summarise(
    n_districts = n_distinct(district),
    n_observations = sum(n),
    urban_pct = sum(n[urban_rural == "Urban"]) / sum(n) * 100
  )

cat("Domain Structure Summary:\n")
print(domain_structure)

# Cross-tabulation of domains
domain_crosstab <- table(domain_data$province, domain_data$urban_rural)
cat("\nProvince by Urban/Rural:\n")
print(domain_crosstab)

# ===== SLIDE 156-160: Create Survey Design =====
cat("\n--- Survey Design Setup (Slides 156-160) ---\n")

# Create survey design with domains
domain_design <- svydesign(
  ids = ~psu,
  strata = ~stratum,
  weights = ~weight,
  data = domain_data,
  nest = TRUE
)

cat("Domain Survey Design Created\n")
print(domain_design)

# ===== SLIDE 161-165: Direct Domain Estimates =====
cat("\n--- Direct Domain Estimates (Slides 161-165) ---\n")

# Direct estimates by province
province_direct <- svyby(
  ~income + employed + health_insurance,
  ~province,
  design = domain_design,
  svymean,
  vartype = c("se", "cv")
)

cat("\nDirect Estimates by Province:\n")
print(province_direct)

# Check sample sizes by province
province_n <- domain_data %>%
  group_by(province) %>%
  summarise(n = n())

cat("\nSample Sizes by Province:\n")
print(province_n)

# ===== SLIDE 166-170: Small Domains =====
cat("\n--- Small Domain Analysis (Slides 166-170) ---\n")

# District level estimates (smaller domains)
district_direct <- svyby(
  ~income,
  ~district,
  design = domain_design,
  svymean,
  vartype = c("se", "cv"),
  keep.var = TRUE
)

# Identify unreliable estimates (CV > 30%)
unreliable_districts <- district_direct %>%
  filter(cv > 0.30) %>%
  arrange(desc(cv))

cat("\nDistricts with Unreliable Estimates (CV > 30%):\n")
print(unreliable_districts)

# Sample sizes by district
district_n <- domain_data %>%
  count(district) %>%
  arrange(n)

cat("\nSmallest District Sample Sizes:\n")
print(head(district_n, 10))

# ===== SLIDE 171-175: Domain Quality Metrics =====
cat("\n--- Domain Quality Metrics (Slides 171-175) ---\n")

# Calculate effective sample sizes for domains
calculate_domain_neff <- function(domain_var) {
  domain_estimates <- svyby(
    ~income,
    as.formula(paste("~", domain_var)),
    design = domain_design,
    svymean,
    vartype = "se",
    deff = TRUE
  )
  
  domain_n <- table(domain_data[[domain_var]])
  
  neff_df <- data.frame(
    domain = names(domain_n),
    n = as.numeric(domain_n),
    deff = domain_estimates$DEff.income,
    neff = as.numeric(domain_n) / domain_estimates$DEff.income
  )
  
  return(neff_df)
}

# Calculate for provinces
province_neff <- calculate_domain_neff("province")
cat("\nEffective Sample Sizes by Province:\n")
print(province_neff)

# ===== SLIDE 176-180: Composite Estimators =====
cat("\n--- Composite Estimators (Slides 176-180) ---\n")

# Synthetic estimation example
# Overall mean as synthetic estimate for small domains
overall_mean <- svymean(~income, domain_design)

# Combine direct and synthetic estimates
# Weight by sample size (simplified composite)
composite_estimates <- district_direct %>%
  left_join(district_n, by = "district") %>%
  mutate(
    synthetic_est = as.numeric(overall_mean[1]),
    weight = pmin(n / 100, 1),  # Weight based on sample size
    composite_est = weight * income + (1 - weight) * synthetic_est,
    composite_se = se * sqrt(weight)  # Simplified SE
  )

cat("\nComposite Estimates for Small Districts:\n")
print(head(composite_estimates %>% 
             select(district, n, income, synthetic_est, composite_est, cv), 10))

# ===== SLIDE 181-185: Model-Based Estimation =====
cat("\n--- Model-Based Small Area Estimation (Slides 181-185) ---\n")

# Fit area-level model (Fay-Herriot type)
# Using district-level data
district_summary <- domain_data %>%
  group_by(district, urban_rural) %>%
  summarise(
    mean_income = mean(income),
    mean_education = mean(education_level == "Tertiary"),
    employment_rate = mean(employed),
    n = n(),
    .groups = "drop"
  )

# Simple area-level model
area_model <- lm(
  mean_income ~ urban_rural + mean_education + employment_rate,
  data = district_summary,
  weights = n
)

cat("\nArea-Level Model:\n")
print(summary(area_model))

# Get model-based predictions
district_summary$model_pred <- predict(area_model)

# ===== SLIDE 186-190: Mixed Model Approach =====
cat("\n--- Mixed Model Approach (Slides 186-190) ---\n")

# Fit mixed effects model
mixed_model <- lmer(
  income ~ urban_rural + education_level + employed + 
    (1 | province) + (1 | district),
  data = domain_data,
  weights = weight
)

cat("\nMixed Effects Model:\n")
print(summary(mixed_model))

# Extract random effects (BLUPs)
province_effects <- ranef(mixed_model)$province
district_effects <- ranef(mixed_model)$district

cat("\nProvince Random Effects:\n")
print(head(province_effects))

# ===== SLIDE 191-195: Calibration for Domains =====
cat("\n--- Domain Calibration (Slides 191-195) ---\n")

# Calibrate domain estimates to known totals
# Example: calibrate to known provincial populations
known_totals <- data.frame(
  province = paste0("Prov", 1:10),
  pop_total = c(1.2e6, 0.8e6, 1.5e6, 0.9e6, 1.1e6,
                0.7e6, 1.3e6, 0.6e6, 1.0e6, 0.9e6)
)

# Calculate calibration factors
current_totals <- svyby(
  ~weight,
  ~province,
  domain_design,
  svytotal
)

calibration_factors <- known_totals %>%
  left_join(
    data.frame(
      province = rownames(current_totals),
      current = current_totals$weight
    ),
    by = "province"
  ) %>%
  mutate(cal_factor = pop_total / current)

cat("\nCalibration Factors by Province:\n")
print(calibration_factors)

# ===== SLIDE 196-200: Quality Assessment =====
cat("\n--- Domain Estimation Quality (Slides 196-200) ---\n")

# Create comprehensive quality report
quality_report <- province_direct %>%
  select(province, income, se = se.income, cv = cv.income) %>% # Corrected column names
  left_join(province_n, by = "province") %>%
  left_join(province_neff %>% select(domain, neff), 
            by = c("province" = "domain")) %>%
  mutate(
    reliability = case_when(
      cv < 0.15 ~ "Good",
      cv < 0.30 ~ "Acceptable",
      TRUE ~ "Poor"
    )
  )

# Print the resulting report
print(quality_report)

# Visualize domain estimates with uncertainty
library(ggplot2)

domain_plot <- province_direct %>%
  ggplot(aes(x = reorder(province, income), y = income)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = income - 1.96 * se.income,  # Changed from se
                    ymax = income + 1.96 * se.income), # Changed from se
                width = 0.3) +
  coord_flip() +
  labs(title = "Income by Province with 95% CI",
       subtitle = "Direct Domain Estimates",
       x = "Province", y = "Mean Income") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

print(domain_plot)
ggsave("06-Outputs/domain_estimates.png", domain_plot, width = 10, height = 8)

# Comparison plot: Direct vs Model-based
comparison_data <- district_summary %>%
  left_join(
    district_direct %>% select(district, direct_est = income, direct_se = se),
    by = "district"
  ) %>%
  filter(!is.na(direct_est))

comparison_plot <- comparison_data %>%
  ggplot(aes(x = direct_est, y = model_pred)) +
  geom_point(aes(size = n), alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Direct vs Model-Based Estimates",
       x = "Direct Estimate", y = "Model-Based Estimate",
       size = "Sample Size") +
  theme_minimal()

print(comparison_plot)
ggsave("06-Outputs/direct_vs_model.png", comparison_plot, width = 8, height = 6)

# Summary statistics
summary_stats <- data.frame(
  Method = c("Direct", "Synthetic", "Composite", "Model-Based"),
  Mean_CV = c(
    mean(province_direct$cv, na.rm = TRUE),
    0.05,  # Synthetic has low CV but potential bias
    mean(composite_estimates$composite_se / composite_estimates$composite_est, na.rm = TRUE),
    0.08  # Model-based typically lower CV
  ),
  Coverage = c(
    sum(province_n$n >= 100) / nrow(province_n) * 100,
    100,  # Synthetic covers all
    100,  # Composite covers all
    100   # Model covers all
  ),
  Bias_Risk = c("Low", "High", "Medium", "Low-Medium")
)

cat("\n\nComparison of Estimation Methods:\n")
print(summary_stats)

# Save results
save(domain_design, province_direct, district_direct,
     composite_estimates, area_model, mixed_model,
     quality_report,
     file = "06-Outputs/part4_domain_results.RData")

cat("\n========================================\n")
cat("Part 4 Analysis Complete!\n")
cat("Results saved to: 06-Outputs/part4_domain_results.RData\n")
cat("========================================\n")