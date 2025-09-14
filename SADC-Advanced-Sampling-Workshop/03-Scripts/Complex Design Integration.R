# Day 4 - Part 3: Complex Design Integration
# Corresponds to Slides 101-150
# Run after data generation script

library(tidyverse)
library(survey)
library(kableExtra)

# Load the complex design data
psu_frame <- read.csv("02-Data/complex_psu_frame.csv")
selected_psus <- read.csv("02-Data/complex_selected_psus.csv")
household_data <- read.csv("02-Data/complex_household_data.csv")

cat("========================================\n")
cat("PART 3: COMPLEX DESIGN INTEGRATION\n")
cat("Slides 101-150\n")
cat("========================================\n\n")

# ===== SLIDE 101-105: Design Overview =====
cat("--- Complex Design Overview (Slides 101-105) ---\n")

# Summarize the complex design
design_summary <- list(
  total_psus = nrow(psu_frame),
  selected_psus = nrow(selected_psus),
  total_households = nrow(household_data),
  strata = n_distinct(household_data$stratum),
  provinces = n_distinct(household_data$province),
  urban_rural_split = table(household_data$urban_rural)
)

cat("Complex Design Summary:\n")
print(design_summary)

# Stratification table
strata_summary <- household_data %>%
  group_by(stratum, province, urban_rural) %>%
  summarise(
    n_psus = n_distinct(psu_id),
    n_households = n(),
    avg_hh_per_psu = n() / n_distinct(psu_id)
  ) %>%
  arrange(province, urban_rural)

cat("\nStratification Summary:\n")
print(as.data.frame(strata_summary))

# ===== SLIDE 106-110: Weight Components =====
cat("\n--- Weight Components (Slides 106-110) ---\n")

# Examine weight distribution
weight_summary <- household_data %>%
  summarise(
    min_weight = min(final_weight),
    q1_weight = quantile(final_weight, 0.25),
    median_weight = median(final_weight),
    mean_weight = mean(final_weight),
    q3_weight = quantile(final_weight, 0.75),
    max_weight = max(final_weight),
    cv_weight = sd(final_weight) / mean(final_weight)
  )

cat("\nWeight Distribution:\n")
print(weight_summary)

# Weight components by stratum
weight_components <- household_data %>%
  group_by(stratum) %>%
  summarise(
    mean_psu_weight = mean(psu_weight),
    mean_hh_weight = mean(hh_weight),
    mean_final_weight = mean(final_weight),
    cv_final = sd(final_weight) / mean(final_weight)
  )

cat("\nWeight Components by Stratum:\n")
print(as.data.frame(weight_components))

# ===== SLIDE 111-115: Create Survey Design Object =====
cat("\n--- Survey Design Object (Slides 111-115) ---\n")

# Create complex survey design
complex_design <- svydesign(
  ids = ~psu_id,          # PSU is cluster variable
  strata = ~stratum,      # Explicit stratification
  weights = ~final_weight,# Final weights
  data = household_data,
  nest = TRUE            # PSUs nested within strata
)

cat("Complex Survey Design Created:\n")
print(complex_design)

# Design effects
deff_income <- deff(svymean(~income, complex_design))
cat("\nDesign Effect for Income:", deff_income, "\n")

# ===== SLIDE 116-120: Point Estimates =====
cat("\n--- Point Estimates (Slides 116-120) ---\n")

# Calculate various estimates
estimates_continuous <- svymean(
  ~income + household_size + education_head,
  design = complex_design
)

estimates_binary <- svymean(
  ~has_electricity + has_internet,
  design = complex_design
)

cat("\nContinuous Variable Estimates:\n")
print(estimates_continuous)
print(confint(estimates_continuous))

cat("\nBinary Variable Estimates (Proportions):\n")
print(estimates_binary)
print(confint(estimates_binary))

# ===== SLIDE 121-125: Domain Estimation =====
cat("\n--- Domain Estimation (Slides 121-125) ---\n")

# Estimates by urban/rural
urban_rural_estimates <- svyby(
  ~income + has_electricity + has_internet,
  ~urban_rural,
  design = complex_design,
  svymean
)

cat("\nEstimates by Urban/Rural:\n")
print(urban_rural_estimates)

# Estimates by province
province_estimates <- svyby(
  ~income,
  ~province,
  design = complex_design,
  svymean,
  vartype = c("se", "cv")
)

cat("\nIncome by Province:\n")
print(province_estimates)

# ===== SLIDE 126-130: Cross-Classifications =====
cat("\n--- Cross-Classifications (Slides 126-130) ---\n")

# Create cross-classification domain
household_data$domain <- paste(household_data$province, 
                               household_data$urban_rural, sep = "_")

# Update design with domain
complex_design_domain <- svydesign(
  ids = ~psu_id,
  strata = ~stratum,
  weights = ~final_weight,
  data = household_data,
  nest = TRUE
)

# Estimates for cross-classified domains
domain_estimates <- svyby(
  ~income,
  ~domain,
  design = complex_design_domain,
  svymean,
  vartype = c("se", "cv")
)

cat("\nEstimates by Province × Urban/Rural:\n")
print(domain_estimates)

# Check sample sizes
domain_sizes <- household_data %>%
  group_by(domain) %>%
  summarise(
    n_psus = n_distinct(psu_id),
    n_households = n()
  )

cat("\nDomain Sample Sizes:\n")
print(domain_sizes)

# ===== SLIDE 131-135: Regression Analysis =====
cat("\n--- Regression Analysis (Slides 131-135) ---\n")

# Complex survey regression
model1 <- svyglm(
  income ~ urban_rural + education_head + household_size + 
    has_electricity + has_internet,
  design = complex_design
)

cat("\nSurvey-Weighted Regression Model:\n")
print(summary(model1))

# Compare with unweighted
model_unweighted <- lm(
  income ~ urban_rural + education_head + household_size + 
    has_electricity + has_internet,
  data = household_data
)

cat("\nComparison: Unweighted Coefficients:\n")
print(coef(model_unweighted))

# ===== SLIDE 136-140: Variance Estimation Methods =====
cat("\n--- Variance Estimation Methods (Slides 136-140) ---\n")

# Taylor linearization (default)
taylor_est <- svymean(~income, complex_design)

# Jackknife
options(survey.replicates.mse = TRUE)
jk_design <- as.svrepdesign(complex_design, type = "JKn")
jk_est <- svymean(~income, jk_design)

# Bootstrap
boot_design <- as.svrepdesign(complex_design, type = "bootstrap", replicates = 100)
boot_est <- svymean(~income, boot_design)

# Compare methods
variance_comparison <- data.frame(
  Method = c("Taylor", "Jackknife", "Bootstrap"),
  Estimate = c(coef(taylor_est), coef(jk_est), coef(boot_est)),
  SE = c(SE(taylor_est), SE(jk_est), SE(boot_est))
)

cat("\nVariance Estimation Method Comparison:\n")
print(variance_comparison)

# ===== SLIDE 141-145: Calibration =====
cat("\n--- Calibration/Post-Stratification (Slides 141-145) ---\n")

# Population totals for calibration
pop_totals <- data.frame(
  urban_rural = c("Urban", "Rural"),
  Freq = c(4000000, 6000000)  # Known population totals
)

# Post-stratify
ps_design <- postStratify(
  complex_design,
  ~urban_rural,
  pop_totals
)

cat("Post-Stratified Design Created\n")

# Compare estimates
original_est <- svymean(~income, complex_design)
calibrated_est <- svymean(~income, ps_design)

cat("\nOriginal Estimate:", coef(original_est), "SE:", SE(original_est), "\n")
cat("Calibrated Estimate:", coef(calibrated_est), "SE:", SE(calibrated_est), "\n")

# ===== SLIDE 146-150: Quality Assessment =====
cat("\n--- Quality Assessment (Slides 146-150) ---\n")

# Calculate quality metrics
quality_metrics <- list()

# 1. Design effects by variable
variables <- c("income", "has_electricity", "has_internet")
deff_values <- numeric(length(variables))
for(i in seq_along(variables)) {
  formula <- as.formula(paste("~", variables[i]))
  deff_result <- try(deff(svymean(formula, complex_design)), silent = TRUE)
  if(!inherits(deff_result, "try-error") && !is.null(deff_result)) {
    deff_values[i] <- as.numeric(deff_result)
  } else {
    # If deff returns NULL or error, calculate manually
    # DEFF = var(complex) / var(SRS)
    complex_var <- SE(svymean(formula, complex_design))^2
    srs_var <- var(household_data[[variables[i]]]) / nrow(household_data)
    deff_values[i] <- complex_var / srs_var
  }
}
names(deff_values) <- variables

quality_metrics$design_effects <- deff_values

# 2. Coefficient of variation by domain
cv_by_domain <- domain_estimates$cv.income

quality_metrics$cv_range <- c(
  min = min(cv_by_domain),
  median = median(cv_by_domain),
  max = max(cv_by_domain)
)

# 3. Effective sample sizes
quality_metrics$effective_n <- nrow(household_data) / mean(deff_values)

# 4. Weight diagnostics
quality_metrics$weight_cv <- sd(household_data$final_weight) / 
  mean(household_data$final_weight)

cat("\nQuality Assessment:\n")
cat("Design Effects:\n")
print(quality_metrics$design_effects)
cat("\nCV Range across domains:\n")
print(quality_metrics$cv_range)
cat("\nEffective Sample Size:", round(quality_metrics$effective_n), "\n")
cat("Weight CV:", round(quality_metrics$weight_cv, 3), "\n")

# Create summary plot
library(ggplot2)

quality_plot <- domain_estimates %>%
  ggplot(aes(x = reorder(domain, income), y = income)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = income - 1.96 * se,  # Changed from se.income to se
                    ymax = income + 1.96 * se), # Changed from se.income to se
                width = 0.2) +
  coord_flip() +
  labs(title = "Income Estimates by Domain with 95% CI",
       x = "Domain", y = "Mean Income") +
  theme_minimal()

print(quality_plot)
ggsave("06-Outputs/domain_estimates_complex.png", quality_plot, 
       width = 10, height = 8)

# Save results
save(complex_design, ps_design, urban_rural_estimates, 
     province_estimates, domain_estimates, model1,
     variance_comparison, quality_metrics,
     file = "06-Outputs/part3_complex_design_results.RData")

cat("\n========================================\n")
cat("Part 3 Analysis Complete!\n")
cat("Results saved to: 06-Outputs/part3_complex_design_results.RData\n")
cat("========================================\n")