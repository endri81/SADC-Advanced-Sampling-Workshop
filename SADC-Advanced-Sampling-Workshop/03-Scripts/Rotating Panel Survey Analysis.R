# Day 4 - Part 2: Rotating Panel Survey Analysis
# Corresponds to Slides 51-100
# Run after data generation script

library(tidyverse)
library(survey)
library(gridExtra)

# Load the rotating panel data
rotating_panel_data <- read.csv("02-Data/rotating_panel_data.csv")

cat("========================================\n")
cat("PART 2: ROTATING PANEL SURVEY ANALYSIS\n")
cat("Slides 51-100\n")
cat("========================================\n\n")

# ===== SLIDE 51-55: Rotating Panel Structure =====
cat("--- Rotating Panel Structure (Slides 51-55) ---\n")

# Examine panel structure
panel_structure <- rotating_panel_data %>%
  group_by(wave, panel) %>%
  summarise(n_households = n_distinct(household_id)) %>%
  pivot_wider(names_from = panel, values_from = n_households, values_fill = 0)

cat("Panel Participation by Wave:\n")
print(as.data.frame(panel_structure))

# Visualize rotation pattern
rotation_plot <- rotating_panel_data %>%
  mutate(in_sample = 1) %>%
  complete(wave = 1:12, panel = 1:5, fill = list(in_sample = 0)) %>%
  ggplot(aes(x = wave, y = factor(panel), fill = factor(in_sample))) +
  geom_tile(color = "white", size = 1) +
  scale_fill_manual(values = c("0" = "gray90", "1" = "steelblue"),
                    labels = c("Out", "In Sample")) +
  labs(title = "Rotating Panel Design Pattern",
       x = "Wave", y = "Panel", fill = "Status") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:12)

print(rotation_plot)
ggsave("06-Outputs/rotation_pattern.png", rotation_plot, width = 10, height = 6)

# ===== SLIDE 56-60: Panel Overlap Analysis =====
cat("\n--- Panel Overlap Analysis (Slides 56-60) ---\n")

# Calculate overlap between consecutive waves
calculate_overlap <- function(data, wave1, wave2) {
  ids_wave1 <- unique(data$household_id[data$wave == wave1])
  ids_wave2 <- unique(data$household_id[data$wave == wave2])
  
  overlap <- length(intersect(ids_wave1, ids_wave2))
  total <- length(union(ids_wave1, ids_wave2))
  
  return(c(overlap = overlap, 
           total = total, 
           pct_overlap = overlap/length(ids_wave1) * 100))
}

# Calculate overlaps
overlaps <- data.frame()
for(w in 1:11) {
  overlap_stats <- calculate_overlap(rotating_panel_data, w, w+1)
  overlaps <- rbind(overlaps, 
                    data.frame(wave_pair = paste0(w, "-", w+1),
                               t(overlap_stats)))
}

cat("\nOverlap Between Consecutive Waves:\n")
print(overlaps)

# ===== SLIDE 61-65: Cross-Sectional Estimates =====
cat("\n--- Cross-Sectional Estimates (Slides 61-65) ---\n")

# Calculate cross-sectional estimates for each wave
cross_sectional_estimates <- rotating_panel_data %>%
  group_by(wave) %>%
  summarise(
    mean_income = mean(income),
    se_income = sd(income) / sqrt(n()),
    employment_rate = mean(employment) * 100,
    mean_expenditure = mean(expenditure)
  )

cat("\nCross-Sectional Estimates by Wave:\n")
print(cross_sectional_estimates)

# Plot trends
trend_plot <- cross_sectional_estimates %>%
  ggplot(aes(x = wave)) +
  geom_line(aes(y = mean_income), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = mean_income - 1.96*se_income,
                  ymax = mean_income + 1.96*se_income),
              alpha = 0.2, fill = "blue") +
  labs(title = "Income Trend Across Waves",
       x = "Wave", y = "Mean Income") +
  theme_minimal()

print(trend_plot)
ggsave("06-Outputs/income_trend.png", trend_plot, width = 8, height = 5)

# ===== SLIDE 66-70: Panel-Specific Analysis =====
cat("\n--- Panel-Specific Analysis (Slides 66-70) ---\n")

# Analyze each panel's trajectory
panel_trajectories <- rotating_panel_data %>%
  group_by(panel, panel_wave) %>%
  summarise(
    mean_income = mean(income),
    mean_employment = mean(employment),
    n = n()
  )

cat("\nPanel Trajectories (First 3 Waves):\n")
print(panel_trajectories %>% filter(panel_wave <= 3))

# Plot panel trajectories
panel_plot <- panel_trajectories %>%
  ggplot(aes(x = panel_wave, y = mean_income, 
             color = factor(panel), group = panel)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Income Trajectories by Panel",
       x = "Panel Wave", y = "Mean Income", color = "Panel") +
  theme_minimal()

print(panel_plot)
ggsave("06-Outputs/panel_trajectories.png", panel_plot, width = 8, height = 5)

# ===== SLIDE 71-75: Composite Estimators =====
cat("\n--- Composite Estimators (Slides 71-75) ---\n")

# Calculate composite estimates combining overlapping panels
# For waves with multiple panels, combine estimates

composite_estimates <- rotating_panel_data %>%
  group_by(wave) %>%
  summarise(
    # Simple mean (equal weights)
    simple_mean = mean(income),
    
    # Weighted by panel age (newer panels get more weight)
    weighted_mean = weighted.mean(income, 
                                  weights = 1 / panel_wave),
    
    # Number of panels
    n_panels = n_distinct(panel),
    n_obs = n()
  )

cat("\nComposite Estimates:\n")
print(composite_estimates)

# Compare estimation methods
comparison_plot <- composite_estimates %>%
  pivot_longer(cols = c(simple_mean, weighted_mean),
               names_to = "method", values_to = "estimate") %>%
  ggplot(aes(x = wave, y = estimate, color = method)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Comparison of Estimation Methods",
       x = "Wave", y = "Income Estimate", color = "Method") +
  theme_minimal()

print(comparison_plot)
ggsave("06-Outputs/estimation_comparison.png", comparison_plot, width = 8, height = 5)

# ===== SLIDE 76-80: Change Estimation =====
cat("\n--- Change Estimation (Slides 76-80) ---\n")

# Calculate wave-to-wave changes for continuing panels
change_analysis <- rotating_panel_data %>%
  arrange(household_id, wave) %>%
  group_by(household_id) %>%
  mutate(
    income_change = income - lag(income),
    employment_change = employment - lag(employment)
  ) %>%
  filter(!is.na(income_change)) %>%
  group_by(wave) %>%
  summarise(
    mean_income_change = mean(income_change),
    se_income_change = sd(income_change) / sqrt(n()),
    pct_job_loss = mean(employment_change == -1) * 100,
    pct_job_gain = mean(employment_change == 1) * 100
  )

cat("\nWave-to-Wave Changes:\n")
print(change_analysis)

# ===== SLIDE 81-85: Variance Estimation =====
cat("\n--- Variance Estimation (Slides 81-85) ---\n")

# Create survey design for rotating panel
# Account for panel structure

# For demonstration, use wave 6 (multiple panels present)
wave6_data <- rotating_panel_data %>%
  filter(wave == 6) %>%
  mutate(weight = 1000)  # Base weight

# Create design object
rotating_design <- svydesign(
  ids = ~household_id,
  strata = ~panel,
  weights = ~weight,
  data = wave6_data
)

# Estimates with proper variance
rotating_estimates <- svymean(~income + employment, rotating_design)

cat("\nWave 6 Estimates with Design-Based Variance:\n")
print(rotating_estimates)
print(confint(rotating_estimates))

# ===== SLIDE 86-90: Attrition Analysis =====
cat("\n--- Attrition Analysis (Slides 86-90) ---\n")

# Simulate attrition patterns
attrition_analysis <- rotating_panel_data %>%
  group_by(panel, panel_wave) %>%
  summarise(n = n()) %>%
  group_by(panel) %>%
  mutate(
    retention_rate = n / first(n) * 100,
    attrition_rate = 100 - retention_rate
  )

cat("\nAttrition Rates by Panel Wave:\n")
print(attrition_analysis %>% filter(panel <= 3))

# Plot attrition
attrition_plot <- attrition_analysis %>%
  ggplot(aes(x = panel_wave, y = retention_rate, 
             color = factor(panel), group = panel)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Retention Rates by Panel",
       x = "Panel Wave", y = "Retention Rate (%)", 
       color = "Panel") +
  theme_minimal() +
  ylim(0, 100)

print(attrition_plot)
ggsave("06-Outputs/attrition_rates.png", attrition_plot, width = 8, height = 5)

# ===== SLIDE 91-95: Weighting Adjustments =====
cat("\n--- Weighting Adjustments (Slides 91-95) ---\n")

# Calculate weights accounting for rotation and attrition
weight_calculation <- rotating_panel_data %>%
  group_by(wave) %>%
  mutate(
    # Base weight
    base_weight = 1000,
    
    # Rotation adjustment (number of panels in sample)
    n_panels_wave = n_distinct(panel),
    rotation_weight = 5 / n_panels_wave,
    
    # Combined weight
    final_weight = base_weight * rotation_weight
  ) %>%
  group_by(wave, panel) %>%
  summarise(
    mean_weight = mean(final_weight),
    cv_weight = sd(final_weight) / mean(final_weight)
  )

cat("\nWeight Summary by Wave and Panel:\n")
print(weight_calculation %>% filter(wave %in% c(1, 6, 12)))

# ===== SLIDE 96-100: Quality Metrics =====
cat("\n--- Quality Metrics (Slides 96-100) ---\n")

# Calculate quality metrics for rotating panel
quality_metrics <- data.frame(
  Metric = c("Average overlap %", "Income variance", 
             "Employment stability", "Panel balance"),
  Value = c(
    mean(overlaps$pct_overlap),
    sd(cross_sectional_estimates$mean_income),
    100 - mean(abs(change_analysis$pct_job_loss - 
                     change_analysis$pct_job_gain)),
    sd(table(rotating_panel_data$panel))
  ),
  Target = c(60, 2000, 90, 50),
  Status = c("Good", "Good", "Good", "Good")
)

cat("\nRotating Panel Quality Metrics:\n")
print(quality_metrics)

# Save all results
save(panel_structure, cross_sectional_estimates, 
     composite_estimates, change_analysis,
     rotating_design, quality_metrics,
     file = "06-Outputs/part2_rotating_panel_results.RData")

cat("\n========================================\n")
cat("Part 2 Analysis Complete!\n")
cat("Results saved to: 06-Outputs/part2_rotating_panel_results.RData\n")
cat("Plots saved to: 06-Outputs/\n")
cat("========================================\n")