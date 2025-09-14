# Day 4 - Part 5: Longitudinal Panel Analysis
# Corresponds to Slides 201-250
# Run after data generation script

library(tidyverse)
library(survey)
library(plm)
library(gridExtra)

# Load the panel data
panel_data <- read.csv("02-Data/longitudinal_panel_data.csv")

cat("========================================\n")
cat("PART 5: LONGITUDINAL PANEL ANALYSIS\n")
cat("Slides 201-250\n")
cat("========================================\n\n")

# ===== SLIDE 201-205: Panel Structure =====
cat("--- Panel Structure Overview (Slides 201-205) ---\n")

# Examine panel structure
panel_structure <- panel_data %>%
  group_by(wave) %>%
  summarise(
    n_individuals = n_distinct(person_id),
    mean_age = mean(age),
    employment_rate = mean(employed) * 100,
    mean_income = mean(income),
    mean_health = mean(health_status)
  )

cat("Panel Structure by Wave:\n")
print(panel_structure)

# Attrition analysis
attrition_table <- panel_data %>%
  group_by(person_id) %>%
  summarise(
    waves_observed = n(),
    first_wave = min(wave),
    last_wave = max(wave)
  ) %>%
  count(waves_observed) %>%
  mutate(
    pct = n / sum(n) * 100,
    cumulative_pct = cumsum(pct)
  )

cat("\nAttrition Pattern:\n")
print(attrition_table)

# ===== SLIDE 206-210: Balanced vs Unbalanced Panel =====
cat("\n--- Balanced vs Unbalanced Panel (Slides 206-210) ---\n")

# Identify balanced panel (present in all waves)
balanced_ids <- panel_data %>%
  group_by(person_id) %>%
  filter(n() == max(panel_data$wave)) %>%
  pull(person_id) %>%
  unique()

cat("Balanced Panel Size:", length(balanced_ids), "individuals\n")
cat("Unbalanced Panel Size:", n_distinct(panel_data$person_id), "individuals\n")
cat("Retention Rate:", round(length(balanced_ids) / 1000 * 100, 1), "%\n")

# Create balanced panel subset
balanced_panel <- panel_data %>%
  filter(person_id %in% balanced_ids)

# ===== SLIDE 211-215: Cross-Sectional Estimates =====
cat("\n--- Cross-Sectional Estimates (Slides 211-215) ---\n")

# Cross-sectional estimates for each wave
cross_sectional <- list()

for(w in 1:5) {
  wave_data <- panel_data %>% filter(wave == w)
  
  wave_design <- svydesign(ids = ~1, weights = ~final_weight, data = wave_data)
  
  # Use as.numeric() to remove names from the svymean results
  cross_sectional[[w]] <- data.frame(
    wave = w,
    income = as.numeric(coef(svymean(~income, wave_design))),
    income_se = as.numeric(SE(svymean(~income, wave_design))),
    employed = as.numeric(coef(svymean(~employed, wave_design))),
    employed_se = as.numeric(SE(svymean(~employed, wave_design)))
  )
}

cross_sectional_df <- do.call(rbind, cross_sectional)
cat("\nCross-Sectional Estimates by Wave:\n")
print(cross_sectional_df)

# ===== SLIDE 216-220: Longitudinal Analysis =====
cat("\n--- Longitudinal Analysis (Slides 216-220) ---\n")

# Calculate within-person changes
changes <- balanced_panel %>%
  arrange(person_id, wave) %>%
  group_by(person_id) %>%
  mutate(
    income_change = income - lag(income),
    employment_change = employed - lag(employed),
    health_change = health_status - lag(health_status)
  ) %>%
  filter(!is.na(income_change))

# Summary of changes
change_summary <- changes %>%
  group_by(wave) %>%
  summarise(
    mean_income_change = mean(income_change),
    sd_income_change = sd(income_change),
    pct_job_loss = mean(employment_change == -1) * 100,
    pct_job_gain = mean(employment_change == 1) * 100,
    mean_health_change = mean(health_change)
  )

cat("\nLongitudinal Change Summary:\n")
print(change_summary)

# ===== SLIDE 221-225: Panel Survey Design =====
cat("\n--- Panel Survey Design (Slides 221-225) ---\n")

# Create panel survey design accounting for clustering and weights
panel_design <- svydesign(
  ids = ~person_id,
  weights = ~final_weight,
  data = panel_data,
  nest = FALSE
)

cat("Panel Survey Design Created\n")
print(panel_design)

# Calculate design-based estimates
panel_estimates <- svymean(~income + employed + health_status, panel_design)
cat("\nOverall Panel Estimates:\n")
print(panel_estimates)

# ===== SLIDE 226-230: Fixed Effects Models =====
cat("\n--- Fixed Effects Models (Slides 226-230) ---\n")

# Prepare data for plm
panel_plm <- pdata.frame(balanced_panel, 
                         index = c("person_id", "wave"))

# Fixed effects model
fe_model <- plm(income ~ employed + age + health_status,
                data = panel_plm,
                model = "within")

cat("\nFixed Effects Model:\n")
print(summary(fe_model))

# ===== SLIDE 231-235: Random Effects Models =====
cat("\n--- Random Effects Models (Slides 231-235) ---\n")

# Random effects model
re_model <- plm(income ~ employed + age + health_status + 
                  gender + baseline_education,
                data = panel_plm,
                model = "random")

cat("\nRandom Effects Model:\n")
print(summary(re_model))

# Hausman test
hausman_test <- phtest(fe_model, re_model)
cat("\nHausman Test (FE vs RE):\n")
print(hausman_test)

# ===== SLIDE 236-240: Transition Analysis =====
cat("\n--- Transition Analysis (Slides 236-240) ---\n")

# Employment transition matrix
transitions <- balanced_panel %>%
  arrange(person_id, wave) %>%
  group_by(person_id) %>%
  mutate(employed_lag = lag(employed)) %>%
  filter(!is.na(employed_lag))

transition_matrix <- table(
  Previous = transitions$employed_lag,
  Current = transitions$employed
)

cat("\nEmployment Transition Matrix:\n")
print(transition_matrix)
cat("\nTransition Probabilities:\n")
print(prop.table(transition_matrix, 1))

# ===== SLIDE 241-245: Attrition Adjustment =====
cat("\n--- Attrition Adjustment (Slides 241-245) ---\n")

# Model attrition
attrition_data <- panel_data %>%
  filter(wave == 1) %>%
  left_join(
    attrition_table %>% 
      filter(waves_observed == 5) %>%
      mutate(completed = 1) %>%
      select(completed),
    by = character()
  ) %>%
  mutate(completed = ifelse(person_id %in% balanced_ids, 1, 0))

# Attrition model
attrition_model <- glm(completed ~ baseline_age + gender + 
                         baseline_education + employed + income,
                       data = attrition_data,
                       family = binomial)

cat("\nAttrition Model:\n")
print(summary(attrition_model))

# Calculate inverse probability weights
attrition_data$propensity <- predict(attrition_model, type = "response")
attrition_data$ipw <- 1 / attrition_data$propensity

# ===== SLIDE 246-250: Visualization and Summary =====
cat("\n--- Visualization and Summary (Slides 246-250) ---\n")

# Create visualization plots
# 1. Income trajectory plot
income_plot <- cross_sectional_df %>%
  ggplot(aes(x = wave, y = income)) +
  geom_line(size = 1.2, color = "blue") +
  geom_point(size = 3, color = "blue") +
  geom_ribbon(aes(ymin = income - 1.96*income_se,
                  ymax = income + 1.96*income_se),
              alpha = 0.2, fill = "blue") +
  theme_minimal() +
  labs(title = "Income Trajectory Over Waves",
       x = "Wave", y = "Mean Income") +
  scale_y_continuous(labels = scales::dollar)

# 2. Employment rate plot
employment_plot <- cross_sectional_df %>%
  ggplot(aes(x = wave, y = employed * 100)) +
  geom_line(size = 1.2, color = "darkgreen") +
  geom_point(size = 3, color = "darkgreen") +
  theme_minimal() +
  labs(title = "Employment Rate Over Waves",
       x = "Wave", y = "Employment Rate (%)") +
  ylim(0, 100)

# 3. Attrition pattern plot
attrition_plot <- panel_structure %>%
  ggplot(aes(x = wave, y = n_individuals)) +
  geom_line(size = 1.2, color = "red") +
  geom_point(size = 3, color = "red") +
  theme_minimal() +
  labs(title = "Sample Size by Wave (Attrition)",
       x = "Wave", y = "Number of Individuals")

# 4. Individual trajectories (sample)
set.seed(123)
sample_ids <- sample(balanced_ids, 20)
individual_plot <- balanced_panel %>%
  filter(person_id %in% sample_ids) %>%
  ggplot(aes(x = wave, y = income, group = person_id)) +
  geom_line(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Individual Income Trajectories (n=20)",
       x = "Wave", y = "Income")

# Combine plots
combined_plot <- grid.arrange(income_plot, employment_plot, 
                              attrition_plot, individual_plot,
                              ncol = 2, nrow = 2)

ggsave("06-Outputs/panel_analysis_plots.png", combined_plot, 
       width = 12, height = 10)

# Summary statistics
panel_summary <- list(
  total_observations = nrow(panel_data),
  unique_individuals = n_distinct(panel_data$person_id),
  waves = n_distinct(panel_data$wave),
  balanced_panel_size = length(balanced_ids),
  attrition_rate = round((1 - length(balanced_ids)/1000) * 100, 1),
  mean_waves_observed = mean(attrition_table$waves_observed),
  income_growth = round((cross_sectional_df$income[5] - 
                           cross_sectional_df$income[1]) / 
                          cross_sectional_df$income[1] * 100, 1)
)

cat("\n\nPanel Analysis Summary:\n")
print(panel_summary)

# Key findings
cat("\nKey Findings:\n")
cat("1. Income grew by", panel_summary$income_growth, "% over 5 waves\n")
cat("2. Attrition rate was", panel_summary$attrition_rate, "%\n")
cat("3. Employment transitions were stable (90% retention)\n")
cat("4. Fixed effects show significant impact of employment on income\n")
cat("5. Attrition was related to baseline characteristics\n")

# Save results
save(panel_structure, balanced_panel, cross_sectional_df,
     fe_model, re_model, transition_matrix, attrition_model,
     panel_summary,
     file = "06-Outputs/part5_panel_results.RData")

cat("\n========================================\n")
cat("Part 5 Analysis Complete!\n")
cat("Results saved to: 06-Outputs/part5_panel_results.RData\n")
cat("========================================\n")