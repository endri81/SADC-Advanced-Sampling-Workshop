# Day 4: Generate All Datasets for Complex Survey Designs
# This script creates all datasets needed for Day 4 exercises
# Run after 00-Setup

# Load setup if not already loaded
if(!exists("workshop_colors")) {
  source("00-Setup/setup_complete.R")
}

set.seed(2024)

# ===== DATASET 1: Multi-Phase Survey Data =====
# For slides on multi-phase sampling (Part 1)

generate_multiphase_data <- function(n = 10000) {
  # Phase 1: Large screening sample
  phase1 <- data.frame(
    id = 1:n,
    stratum = sample(c("Urban", "Rural"), n, replace = TRUE, prob = c(0.4, 0.6)),
    region = sample(c("North", "South", "East", "West"), n, replace = TRUE),
    age_group = sample(c("18-29", "30-44", "45-59", "60+"), n, replace = TRUE),
    employed = rbinom(n, 1, 0.65),
    chronic_disease = rbinom(n, 1, 0.25)
  )
  
  # Phase 2: Subsample for detailed survey
  phase2_prob <- ifelse(phase1$chronic_disease == 1, 0.6, 0.2)
  phase1$selected_phase2 <- rbinom(n, 1, phase2_prob)
  
  # Add detailed variables for phase 2
  phase2_indices <- which(phase1$selected_phase2 == 1)
  phase1$healthcare_cost <- NA
  phase1$quality_of_life <- NA
  phase1$medications <- NA
  
  phase1$healthcare_cost[phase2_indices] <- round(
    rlnorm(length(phase2_indices), 
           log(5000 + 10000 * phase1$chronic_disease[phase2_indices]), 0.5)
  )
  
  phase1$quality_of_life[phase2_indices] <- round(
    pmin(100, pmax(0, rnorm(length(phase2_indices), 
                            70 - 20 * phase1$chronic_disease[phase2_indices], 15)))
  )
  
  phase1$medications[phase2_indices] <- rpois(
    length(phase2_indices), 
    1 + 3 * phase1$chronic_disease[phase2_indices]
  )
  
  # Add weights
  phase1$phase1_weight <- n / nrow(phase1)
  phase1$phase2_weight <- NA
  phase1$phase2_weight[phase2_indices] <- 1 / phase2_prob[phase2_indices]
  
  return(phase1)
}

multiphase_data <- generate_multiphase_data(10000)
write.csv(multiphase_data, "02-Data/multiphase_data.csv", row.names = FALSE)
cat("Generated: multiphase_data.csv (n=10000)\n")

# ===== DATASET 2: Rotating Panel Data =====
# For slides on rotating panel surveys (Part 2)

generate_rotating_panel <- function(n_panels = 5, panel_size = 1000, n_waves = 12) {
  
  all_data <- list()
  
  for(wave in 1:n_waves) {
    # Determine which panels are in sample
    panels_in_wave <- ((wave - 1) %% n_panels + 1):min(wave, n_panels)
    if(wave > n_panels) {
      panels_in_wave <- ((wave - n_panels):(wave - 1)) %% n_panels + 1
    }
    
    wave_data <- data.frame()
    
    for(panel in panels_in_wave) {
      panel_wave <- wave - panel + 1
      if(panel_wave > 0 && panel_wave <= n_panels) {
        panel_data <- data.frame(
          household_id = paste0("P", panel, "_H", 1:panel_size),
          panel = panel,
          wave = wave,
          panel_wave = panel_wave,
          income = round(rlnorm(panel_size, log(50000), 0.4) * 
                           (1 + 0.02)^panel_wave),  # Income growth
          employment = rbinom(panel_size, 1, 0.7 - 0.02 * panel_wave),  # Slight decline
          expenditure = round(rlnorm(panel_size, log(40000), 0.3) * 
                                (1 + 0.03)^panel_wave)
        )
        wave_data <- rbind(wave_data, panel_data)
      }
    }
    
    all_data[[wave]] <- wave_data
  }
  
  rotating_panel <- do.call(rbind, all_data)
  return(rotating_panel)
}

rotating_panel_data <- generate_rotating_panel()
write.csv(rotating_panel_data, "02-Data/rotating_panel_data.csv", row.names = FALSE)
cat("Generated: rotating_panel_data.csv (5 panels, 12 waves)\n")

# ===== DATASET 3: Complex Stratified Multi-Stage Data =====
# For slides on combined techniques (Part 3)

generate_complex_design <- function() {
  # Create PSU frame with complex stratification
  n_psu <- 500
  
  psu_frame <- data.frame(
    psu_id = 1:n_psu,
    province = sample(c("P1", "P2", "P3", "P4", "P5"), n_psu, 
                      replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.15, 0.1)),
    urban_rural = sample(c("Urban", "Rural"), n_psu, 
                         replace = TRUE, prob = c(0.35, 0.65)),
    economic_zone = sample(c("Industrial", "Agricultural", "Service", "Mixed"), 
                           n_psu, replace = TRUE)
  )
  
  # Create explicit strata
  psu_frame$stratum <- paste(psu_frame$province, psu_frame$urban_rural, sep = "_")
  
  # Assign PSU sizes based on characteristics
  psu_frame$size <- ifelse(
    psu_frame$urban_rural == "Urban",
    round(rlnorm(sum(psu_frame$urban_rural == "Urban"), log(1000), 0.4)),
    round(rlnorm(sum(psu_frame$urban_rural == "Rural"), log(500), 0.5))
  )
  
  # Select PSUs using PPS within strata
  selected_psus <- psu_frame %>%
    group_by(stratum) %>%
    sample_n(size = pmin(n(), 5), weight = size) %>%
    ungroup()
  
  # Generate household data within selected PSUs
  household_data <- data.frame()
  
  for(i in 1:nrow(selected_psus)) {
    psu <- selected_psus[i,]
    n_hh <- min(30, round(psu$size * 0.1))  # Sample 10% or max 30
    
    hh_data <- data.frame(
      psu_id = psu$psu_id,
      household_id = 1:n_hh,
      stratum = psu$stratum,
      province = psu$province,
      urban_rural = psu$urban_rural,
      economic_zone = psu$economic_zone,
      household_size = rpois(n_hh, 4),
      income = round(rlnorm(n_hh, 
                            log(40000 + 20000 * (psu$urban_rural == "Urban")), 
                            0.5)),
      education_head = sample(0:16, n_hh, replace = TRUE,
                              prob = dnorm(0:16, mean = 10, sd = 3)),
      has_electricity = rbinom(n_hh, 1, 
                               ifelse(psu$urban_rural == "Urban", 0.95, 0.70)),
      has_internet = rbinom(n_hh, 1, 
                            ifelse(psu$urban_rural == "Urban", 0.70, 0.30))
    )
    
    household_data <- rbind(household_data, hh_data)
  }
  
  # Calculate weights
  household_data <- household_data %>%
    left_join(selected_psus %>% select(psu_id, size), by = "psu_id") %>%
    group_by(stratum) %>%
    mutate(
      n_psu_stratum = n_distinct(psu_id),
      N_psu_stratum = sum(psu_frame$stratum == first(stratum))
    ) %>%
    ungroup() %>%
    mutate(
      psu_weight = N_psu_stratum / n_psu_stratum,
      hh_weight = size / n(),
      final_weight = psu_weight * hh_weight
    )
  
  return(list(psu_frame = psu_frame, 
              selected_psus = selected_psus,
              household_data = household_data))
}

complex_design <- generate_complex_design()
write.csv(complex_design$psu_frame, "02-Data/complex_psu_frame.csv", row.names = FALSE)
write.csv(complex_design$selected_psus, "02-Data/complex_selected_psus.csv", row.names = FALSE)
write.csv(complex_design$household_data, "02-Data/complex_household_data.csv", row.names = FALSE)
cat("Generated: complex_psu_frame.csv, complex_selected_psus.csv, complex_household_data.csv\n")

# ===== DATASET 4: Domain Estimation Data =====
# For slides on domain/small area estimation (Part 4)

generate_domain_data <- function(n = 5000) {
  # Generate data with multiple overlapping domains
  
  domain_data <- data.frame(
    id = 1:n,
    province = sample(paste0("Prov", 1:10), n, replace = TRUE),
    district = sample(paste0("Dist", 1:30), n, replace = TRUE),
    urban_rural = sample(c("Urban", "Rural"), n, replace = TRUE, prob = c(0.4, 0.6)),
    age_group = sample(c("0-14", "15-24", "25-44", "45-64", "65+"), n, replace = TRUE),
    gender = sample(c("Male", "Female"), n, replace = TRUE),
    education_level = sample(c("None", "Primary", "Secondary", "Tertiary"), n, 
                             replace = TRUE, prob = c(0.1, 0.3, 0.4, 0.2))
  )
  
  # Add survey variables correlated with domains
  domain_data$employed <- rbinom(n, 1, 
                                 0.3 + 0.2 * (domain_data$urban_rural == "Urban") + 
                                   0.1 * (domain_data$education_level == "Tertiary") -
                                   0.15 * (domain_data$age_group == "65+"))
  
  domain_data$health_insurance <- rbinom(n, 1,
                                         0.2 + 0.3 * (domain_data$urban_rural == "Urban") +
                                           0.2 * (domain_data$employed == 1))
  
  domain_data$income <- round(rlnorm(n, 
                                     log(20000 + 
                                           15000 * (domain_data$urban_rural == "Urban") +
                                           10000 * (domain_data$education_level == "Secondary") +
                                           25000 * (domain_data$education_level == "Tertiary") +
                                           20000 * domain_data$employed), 
                                     0.6))
  
  # Add complex survey design variables
  domain_data$stratum <- paste(domain_data$province, domain_data$urban_rural, sep = "_")
  domain_data$psu <- paste(domain_data$stratum, 
                           sample(1:20, n, replace = TRUE), sep = "_")
  
  # Create sampling weights
  strata_sizes <- table(domain_data$stratum)
  domain_data$weight <- 100  # Base weight
  
  # Adjust weights by stratum
  for(s in names(strata_sizes)) {
    domain_data$weight[domain_data$stratum == s] <- 
      domain_data$weight[domain_data$stratum == s] * 
      (1 + rnorm(1, 0, 0.2))
  }
  
  return(domain_data)
}

domain_data <- generate_domain_data(5000)
write.csv(domain_data, "02-Data/domain_estimation_data.csv", row.names = FALSE)
cat("Generated: domain_estimation_data.csv (n=5000)\n")

# ===== DATASET 5: Longitudinal Panel Data =====
# For slides on panel survey analysis (Part 5)

generate_panel_data <- function(n_individuals = 1000, n_waves = 5) {
  
  panel_data <- data.frame()
  
  # Generate baseline characteristics
  individuals <- data.frame(
    person_id = 1:n_individuals,
    gender = sample(c("Male", "Female"), n_individuals, replace = TRUE),
    baseline_age = sample(18:65, n_individuals, replace = TRUE),
    baseline_education = sample(0:20, n_individuals, replace = TRUE),
    region = sample(c("North", "South", "East", "West"), n_individuals, replace = TRUE)
  )
  
  # Track which individuals remain in sample
  active_individuals <- individuals
  
  # Generate panel waves
  for(wave in 1:n_waves) {
    
    # Add attrition before processing (except wave 1)
    if(wave > 1) {
      attrition_prob <- 0.05 * (wave - 1)  # 5% per wave cumulative
      keep <- rbinom(nrow(active_individuals), 1, 1 - attrition_prob)
      active_individuals <- active_individuals[keep == 1, ]
    }
    
    n_active <- nrow(active_individuals)
    
    # Create wave data for active individuals
    wave_data <- active_individuals
    wave_data$wave <- wave
    wave_data$year <- 2020 + wave - 1
    wave_data$age <- wave_data$baseline_age + wave - 1
    
    # Time-varying variables with correlation across waves
    if(wave == 1) {
      wave_data$employed <- rbinom(n_active, 1, 
                                   0.5 + 0.02 * wave_data$baseline_education)
      wave_data$income <- round(rlnorm(n_active, 
                                       log(30000 + 2000 * wave_data$baseline_education), 
                                       0.5))
      wave_data$health_status <- pmin(100, pmax(0, 
                                                rnorm(n_active, 80 - wave_data$age/2, 15)))
    } else {
      # Get previous wave data for these individuals
      prev_wave <- panel_data[panel_data$wave == wave - 1 & 
                                panel_data$person_id %in% wave_data$person_id, ]
      
      # Match order
      prev_wave <- prev_wave[match(wave_data$person_id, prev_wave$person_id), ]
      
      # Employment transitions
      wave_data$employed <- ifelse(
        prev_wave$employed == 1,
        rbinom(n_active, 1, 0.9),  # 90% stay employed
        rbinom(n_active, 1, 0.3)   # 30% find employment
      )
      
      # Income growth/change
      wave_data$income <- round(prev_wave$income * 
                                  (1 + rnorm(n_active, 0.03, 0.1)) *
                                  ifelse(wave_data$employed == 1, 1, 0.6))
      
      # Health deterioration
      wave_data$health_status <- pmin(100, pmax(0,
                                                prev_wave$health_status + 
                                                  rnorm(n_active, -1, 5)))
    }
    
    panel_data <- rbind(panel_data, wave_data)
  }
  
  # Add survey weights
  panel_data$base_weight <- 1000
  panel_data$attrition_weight <- 1
  
  # Adjust for attrition
  for(wave in 2:n_waves) {
    wave_ids <- unique(panel_data$person_id[panel_data$wave == wave])
    n_wave <- length(wave_ids)
    n_baseline <- n_individuals
    panel_data$attrition_weight[panel_data$wave == wave] <- n_baseline / n_wave
  }
  
  panel_data$final_weight <- panel_data$base_weight * panel_data$attrition_weight
  
  return(panel_data)
}

panel_data <- generate_panel_data(1000, 5)
write.csv(panel_data, "02-Data/longitudinal_panel_data.csv", row.names = FALSE)
cat("Generated: longitudinal_panel_data.csv (1000 individuals, 5 waves)\n")

# ===== DATASET 6: Mixed-Mode Survey Data =====
# For slides on mixed-mode designs

generate_mixed_mode <- function(n = 3000) {
  
  mixed_mode <- data.frame(
    respondent_id = 1:n,
    age = sample(18:80, n, replace = TRUE),
    urban_rural = sample(c("Urban", "Rural"), n, replace = TRUE, prob = c(0.45, 0.55)),
    education = sample(c("Low", "Medium", "High"), n, replace = TRUE, 
                       prob = c(0.3, 0.5, 0.2))
  )
  
  # Assign mode based on characteristics
  mode_prob_web <- 0.2 + 
    0.3 * (mixed_mode$urban_rural == "Urban") +
    0.2 * (mixed_mode$education == "High") -
    0.3 * (mixed_mode$age > 60)
  
  mode_prob_phone <- 0.3 +
    0.1 * (mixed_mode$urban_rural == "Urban") -
    0.1 * (mixed_mode$age < 30)
  
  # Assign modes
  rand <- runif(n)
  mixed_mode$mode <- ifelse(rand < mode_prob_web, "Web",
                            ifelse(rand < mode_prob_web + mode_prob_phone, "Phone", "Face-to-face"))
  
  # Add mode effects on responses
  mixed_mode$satisfaction <- round(
    7 + 
      rnorm(n, 0, 1) +
      1 * (mixed_mode$mode == "Face-to-face") -  # Higher in F2F
      0.5 * (mixed_mode$mode == "Web"),  # Lower in Web
    1
  )
  
  mixed_mode$income_reported <- round(
    rlnorm(n, log(40000), 0.5) *
      ifelse(mixed_mode$mode == "Web", 1.1, 1) *  # Higher reporting in Web
      ifelse(mixed_mode$mode == "Face-to-face", 0.95, 1)  # Lower in F2F
  )
  
  # Response propensity by mode
  mixed_mode$responded <- rbinom(n, 1,
                                 ifelse(mixed_mode$mode == "Web", 0.35,
                                        ifelse(mixed_mode$mode == "Phone", 0.55, 0.75)))
  
  # Calculate mode-adjusted weights
  response_rates <- mixed_mode %>%
    group_by(mode) %>%
    summarise(rr = mean(responded)) %>%
    deframe()
  
  mixed_mode$mode_weight <- 1 / response_rates[mixed_mode$mode]
  
  return(mixed_mode)
}

mixed_mode_data <- generate_mixed_mode(3000)
write.csv(mixed_mode_data, "02-Data/mixed_mode_data.csv", row.names = FALSE)
cat("Generated: mixed_mode_data.csv (n=3000)\n")

# ===== Create Data Dictionary =====
data_dictionary <- data.frame(
  Dataset = c("multiphase_data", "rotating_panel_data", "complex_household_data",
              "domain_estimation_data", "longitudinal_panel_data", "mixed_mode_data"),
  Description = c(
    "Two-phase health survey with screening and detailed follow-up",
    "Rotating panel with 5 panels over 12 waves",
    "Complex stratified multi-stage household survey",
    "Data for domain and small area estimation",
    "5-wave panel survey with attrition",
    "Mixed-mode survey with mode effects"
  ),
  N_Records = c(10000, nrow(rotating_panel_data), nrow(complex_design$household_data),
                5000, nrow(panel_data), 3000),
  Key_Variables = c(
    "chronic_disease, healthcare_cost, phase2_weight",
    "panel, wave, panel_wave, income",
    "stratum, psu_id, household_id, final_weight",
    "province, district, domain weights",
    "person_id, wave, employed, attrition_weight",
    "mode, responded, mode_weight"
  ),
  Used_In_Slides = c(
    "1-50 (Multi-phase)",
    "51-100 (Rotating panels)",
    "101-150 (Complex designs)",
    "151-200 (Domain estimation)",
    "201-250 (Panel analysis)",
    "251-300 (Mixed modes)"
  )
)

write.csv(data_dictionary, "02-Data/data_dictionary.csv", row.names = FALSE)
cat("\nGenerated: data_dictionary.csv\n")

cat("\n=== All Day 4 datasets generated successfully! ===\n")
cat("Total datasets created: 6\n")
cat("Location: 02-Data/\n")
cat("\nReady for Day 4 exercises!\n")