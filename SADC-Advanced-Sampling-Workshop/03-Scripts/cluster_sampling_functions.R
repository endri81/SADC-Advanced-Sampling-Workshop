# ============================================================================
# Script: cluster_sampling_functions.R
# Purpose: Helper functions for cluster and multi-stage sampling
# Location: 03-Scripts/cluster_sampling_functions.R
# Author: Survey Sampling Workshop
# Date: 2024
#
# INSTRUCTIONS:
# Source this file at the beginning of Day 3 exercises
# In R: source("03-Scripts/cluster_sampling_functions.R")
# ============================================================================

# Load required packages
library(tidyverse)
library(survey)

# ============================================================================
# SECTION 1: PSU SELECTION FUNCTIONS
# ============================================================================

# Function 1: Select PSUs with PPS (Probability Proportional to Size)
select_pps <- function(psu_frame, n_psu, size_var = "size", seed = NULL) {
  # Select n_psu PSUs with probability proportional to size
  
  if (!is.null(seed)) set.seed(seed)
  
  # Calculate selection probabilities
  psu_frame$pps_prob <- psu_frame[[size_var]] / sum(psu_frame[[size_var]])
  
  # Systematic PPS selection
  interval <- 1 / n_psu
  start <- runif(1, 0, interval)
  selections <- start + (0:(n_psu - 1)) * interval
  
  # Cumulative probabilities
  psu_frame$cum_prob <- cumsum(psu_frame$pps_prob)
  
  # Select PSUs
  selected_indices <- sapply(selections, function(x) {
    which(psu_frame$cum_prob >= x)[1]
  })
  
  # Mark selected PSUs
  psu_frame$selected <- FALSE
  psu_frame$selected[selected_indices] <- TRUE
  psu_frame$psu_selection_prob <- ifelse(psu_frame$selected, 
                                         n_psu * psu_frame$pps_prob, 0)
  
  return(psu_frame)
}

# Function 2: Select PSUs with SRS
select_srs_psu <- function(psu_frame, n_psu, stratified = FALSE, 
                           strat_var = NULL, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  if (stratified && !is.null(strat_var)) {
    # Stratified selection
    selected_ids <- psu_frame %>%
      group_by(!!sym(strat_var)) %>%
      sample_n(size = n_psu / length(unique(psu_frame[[strat_var]]))) %>%
      pull(psu_id)
  } else {
    # Simple random selection
    selected_ids <- sample(psu_frame$psu_id, n_psu)
  }
  
  psu_frame$selected <- psu_frame$psu_id %in% selected_ids
  psu_frame$psu_selection_prob <- ifelse(psu_frame$selected, 
                                         n_psu / nrow(psu_frame), 0)
  
  return(psu_frame)
}

# ============================================================================
# SECTION 2: HOUSEHOLD SELECTION FUNCTIONS
# ============================================================================

# Function 3: Select households within selected PSUs
select_households <- function(data, psu_frame, n_hh_per_psu, method = "SRS") {
  
  selected_psus <- psu_frame[psu_frame$selected, ]
  
  sampled_hh <- list()
  
  for (i in 1:nrow(selected_psus)) {
    psu_id <- selected_psus$psu_id[i]
    psu_data <- data[data$psu_id == psu_id, ]
    
    if (method == "SRS") {
      # Simple random sampling within PSU
      if (nrow(psu_data) >= n_hh_per_psu) {
        selected_hh <- sample(psu_data$hh_id, n_hh_per_psu)
      } else {
        selected_hh <- psu_data$hh_id  # Take all if fewer than needed
      }
    } else if (method == "systematic") {
      # Systematic sampling within PSU
      k <- floor(nrow(psu_data) / n_hh_per_psu)
      start <- sample(1:k, 1)
      selected_hh <- psu_data$hh_id[seq(start, nrow(psu_data), by = k)][1:n_hh_per_psu]
      selected_hh <- selected_hh[!is.na(selected_hh)]
    }
    
    sampled_hh[[i]] <- data[data$hh_id %in% selected_hh, ]
    sampled_hh[[i]]$hh_selection_prob <- length(selected_hh) / nrow(psu_data)
    sampled_hh[[i]]$psu_selection_prob <- selected_psus$psu_selection_prob[i]
  }
  
  final_sample <- do.call(rbind, sampled_hh)
  
  # Calculate final weights
  final_sample$weight <- 1 / (final_sample$psu_selection_prob * 
                                final_sample$hh_selection_prob)
  
  return(final_sample)
}

# ============================================================================
# SECTION 3: ICC AND DESIGN EFFECT FUNCTIONS
# ============================================================================

# Function 4: Calculate ICC
calculate_icc <- function(data, outcome_var, cluster_var) {
  
  # Remove missing values
  data_clean <- data[!is.na(data[[outcome_var]]), ]
  
  # One-way ANOVA
  formula <- as.formula(paste(outcome_var, "~", cluster_var))
  anova_result <- aov(formula, data = data_clean)
  
  # Extract components
  ms_between <- summary(anova_result)[[1]]["Mean Sq"][1, 1]
  ms_within <- summary(anova_result)[[1]]["Mean Sq"][2, 1]
  
  # Cluster sizes
  cluster_sizes <- table(data_clean[[cluster_var]])
  k <- length(cluster_sizes)  # Number of clusters
  N <- nrow(data_clean)       # Total observations
  
  # Average cluster size (adjusted)
  n_bar <- (N - sum(cluster_sizes^2) / N) / (k - 1)
  
  # ICC calculation
  icc <- (ms_between - ms_within) / (ms_between + (n_bar - 1) * ms_within)
  
  return(list(
    icc = max(0, icc),
    ms_between = ms_between,
    ms_within = ms_within,
    n_bar = n_bar,
    k = k
  ))
}

# Function 5: Calculate design effect for cluster sampling
calculate_deff_cluster <- function(cluster_size, icc) {
  # DEFF = 1 + (m - 1) * rho
  # where m = cluster size, rho = ICC
  
  deff <- 1 + (cluster_size - 1) * icc
  
  return(deff)
}

# Function 6: Calculate effective sample size
calculate_neff <- function(n, deff) {
  return(n / deff)
}

# ============================================================================
# SECTION 4: VARIANCE ESTIMATION FUNCTIONS
# ============================================================================

# Function 7: Two-stage variance estimation
variance_two_stage <- function(sample_data, outcome_var, psu_var, weight_var) {
  
  # Create survey design object
  design <- svydesign(
    ids = as.formula(paste("~", psu_var)),
    weights = as.formula(paste("~", weight_var)),
    data = sample_data
  )
  
  # Calculate mean and variance
  formula <- as.formula(paste("~", outcome_var))
  result <- svymean(formula, design)
  
  return(list(
    estimate = as.numeric(coef(result)),
    se = as.numeric(SE(result)),
    cv = as.numeric(cv(result))
  ))
}

# Function 8: Ultimate cluster variance
ultimate_cluster_variance <- function(data, psu_var, outcome_var, weight_var = NULL) {
  
  # Group by PSU and calculate PSU totals
  psu_summary <- data %>%
    group_by(!!sym(psu_var)) %>%
    summarise(
      y_psu = if(is.null(weight_var)) {
        sum(!!sym(outcome_var), na.rm = TRUE)
      } else {
        sum(!!sym(outcome_var) * !!sym(weight_var), na.rm = TRUE)
      },
      n_psu = n(),
      .groups = 'drop'
    )
  
  # Calculate variance between PSU totals
  var_between <- var(psu_summary$y_psu)
  
  # Number of PSUs
  n_psu <- nrow(psu_summary)
  
  # Standard error
  se <- sqrt(var_between / n_psu)
  
  return(list(
    var_between = var_between,
    se = se,
    n_psu = n_psu
  ))
}

# ============================================================================
# SECTION 5: OPTIMAL ALLOCATION FUNCTIONS
# ============================================================================

# Function 9: Optimal cluster size
optimal_cluster_size <- function(c1, c2, icc, budget = NULL) {
  # c1: cost per PSU
  # c2: cost per unit within PSU
  # icc: intraclass correlation
  
  # Optimal size formula
  m_opt <- sqrt(c1 * (1 - icc) / (c2 * icc))
  
  if (!is.null(budget)) {
    # Calculate how many PSUs and units possible
    n_psu_possible <- budget / (c1 + c2 * m_opt)
    total_sample <- n_psu_possible * m_opt
    
    return(list(
      optimal_size = round(m_opt),
      n_psu = floor(n_psu_possible),
      total_sample = round(total_sample),
      total_cost = floor(n_psu_possible) * c1 + 
        round(total_sample) * c2
    ))
  }
  
  return(round(m_opt))
}

# Function 10: Allocate sample across strata for two-stage
allocate_two_stage <- function(strata_info, n_psu_total, n_hh_per_psu, 
                               method = "proportional") {
  
  if (method == "proportional") {
    # Proportional to stratum size
    strata_info$n_psu <- round(n_psu_total * 
                                 strata_info$N_h / sum(strata_info$N_h))
  } else if (method == "equal") {
    # Equal allocation
    strata_info$n_psu <- rep(n_psu_total / nrow(strata_info), nrow(strata_info))
  } else if (method == "optimal") {
    # Optimal considering variance and cost
    strata_info$alloc_factor <- strata_info$N_h * strata_info$S_h / 
      sqrt(strata_info$cost_h)
    strata_info$n_psu <- round(n_psu_total * 
                                 strata_info$alloc_factor / 
                                 sum(strata_info$alloc_factor))
  }
  
  # Ensure at least 2 PSUs per stratum
  strata_info$n_psu <- pmax(2, strata_info$n_psu)
  
  # Adjust to match total
  while(sum(strata_info$n_psu) != n_psu_total) {
    if(sum(strata_info$n_psu) > n_psu_total) {
      # Reduce from largest
      idx <- which.max(strata_info$n_psu)
      strata_info$n_psu[idx] <- strata_info$n_psu[idx] - 1
    } else {
      # Add to largest stratum
      idx <- which.max(strata_info$N_h)
      strata_info$n_psu[idx] <- strata_info$n_psu[idx] + 1
    }
  }
  
  strata_info$n_hh_per_psu <- n_hh_per_psu
  strata_info$total_sample <- strata_info$n_psu * n_hh_per_psu
  
  return(strata_info)
}

# ============================================================================
# SECTION 6: SIMULATION FUNCTIONS
# ============================================================================

# Function 11: Simulate cluster effect
simulate_cluster_effect <- function(n_sim = 100, n_clusters = 30, 
                                    cluster_size = 25, icc = 0.05) {
  
  results <- numeric(n_sim)
  
  for (i in 1:n_sim) {
    # Generate clustered data
    cluster_means <- rnorm(n_clusters, 0, sqrt(icc))
    
    data <- list()
    for (j in 1:n_clusters) {
      cluster_data <- rnorm(cluster_size, cluster_means[j], sqrt(1 - icc))
      data[[j]] <- cluster_data
    }
    
    all_data <- unlist(data)
    results[i] <- mean(all_data)
  }
  
  # Compare to SRS variance
  theoretical_var_srs <- 1 / (n_clusters * cluster_size)
  actual_var <- var(results)
  empirical_deff <- actual_var / theoretical_var_srs
  theoretical_deff <- calculate_deff_cluster(cluster_size, icc)
  
  return(list(
    empirical_deff = empirical_deff,
    theoretical_deff = theoretical_deff,
    results = results
  ))
}

# ============================================================================
# CONFIRMATION MESSAGE
# ============================================================================

cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Cluster sampling functions loaded successfully!\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Available functions:\n")
cat("  PSU Selection:\n")
cat("    - select_pps()           # PPS selection\n")
cat("    - select_srs_psu()       # SRS of PSUs\n")
cat("  Household Selection:\n")
cat("    - select_households()    # Within-PSU sampling\n")
cat("  ICC and DEFF:\n")
cat("    - calculate_icc()        # Intraclass correlation\n")
cat("    - calculate_deff_cluster() # Design effect\n")
cat("    - calculate_neff()       # Effective sample size\n")
cat("  Variance Estimation:\n")
cat("    - variance_two_stage()   # Two-stage variance\n")
cat("    - ultimate_cluster_variance() # Ultimate cluster\n")
cat("  Optimization:\n")
cat("    - optimal_cluster_size() # Optimal m\n")
cat("    - allocate_two_stage()   # Stratum allocation\n")
cat("  Simulation:\n")
cat("    - simulate_cluster_effect() # Demonstrate DEFF\n")
cat(paste(rep("=", 60), collapse = ""), "\n")