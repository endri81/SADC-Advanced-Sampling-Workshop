# ============================================================================
# Script: sampling_functions.R
# Purpose: Helper functions for stratified sampling exercises
# Location: 03-Scripts/sampling_functions.R
# Author: Survey Sampling Workshop
# Date: 2024
#
# INSTRUCTIONS:
# Source this file at the beginning of Day 2 exercises
# In R: source("03-Scripts/sampling_functions.R")
# ============================================================================

# Function 1: Calculate stratum statistics -----------------------------------
calculate_stratum_stats <- function(data, strat_var, target_var) {
  # Calculate mean, variance, and CV by stratum
  # Used for allocation decisions
  
  result <- data %>%
    group_by({{strat_var}}) %>%
    summarise(
      N_h = n(),                           # Stratum size
      mean_h = mean({{target_var}}, na.rm = TRUE),  # Stratum mean
      var_h = var({{target_var}}, na.rm = TRUE),    # Stratum variance
      sd_h = sd({{target_var}}, na.rm = TRUE),      # Stratum SD
      cv_h = sd_h / mean_h * 100,                   # Coefficient of variation
      .groups = 'drop'
    ) %>%
    mutate(
      W_h = N_h / sum(N_h),  # Stratum weight
      stratum_id = row_number()
    )
  
  return(result)
}

# Function 2: Proportional allocation -----------------------------------------
proportional_allocation <- function(strata_stats, n_total) {
  # Allocate sample proportional to stratum size
  # Input: strata_stats from calculate_stratum_stats()
  # Output: allocation table with n_h for each stratum
  
  alloc <- strata_stats %>%
    mutate(
      n_h_exact = n_total * W_h,
      n_h = round(n_h_exact),
      sampling_fraction = n_h / N_h
    )
  
  # Adjust for rounding to ensure sum equals n_total
  diff <- n_total - sum(alloc$n_h)
  if (diff != 0) {
    # Add/subtract from largest stratum
    idx <- which.max(alloc$N_h)
    alloc$n_h[idx] <- alloc$n_h[idx] + diff
  }
  
  return(alloc)
}

# Function 3: Optimal (Neyman) allocation ------------------------------------
neyman_allocation <- function(strata_stats, n_total, cost_equal = TRUE) {
  # Neyman optimal allocation
  # Minimizes variance for fixed sample size
  
  if (cost_equal) {
    # Equal costs: allocate proportional to N_h * S_h
    alloc <- strata_stats %>%
      mutate(
        allocation_factor = N_h * sd_h,
        n_h_exact = n_total * allocation_factor / sum(allocation_factor),
        n_h = round(n_h_exact),
        sampling_fraction = n_h / N_h
      )
  } else {
    stop("Unequal costs not yet implemented")
  }
  
  # Adjust for rounding
  diff <- n_total - sum(alloc$n_h)
  if (diff != 0) {
    idx <- which.max(alloc$allocation_factor)
    alloc$n_h[idx] <- alloc$n_h[idx] + diff
  }
  
  return(alloc)
}

# Function 4: Draw stratified sample ------------------------------------------
draw_stratified_sample <- function(data, strat_var, allocation_table) {
  # Draw stratified random sample based on allocation
  # Input: data frame, stratification variable, allocation table
  # Output: sampled data with weights
  
  sample_data <- list()
  
  for (i in 1:nrow(allocation_table)) {
    stratum_name <- allocation_table[[strat_var]][i]
    n_h <- allocation_table$n_h[i]
    N_h <- allocation_table$N_h[i]
    
    # Get stratum data
    stratum_data <- data[data[[strat_var]] == stratum_name, ]
    
    # Draw sample
    if (n_h > 0 && n_h <= nrow(stratum_data)) {
      sample_idx <- sample(1:nrow(stratum_data), size = n_h, replace = FALSE)
      sample_stratum <- stratum_data[sample_idx, ]
      
      # Add design weight
      sample_stratum$design_weight <- N_h / n_h
      sample_stratum$stratum_sample_size <- n_h
      
      sample_data[[i]] <- sample_stratum
    }
  }
  
  # Combine all strata
  final_sample <- do.call(rbind, sample_data)
  
  return(final_sample)
}

# Function 5: Calculate design effect -----------------------------------------
calculate_deff <- function(data, weight_var) {
  # Calculate design effect from weights
  # DEFF = 1 + CV²(weights)
  
  weights <- data[[weight_var]]
  n <- length(weights)
  
  # Kish's approximation
  deff <- n * sum(weights^2) / (sum(weights))^2
  
  # Also calculate effective sample size
  n_eff <- n / deff
  
  result <- list(
    deff = deff,
    n_eff = n_eff,
    cv_weights = sd(weights) / mean(weights),
    min_weight = min(weights),
    max_weight = max(weights),
    weight_ratio = max(weights) / min(weights)
  )
  
  return(result)
}

# Function 6: Estimate variance for stratified design ------------------------
stratified_variance <- function(sample_data, strat_var, target_var, weight_var) {
  # Calculate variance estimate for stratified sample mean
  
  # Overall estimate
  y_bar <- weighted.mean(sample_data[[target_var]], 
                         w = sample_data[[weight_var]])
  
  # Variance calculation
  var_components <- sample_data %>%
    group_by({{strat_var}}) %>%
    summarise(
      N_h = first(N_h),
      n_h = n(),
      W_h = N_h / sum(N_h),
      s2_h = var({{target_var}}),
      fpc_h = (N_h - n_h) / N_h,  # Finite population correction
      var_h = W_h^2 * s2_h / n_h * fpc_h,
      .groups = 'drop'
    )
  
  var_y_bar <- sum(var_components$var_h)
  se_y_bar <- sqrt(var_y_bar)
  cv <- se_y_bar / y_bar * 100
  
  result <- list(
    estimate = y_bar,
    variance = var_y_bar,
    se = se_y_bar,
    cv_percent = cv,
    ci_lower = y_bar - 1.96 * se_y_bar,
    ci_upper = y_bar + 1.96 * se_y_bar,
    components = var_components
  )
  
  return(result)
}

# Function 7: Compare allocation methods --------------------------------------
compare_allocations <- function(data, strat_var, target_var, n_total) {
  # Compare equal, proportional, and optimal allocation
  
  # Calculate stratum statistics
  strata_stats <- calculate_stratum_stats(data, {{strat_var}}, {{target_var}})
  
  # Equal allocation
  equal <- strata_stats %>%
    mutate(
      n_h = rep(n_total / n(), n()),
      method = "Equal"
    )
  
  # Proportional allocation
  prop <- proportional_allocation(strata_stats, n_total) %>%
    mutate(method = "Proportional")
  
  # Neyman allocation
  neyman <- neyman_allocation(strata_stats, n_total) %>%
    mutate(method = "Neyman")
  
  # Combine results
  comparison <- bind_rows(equal, prop, neyman) %>%
    select(method, everything())
  
  return(comparison)
}

# Function 8: Post-stratification adjustment ----------------------------------
post_stratify <- function(sample_data, pop_totals, strat_var) {
  # Adjust weights for post-stratification
  # pop_totals should have columns: stratum, N_true
  
  # Calculate sample totals by stratum
  sample_totals <- sample_data %>%
    group_by({{strat_var}}) %>%
    summarise(
      n_sample = n(),
      weight_sum = sum(design_weight),
      .groups = 'drop'
    )
  
  # Merge with population totals
  adjustment <- merge(sample_totals, pop_totals, by = strat_var, all.x = TRUE)
  adjustment$ps_factor <- adjustment$N_true / adjustment$weight_sum
  
  # Apply adjustment to weights
  sample_data <- merge(sample_data, 
                       adjustment[, c(strat_var, "ps_factor")], 
                       by = strat_var)
  
  sample_data$ps_weight <- sample_data$design_weight * sample_data$ps_factor
  
  return(sample_data)
}

# Print confirmation message --------------------------------------------------
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Sampling functions loaded successfully!\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Available functions:\n")
cat("  - calculate_stratum_stats()\n")
cat("  - proportional_allocation()\n")
cat("  - neyman_allocation()\n")
cat("  - draw_stratified_sample()\n")
cat("  - calculate_deff()\n")
cat("  - stratified_variance()\n")
cat("  - compare_allocations()\n")
cat("  - post_stratify()\n")
cat(paste(rep("=", 60), collapse = ""), "\n")