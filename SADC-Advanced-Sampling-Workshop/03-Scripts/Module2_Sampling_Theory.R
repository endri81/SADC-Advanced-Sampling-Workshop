# ===============================================================
# MODULE 2: SAMPLING THEORY AND DISTRIBUTIONS
# Save as: Day-01-Foundations/03-Scripts/Module2_Sampling_Theory.R
# ===============================================================

cat("================================================\n")
cat("MODULE 2: SAMPLING THEORY AND DISTRIBUTIONS\n")
cat("SADC Advanced Sampling Workshop - Day 1\n")
cat("================================================\n\n")

# Load required packages
library(survey)
library(ggplot2)
library(gridExtra)

# --- 2.1 Population vs Sample ---
cat("2.1 Population vs Sample Demonstration\n")
cat("--------------------------------------\n")

# Create a population
set.seed(2025)
N_pop <- 100000  # Population size

# Generate population with known parameters
population <- data.frame(
  id = 1:N_pop,
  income = rlnorm(N_pop, meanlog = 10.5, sdlog = 0.6),
  age = round(rnorm(N_pop, mean = 40, sd = 15)),
  employed = rbinom(N_pop, 1, prob = 0.65),
  region = sample(c("North", "South", "East", "West"), N_pop, 
                  replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2))
)

# Adjust for realism
population$age[population$age < 18] <- 18
population$age[population$age > 90] <- 90

# True population parameters
pop_params <- list(
  mean_income = mean(population$income),
  var_income = var(population$income),
  prop_employed = mean(population$employed),
  mean_age = mean(population$age)
)

cat("TRUE POPULATION PARAMETERS:\n")
cat("  Mean income: $", format(pop_params$mean_income, big.mark = ","), "\n")
cat("  Income std dev: $", format(sqrt(pop_params$var_income), big.mark = ","), "\n")
cat("  Employment rate:", round(pop_params$prop_employed * 100, 1), "%\n")
cat("  Mean age:", round(pop_params$mean_age, 1), "years\n\n")

# --- 2.2 Sampling Distribution Demonstration ---
cat("\n2.2 Sampling Distribution of the Mean\n")
cat("-------------------------------------\n")

# Function to demonstrate sampling distribution
demonstrate_sampling_distribution <- function(pop_data, sample_sizes, n_samples = 1000) {
  
  results <- list()
  
  for (n in sample_sizes) {
    # Generate sampling distribution
    sample_means <- replicate(n_samples, {
      sample_data <- pop_data[sample(nrow(pop_data), n), ]
      mean(sample_data$income)
    })
    
    results[[paste0("n_", n)]] <- list(
      n = n,
      means = sample_means,
      mean_of_means = mean(sample_means),
      sd_of_means = sd(sample_means),
      theoretical_se = sqrt(pop_params$var_income / n)
    )
  }
  
  return(results)
}

# Generate sampling distributions for different n
sample_sizes <- c(30, 100, 500, 1000)
sampling_dists <- demonstrate_sampling_distribution(population, sample_sizes)

# Display results
cat("Sampling Distribution Properties:\n\n")
cat(sprintf("%-10s %-15s %-15s %-15s %-15s\n", 
            "n", "Mean of Means", "SD of Means", "Theoretical SE", "Ratio"))
cat(rep("-", 70), "\n", sep = "")

for (dist in sampling_dists) {
  cat(sprintf("%-10d $%-14.0f $%-14.0f $%-14.0f %-14.3f\n",
              dist$n,
              dist$mean_of_means,
              dist$sd_of_means,
              dist$theoretical_se,
              dist$sd_of_means / dist$theoretical_se))
}

# --- 2.3 Central Limit Theorem Visualization ---
cat("\n\n2.3 Central Limit Theorem in Action\n")
cat("------------------------------------\n")

# Create plots for CLT demonstration
clt_plots <- list()

for (i in 1:length(sample_sizes)) {
  n <- sample_sizes[i]
  dist_data <- data.frame(
    means = sampling_dists[[i]]$means
  )
  
  p <- ggplot(dist_data, aes(x = means)) +
    geom_histogram(aes(y = ..density..), bins = 30, 
                   fill = "lightblue", color = "black", alpha = 0.7) +
    stat_function(fun = dnorm, 
                  args = list(mean = pop_params$mean_income,
                              sd = sqrt(pop_params$var_income / n)),
                  color = "red", size = 1.5) +
    labs(title = paste("n =", n),
         x = "Sample Mean Income",
         y = "Density") +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))
  
  clt_plots[[i]] <- p
}

# Display CLT plots
grid.arrange(grobs = clt_plots, ncol = 2, 
             top = "Central Limit Theorem: Sampling Distribution Approaches Normal")

# --- 2.4 Standard Error and Sample Size ---
cat("\n2.4 Standard Error vs Sample Size\n")
cat("----------------------------------\n")

# Calculate SE for range of sample sizes
n_range <- seq(10, 5000, by = 10)
se_values <- sqrt(pop_params$var_income / n_range)
me_95 <- 1.96 * se_values
relative_me <- me_95 / pop_params$mean_income * 100

se_data <- data.frame(
  n = n_range,
  SE = se_values,
  ME_95 = me_95,
  Relative_ME_Pct = relative_me
)

# Find key sample sizes
key_sizes <- data.frame(
  Target_ME = c(10, 5, 3, 2, 1),
  Required_n = NA
)

for (i in 1:nrow(key_sizes)) {
  idx <- which(se_data$Relative_ME_Pct <= key_sizes$Target_ME[i])[1]
  key_sizes$Required_n[i] <- se_data$n[idx]
}

cat("Sample Sizes for Target Precision:\n\n")
print(key_sizes)

# Plot SE vs n
plot(se_data$n, se_data$SE, type = "l", col = "blue", lwd = 2,
     xlab = "Sample Size (n)", ylab = "Standard Error ($)",
     main = "Standard Error Decreases with √n",
     log = "xy")
grid()

# Add reference lines
abline(h = c(1000, 500, 250), col = "gray", lty = 2)
text(3000, c(1000, 500, 250), 
     labels = c("SE = $1,000", "SE = $500", "SE = $250"),
     pos = 3, cex = 0.8)

# --- 2.5 Finite Population Correction ---
cat("\n\n2.5 Finite Population Correction Impact\n")
cat("----------------------------------------\n")

# Compare with and without FPC
fpc_comparison <- function(N, n_values) {
  results <- data.frame(
    n = n_values,
    sampling_fraction = n_values / N,
    SE_without_FPC = sqrt(pop_params$var_income / n_values),
    FPC_factor = sqrt((N - n_values) / (N - 1)),
    SE_with_FPC = sqrt(pop_params$var_income / n_values) * sqrt((N - n_values) / (N - 1)),
    Reduction_Pct = NA
  )
  
  results$Reduction_Pct <- (1 - results$SE_with_FPC / results$SE_without_FPC) * 100
  
  return(results)
}

# Calculate for different sampling fractions
N_finite <- 10000
n_values <- c(100, 500, 1000, 2000, 5000, 8000)
fpc_results <- fpc_comparison(N_finite, n_values)

cat("Finite Population Correction (N =", N_finite, "):\n\n")
print(round(fpc_results, 2))

# --- 2.6 Bias vs Variance Trade-off ---
cat("\n\n2.6 Bias and Variance in Sampling\n")
cat("----------------------------------\n")

# Simulate biased vs unbiased sampling
simulate_bias_variance <- function(n_sims = 1000) {
  n <- 100
  
  # Unbiased sampling (SRS)
  unbiased_means <- replicate(n_sims, {
    mean(sample(population$income, n))
  })
  
  # Biased sampling (convenience - higher income more likely)
  biased_means <- replicate(n_sims, {
    # Create selection probabilities proportional to income
    probs <- population$income / sum(population$income)
    sample_idx <- sample(1:N_pop, n, prob = probs, replace = FALSE)
    mean(population$income[sample_idx])
  })
  
  results <- list(
    unbiased = list(
      mean = mean(unbiased_means),
      variance = var(unbiased_means),
      bias = mean(unbiased_means) - pop_params$mean_income,
      mse = var(unbiased_means) + (mean(unbiased_means) - pop_params$mean_income)^2
    ),
    biased = list(
      mean = mean(biased_means),
      variance = var(biased_means),
      bias = mean(biased_means) - pop_params$mean_income,
      mse = var(biased_means) + (mean(biased_means) - pop_params$mean_income)^2
    )
  )
  
  return(results)
}

bias_var_results <- simulate_bias_variance()

cat("Unbiased (SRS) Sampling:\n")
cat("  Mean of estimates: $", format(bias_var_results$unbiased$mean, big.mark = ","), "\n")
cat("  Bias: $", format(bias_var_results$unbiased$bias, big.mark = ","), "\n")
cat("  Variance:", format(bias_var_results$unbiased$variance, big.mark = ","), "\n")
cat("  MSE:", format(bias_var_results$unbiased$mse, big.mark = ","), "\n\n")

cat("Biased (Convenience) Sampling:\n")
cat("  Mean of estimates: $", format(bias_var_results$biased$mean, big.mark = ","), "\n")
cat("  Bias: $", format(bias_var_results$biased$bias, big.mark = ","), "\n")
cat("  Variance:", format(bias_var_results$biased$variance, big.mark = ","), "\n")
cat("  MSE:", format(bias_var_results$biased$mse, big.mark = ","), "\n\n")

# --- 2.7 Save Module Results ---
cat("\n2.7 Saving Module 2 Results\n")
cat("---------------------------\n")

# Save workspace
save(population, pop_params, sampling_dists, se_data, fpc_results, bias_var_results,
     file = "Day-01-Foundations/Module2_workspace.RData")

cat("Module 2 workspace saved successfully!\n")
cat("\n========== MODULE 2 COMPLETE ==========\n")