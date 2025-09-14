---
title: "Advanced Sampling Methods for Household Surveys"
subtitle: "Day 1: Foundations and Simple Random Sampling"
author: |
  Dr. Endri Raço, PhD  
  Survey Methodology Expert  
  SADC Regional Statistics Project
date: "November 2025"
output:
  xaringan::moon_reader:
    css: ["default", "css/sadc-theme.css", "css/sadc-fonts.css"]
    lib_dir: libs
    nature:
      highlightStyle: github
      highlightLines: true
      countIncrementalSlides: false
      ratio: "16:9"
      slideNumberFormat: "%current%"
---

```{r setup, include=FALSE}
library(knitr)
library(xaringan)
library(ggplot2)
library(dplyr)
library(survey)
library(sampling)
library(plotly)
library(DT)

knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE,
                      fig.retina = 3, fig.align = 'center', 
                      out.width = '100%', cache = FALSE)

# Custom theme for plots
theme_sadc <- theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10))
```

class: center, middle, inverse

# Welcome to Day 1
## Foundations of Advanced Sampling Methods

### Building the Statistical Foundation for Quality Household Surveys

---

class: inverse, center, middle

# Learning Objectives - Day 1

---

## By the End of Today, You Will Be Able To:

.pull-left[
**Theoretical Understanding**
- Explain fundamental sampling theory principles
- Distinguish between probability and non-probability sampling
- Calculate sampling errors and confidence intervals
- Understand finite population corrections

**Practical Skills**
- Set up R environment for survey analysis
- Implement simple random sampling in R
- Generate sampling weights
- Conduct basic survey data analysis
]

.pull-right[
**Professional Applications**
- Design sampling strategies for household surveys
- Evaluate sampling efficiency metrics
- Apply international quality standards (Eurostat, UN)
- Document sampling procedures properly

**Statistical Software**
- Master R survey package fundamentals
- Use sampling package for design implementation
- Create reproducible sampling workflows
- Generate professional statistical reports
]

---

class: inverse, center, middle

# Module 1: Introduction to Survey Sampling
## The Foundation of Statistical Inference

---

## Why Survey Sampling Matters for SADC

.pull-left[
**Regional Challenges**
- Large, diverse populations across member states
- Limited resources for data collection
- Need for internationally comparable statistics
- Quality requirements for policy decisions

**Statistical Imperatives**
- Representative population estimates
- Efficient resource allocation
- Measurable uncertainty quantification  
- Reproducible methodologies
]

.pull-right[
**Household Survey Applications**
- Labor force surveys
- Living conditions assessments
- Demographic and health surveys
- Agricultural surveys
- Income and expenditure studies

**Quality Standards**
- Eurostat quality framework compliance
- UN Fundamental Principles adherence
- OECD statistical best practices
- World Bank LSMS methodologies
]

---

## The Evolution of Sampling Theory

```{r sampling-history, echo=FALSE, fig.height=5}
library(ggplot2)
library(dplyr)

history_data <- data.frame(
  Year = c(1895, 1906, 1928, 1934, 1944, 1950, 1953, 1977, 1990, 2000, 2010, 2020),
  Development = c("Bowley's Random Sampling", "Student's t-distribution", 
                  "Neyman's Stratification", "Neyman-Pearson Theory",
                  "Rao-Blackwell Theorem", "Horvitz-Thompson Estimator",
                  "Hansen-Hurwitz Estimator", "Kalton Survey Methods",
                  "Computer-Assisted Sampling", "Internet Surveys",
                  "Big Data Integration", "Machine Learning Applications"),
  Impact = c(7, 8, 9, 8, 9, 9, 8, 7, 8, 6, 7, 8),
  Type = c("Theory", "Theory", "Method", "Theory", "Theory", "Method", 
           "Method", "Practice", "Technology", "Technology", "Integration", "Innovation")
)

ggplot(history_data, aes(x = Year, y = Impact, color = Type)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text(aes(label = Development), vjust = -0.5, size = 3, angle = 45) +
  scale_color_manual(values = c("#2E8B57", "#CD853F", "#4682B4", "#DC143C")) +
  labs(title = "Evolution of Survey Sampling Theory and Methods",
       subtitle = "From Bowley's Random Sampling to Modern Machine Learning Integration",
       x = "Year", y = "Impact on Field (1-10 Scale)",
       color = "Development Type") +
  theme_sadc +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

---

## Fundamental Concepts: Population vs. Sample

.pull-left[
**Population (N)**
- Complete set of all units of interest
- Target population: conceptual population we want to study
- Survey population: population actually covered
- Sampling frame: operational list of population units

**Key Population Parameters**
- μ (population mean)
- σ² (population variance)  
- τ (population total)
- P (population proportion)
]

.pull-right[
**Sample (n)**
- Subset of population units selected for study
- Must be representative of target population
- Size determined by precision requirements
- Selection probability known for each unit

**Sample Statistics**
- ȳ (sample mean)
- s² (sample variance)
- t̂ (estimated total)
- p̂ (sample proportion)
]

---

## Mathematical Foundation: Basic Probability Sampling

**Inclusion Probability**: Probability that unit i is selected in sample

$$\pi_i = P(\text{unit } i \text{ is selected})$$

**Horvitz-Thompson Estimator** for population total:

$$\hat{\tau}_{HT} = \sum_{i \in s} \frac{y_i}{\pi_i}$$

**Properties of HT Estimator**:
- Unbiased: $E[\hat{\tau}_{HT}] = \tau$
- Variance: $V(\hat{\tau}_{HT}) = \sum_{i=1}^N \sum_{j=1}^N (\pi_{ij} - \pi_i \pi_j) \frac{y_i}{\pi_i} \frac{y_j}{\pi_j}$

where $\pi_{ij}$ is joint inclusion probability of units i and j.

---

## Types of Sampling Methods Overview

```{r sampling-types, echo=FALSE, fig.height=6}
library(DiagrammeR)

DiagrammeR::grViz("
digraph sampling_methods {
  graph [layout = dot, rankdir = TB]
  
  node [shape = rectangle, style = filled, fillcolor = lightblue]
  
  A [label = 'Sampling Methods', fillcolor = darkblue, fontcolor = white]
  
  B [label = 'Probability Sampling', fillcolor = lightgreen]
  C [label = 'Non-Probability Sampling', fillcolor = lightcoral]
  
  D [label = 'Simple Random\\nSampling (SRS)']
  E [label = 'Stratified\\nSampling']
  F [label = 'Cluster\\nSampling']
  G [label = 'Systematic\\nSampling']
  
  H [label = 'Convenience\\nSampling']
  I [label = 'Purposive\\nSampling']
  J [label = 'Quota\\nSampling']
  K [label = 'Snowball\\nSampling']
  
  A -> B
  A -> C
  
  B -> D
  B -> E  
  B -> F
  B -> G
  
  C -> H
  C -> I
  C -> J
  C -> K
}
")
```

---

class: inverse, center, middle

# Module 2: R Environment Setup for Survey Analysis
## Professional Statistical Computing Environment

---

## R Installation and Configuration

**Step 1: Install Base R**
- Download from https://cran.r-project.org/
- Choose appropriate version for your operating system
- Install with administrator privileges
- Verify installation with `R --version`

**Step 2: Install RStudio Desktop**
- Download from https://www.rstudio.com/products/rstudio/download/
- Professional IDE for R development
- Enhanced debugging and visualization capabilities
- Integrated version control (Git) support

**Step 3: Configure R Environment**
```r
# Check R version and capabilities
version
capabilities()

# Set up CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Configure scientific notation
options(scipen = 999)
```

---

## Essential Packages for Survey Analysis

```r
# Core survey analysis packages
essential_packages <- c(
  "survey",        # Complex survey data analysis
  "sampling",      # Sampling design implementation  
  "srvyr",         # dplyr-compatible survey analysis
  "PracTools",     # Sample size and power calculations
  "laeken",        # Indicators calculation with sampling weights
  
  # Data manipulation and visualization
  "tidyverse",     # Modern R data science toolkit
  "data.table",    # High-performance data manipulation
  "ggplot2",       # Advanced data visualization
  "plotly",        # Interactive visualizations
  "DT",            # Interactive data tables
  
  # Statistical analysis
  "Hmisc",         # Statistical functions and utilities
  "weights",       # Weighted statistical functions
  "questionr",     # Survey data analysis tools
  "forcats",       # Factor manipulation
  
  # Documentation and reporting
  "knitr",         # Dynamic report generation
  "rmarkdown",     # R Markdown documents
  "bookdown",      # Technical documentation
  "xaringan"       # Presentation slides
)
```

---

## Package Installation and Verification

```{r package-setup, eval=FALSE}
# Function to install missing packages
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    install.packages(new_packages, dependencies = TRUE)
  }
  
  # Load all packages
  sapply(packages, require, character.only = TRUE)
}

# Install and load essential packages
install_if_missing(essential_packages)

# Verify survey package installation
library(survey)
packageVersion("survey")

# Test basic functionality
data(api)
dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, 
                   data = apistrat, fpc = ~fpc)
svymean(~api00, dstrat)
```

---

## RStudio Project Configuration

**Creating Survey Analysis Project**
```r
# Project directory structure
dir.create("SADC-Survey-Project")
setwd("SADC-Survey-Project")

# Create subdirectories
subdirs <- c("data", "scripts", "output", "documentation", 
             "functions", "figures", "reports")
sapply(subdirs, dir.create)

# Create .Rprofile for project-specific settings
writeLines(c(
  "# Project-specific R configuration",
  "options(stringsAsFactors = FALSE)",
  "options(survey.lonely.psu = 'adjust')",
  "options(digits = 4)",
  "",
  "# Load essential libraries",
  "suppressMessages({",
  "  library(survey)",
  "  library(sampling)", 
  "  library(tidyverse)",
  "})",
  "",
  "cat('SADC Survey Analysis Project Environment Loaded\\n')"
), ".Rprofile")
```

---

## Data Import and Validation Functions

```{r data-functions, eval=FALSE}
# Function to read and validate survey data
read_survey_data <- function(file_path, format = "csv") {
  
  # Data import based on format
  if (format == "csv") {
    data <- read.csv(file_path, stringsAsFactors = FALSE)
  } else if (format == "xlsx") {
    data <- readxl::read_excel(file_path)
  } else if (format == "sav") {
    data <- haven::read_spss(file_path)
  }
  
  # Basic validation
  cat("Data dimensions:", dim(data), "\n")
  cat("Missing values:", sum(is.na(data)), "\n")
  cat("Variable types:\n")
  print(sapply(data, class))
  
  # Return data with metadata
  attr(data, "import_date") <- Sys.time()
  attr(data, "file_path") <- file_path
  
  return(data)
}

# Function to create sampling weights summary
weight_summary <- function(weights) {
  summary_stats <- list(
    mean = mean(weights, na.rm = TRUE),
    median = median(weights, na.rm = TRUE),
    min = min(weights, na.rm = TRUE),
    max = max(weights, na.rm = TRUE),
    cv = sd(weights, na.rm = TRUE) / mean(weights, na.rm = TRUE),
    missing = sum(is.na(weights))
  )
  
  return(summary_stats)
}
```

---

class: inverse, center, middle

# Module 3: Simple Random Sampling Theory
## The Foundation of Probability Sampling

---

## Simple Random Sampling (SRS) Definition

**Formal Definition**: A sampling method where:
1. Every unit in the population has equal probability of selection
2. Every possible sample of size n has equal probability of selection
3. Selection of one unit is independent of selection of others

**Mathematical Properties**:
- Selection probability: $\pi_i = \frac{n}{N}$ for all units i
- Sample size: n (fixed)
- Population size: N (known)

**Two Variants**:
- **SRSWR**: Simple Random Sampling With Replacement
- **SRSWOR**: Simple Random Sampling Without Replacement

---

## SRSWR vs SRSWOR: Mathematical Differences

.pull-left[
**SRSWR (With Replacement)**
- Units can be selected multiple times
- Each selection independent
- Sampling distribution: Binomial
- Standard error: $SE(\bar{y}) = \frac{\sigma}{\sqrt{n}}$

**Use Cases**:
- Theoretical applications
- Large population approximations
- Bootstrap resampling
- Simple mathematical models
]

.pull-right[
**SRSWOR (Without Replacement)**
- Each unit selected at most once
- More efficient than SRSWR
- Finite population correction applies
- Standard error: $SE(\bar{y}) = \sqrt{\frac{1-f}{n}} \cdot \frac{\sigma}{\sqrt{n}}$

**Use Cases**:
- Practical survey applications
- Finite population inference
- Resource-constrained studies
- Standard household surveys
]

where f = n/N is the sampling fraction.

---

## Population Parameters and Sample Statistics

```{r parameters-table, echo=FALSE}
library(knitr)
library(kableExtra)

param_table <- data.frame(
  Parameter = c("Population Mean", "Population Total", "Population Variance", 
                "Population Proportion", "Population Size"),
  Symbol = c("μ", "τ", "σ²", "P", "N"),
  Formula = c("μ = (1/N)∑yᵢ", "τ = ∑yᵢ", "σ² = (1/(N-1))∑(yᵢ-μ)²", 
              "P = (1/N)∑aᵢ", "N = |Population|"),
  Estimator = c("Sample Mean (ȳ)", "Estimated Total (τ̂)", "Sample Variance (s²)",
                "Sample Proportion (p̂)", "Known/Estimated"),
  Estimator_Formula = c("ȳ = (1/n)∑yᵢ", "τ̂ = N·ȳ", "s² = (1/(n-1))∑(yᵢ-ȳ)²",
                       "p̂ = (1/n)∑aᵢ", "N or N̂")
)

kable(param_table, format = "html", escape = FALSE,
      caption = "Population Parameters and Their Sample Estimators") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 12) %>%
  column_spec(1, bold = TRUE, color = "darkblue") %>%
  column_spec(2, monospace = TRUE) %>%
  column_spec(4, bold = TRUE, color = "darkgreen")
```

---

## Unbiasedness and Efficiency Properties

**Unbiasedness**: An estimator θ̂ is unbiased if E[θ̂] = θ

For SRS sample mean:
$$E[\bar{y}] = \mu \text{ (unbiased)}$$

**Proof**:
$$E[\bar{y}] = E\left[\frac{1}{n}\sum_{i=1}^n y_i\right] = \frac{1}{n}\sum_{i=1}^n E[y_i] = \frac{1}{n}\sum_{i=1}^n \mu = \mu$$

**Variance of Sample Mean (SRSWOR)**:
$$V(\bar{y}) = \left(1-\frac{n}{N}\right)\frac{\sigma^2}{n}$$

**Efficiency**: SRS is most efficient among unbiased estimators when population is homogeneous.

---

## Finite Population Correction (FPC)

**Why FPC Matters**:
- Adjusts for sampling from finite populations
- Reduces variance as sampling fraction increases
- Critical for accurate standard error calculation

**FPC Factor**: $\sqrt{1-f}$ where $f = \frac{n}{N}$

```{r fpc-visualization, echo=FALSE, fig.height=4}
# Create FPC visualization
sampling_fraction <- seq(0, 1, by = 0.01)
fpc <- sqrt(1 - sampling_fraction)

fpc_data <- data.frame(
  Sampling_Fraction = sampling_fraction,
  FPC_Factor = fpc,
  Variance_Reduction = 1 - fpc^2
)

ggplot(fpc_data, aes(x = Sampling_Fraction)) +
  geom_line(aes(y = FPC_Factor, color = "FPC Factor"), size = 1.2) +
  geom_line(aes(y = Variance_Reduction, color = "Variance Reduction"), size = 1.2) +
  scale_color_manual(values = c("FPC Factor" = "#2E8B57", "Variance Reduction" = "#CD853F")) +
  labs(title = "Finite Population Correction Effects",
       subtitle = "Impact of Sampling Fraction on Variance",
       x = "Sampling Fraction (n/N)", y = "Factor Value",
       color = "Effect") +
  theme_sadc +
  geom_vline(xintercept = 0.05, linetype = "dashed", alpha = 0.7) +
  annotate("text", x = 0.1, y = 0.8, label = "FPC typically\nignored when f < 0.05")
```

---

class: inverse, center, middle

# Module 4: Sample Size Determination
## Precision, Power, and Resource Optimization

---

## Sample Size Formula for SRS

**For estimating population mean with desired precision**:

$$n = \frac{z^2 \sigma^2}{d^2} \cdot \frac{N}{N-1+\frac{z^2 \sigma^2}{d^2}}$$

Where:
- z = critical value from standard normal distribution
- σ² = population variance (estimated from pilot study)
- d = desired margin of error
- N = population size

**Simplified formula when N is large**:
$$n = \frac{z^2 \sigma^2}{d^2}$$

---

## Sample Size Considerations

.pull-left[
**Factors Affecting Sample Size**
- Required precision level
- Confidence level (typically 95%)
- Population variability
- Available resources
- Non-response rates
- Design effects

**Common Confidence Levels**
- 90%: z = 1.645
- 95%: z = 1.96  
- 99%: z = 2.576
]

.pull-right[
**Practical Considerations**
- Minimum sample size for analysis
- Subgroup analysis requirements  
- Budget and time constraints
- Field logistics
- Quality vs. quantity trade-offs

**Rule of Thumb**
- Minimum n = 30 for CLT
- n ≥ 100 for proportion estimates
- n ≥ 400 for complex analysis
- Adjust for expected non-response
]

---

## Interactive Sample Size Calculator

```{r sample-size-calculator, eval=FALSE}
# Interactive sample size calculation function
calculate_sample_size <- function(confidence_level = 0.95, 
                                margin_error = 0.05,
                                population_variance = 1,
                                population_size = Inf,
                                expected_response_rate = 0.8) {
  
  # Get critical value
  alpha <- 1 - confidence_level
  z <- qnorm(1 - alpha/2)
  
  # Calculate base sample size
  if (is.infinite(population_size)) {
    n_base <- (z^2 * population_variance) / margin_error^2
  } else {
    n_base <- (z^2 * population_variance * population_size) / 
              (margin_error^2 * (population_size - 1) + z^2 * population_variance)
  }
  
  # Adjust for non-response
  n_adjusted <- ceiling(n_base / expected_response_rate)
  
  # Return results
  results <- list(
    base_sample_size = ceiling(n_base),
    adjusted_sample_size = n_adjusted,
    confidence_level = confidence_level,
    margin_error = margin_error,
    z_value = z,
    response_rate = expected_response_rate
  )
  
  return(results)
}

# Example calculation
sample_calc <- calculate_sample_size(
  confidence_level = 0.95,
  margin_error = 0.05,
  population_variance = 1,
  population_size = 100000,
  expected_response_rate = 0.75
)

print(sample_calc)
```

---

class: inverse, center, middle

# Module 5: Implementing SRS in R
## Hands-on Programming Session

---

## Basic SRS Implementation

```{r srs-implementation, eval=FALSE}
# Set seed for reproducibility
set.seed(12345)

# Create synthetic population
N <- 10000  # Population size
population <- data.frame(
  id = 1:N,
  age = round(rnorm(N, mean = 35, sd = 12)),
  income = round(rlnorm(N, meanlog = 10, sdlog = 0.5)),
  education = sample(c("Primary", "Secondary", "Tertiary"), 
                    N, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
  region = sample(c("Urban", "Rural"), N, replace = TRUE, prob = c(0.4, 0.6)),
  household_size = rpois(N, lambda = 4) + 1
)

# Add some correlation between variables
population$income <- population$income * 
  ifelse(population$education == "Tertiary", 1.5,
         ifelse(population$education == "Secondary", 1.2, 1.0))

# View population summary
summary(population)
```

---

## Drawing Simple Random Sample

```{r draw-sample, eval=FALSE}
# Method 1: Using base R sample function
n <- 500  # Desired sample size
sample_ids <- sample(population$id, size = n, replace = FALSE)
srs_sample <- population[population$id %in% sample_ids, ]

# Method 2: Using sampling package
library(sampling)
sample_indicator <- srswor(n, N)  # Returns 0/1 indicator vector
srs_sample2 <- population[sample_indicator == 1, ]

# Method 3: Using systematic approach with random start
k <- floor(N / n)  # Sampling interval
random_start <- sample(1:k, 1)
systematic_ids <- seq(random_start, N, by = k)[1:n]
systematic_sample <- population[systematic_ids, ]

# Compare samples
cat("SRS Sample mean income:", mean(srs_sample$income), "\n")
cat("Population mean income:", mean(population$income), "\n")
cat("Difference:", mean(srs_sample$income) - mean(population$income), "\n")
```

---

## Creating Survey Design Object

```{r survey-design, eval=FALSE}
library(survey)

# Create survey design object for SRS
# Method 1: Equal probability sampling
srs_design <- svydesign(
  ids = ~1,                    # No clustering (SRS)
  data = srs_sample,          # Sample data
  fpc = rep(N, nrow(srs_sample))  # Finite population correction
)

# Method 2: Specify sampling weights directly
sampling_weight <- N / n     # Each unit represents N/n population units
srs_sample$weight <- sampling_weight

srs_design2 <- svydesign(
  ids = ~1,
  weights = ~weight,
  data = srs_sample
)

# Verify design
print(srs_design)
summary(srs_design)

# Check weights
svytotal(~1, srs_design)  # Should equal N
```

---

## Point Estimation with SRS

```{r point-estimation, eval=FALSE}
# Estimate population mean
mean_estimate <- svymean(~income, srs_design)
print(mean_estimate)

# Extract point estimate and standard error
point_est <- coef(mean_estimate)
std_error <- SE(mean_estimate)

cat("Estimated mean income:", round(point_est, 2), "\n")
cat("Standard error:", round(std_error, 2), "\n")
cat("True population mean:", round(mean(population$income), 2), "\n")

# Estimate population total
total_estimate <- svytotal(~income, srs_design)
print(total_estimate)

# Estimate population proportions
prop_estimates <- svymean(~education, srs_design)
print(prop_estimates)

# Population totals by category
total_by_education <- svyby(~income, ~education, srs_design, svytotal)
print(total_by_education)
```

---

## Confidence Intervals and Hypothesis Testing

```{r inference, eval=FALSE}
# Confidence intervals for mean
ci_mean <- confint(mean_estimate, level = 0.95)
cat("95% CI for mean income:", ci_mean[1], "to", ci_mean[2], "\n")

# Confidence interval for total
ci_total <- confint(total_estimate, level = 0.95)
print(ci_total)

# Test hypothesis about mean
# H0: μ = 50000 vs H1: μ ≠ 50000
t_test <- svyttest(income ~ 1, srs_design, mu = 50000)
print(t_test)

# Test for difference between groups
education_test <- svyttest(income ~ education, srs_design)
print(education_test)

# ANOVA-type test
anova_test <- svyglm(income ~ education + region, design = srs_design)
summary(anova_test)
```

---

## Variance Estimation and Design Effects

```{r variance-estimation, eval=FALSE}
# Calculate design effect (should be 1 for SRS)
# DEFF = V_actual / V_SRS = 1 for SRS
income_srs <- svymean(~income, srs_design)
theoretical_se <- sqrt(var(population$income) * (1 - n/N) / n)
actual_se <- SE(income_srs)

design_effect <- (actual_se / theoretical_se)^2
cat("Design Effect:", round(design_effect, 3), "\n")

# Effective sample size
eff_sample_size <- n / design_effect
cat("Effective Sample Size:", round(eff_sample_size, 0), "\n")

# Compare with theoretical calculations
theoretical_mean_se <- sqrt(var(population$income)) * sqrt((1 - n/N) / n)
cat("Theoretical SE:", round(theoretical_mean_se, 2), "\n")
cat("Survey package SE:", round(actual_se, 2), "\n")
cat("Difference:", round(abs(theoretical_mean_se - actual_se), 4), "\n")
```

---

## Diagnostic Plots and Visualization

```{r diagnostics, eval=FALSE}
# Distribution comparison
library(ggplot2)

# Create comparison dataset
comparison_data <- rbind(
  data.frame(income = population$income, type = "Population"),
  data.frame(income = srs_sample$income, type = "Sample")
)

# Density plot comparison
ggplot(comparison_data, aes(x = income, fill = type)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("Population" = "lightblue", "Sample" = "red")) +
  labs(title = "Income Distribution: Population vs. Sample",
       subtitle = "Simple Random Sample Representativeness",
       x = "Income", y = "Density", fill = "Data Source") +
  theme_sadc

# Q-Q plot for sample representativeness
qqplot(population$income, srs_sample$income,
       main = "Q-Q Plot: Population vs. Sample",
       xlab = "Population Quantiles", ylab = "Sample Quantiles")
abline(0, 1, col = "red", lwd = 2)

# Sampling distribution simulation
simulate_sampling_distribution <- function(pop_data, sample_size, num_samples = 1000) {
  sample_means <- replicate(num_samples, {
    sample_indices <- sample(nrow(pop_data), sample_size, replace = FALSE)
    mean(pop_data$income[sample_indices])
  })
  return(sample_means)
}

sampling_dist <- simulate_sampling_distribution(population, n, 1000)
```

---

class: inverse, center, middle

# Module 6: Quality Assessment and Validation
## Ensuring Statistical Rigor

---

## Bias Assessment in SRS

```{r bias-assessment, eval=FALSE}
# Function to assess sampling bias
assess_sampling_bias <- function(population, sample, variables) {
  bias_results <- data.frame(
    Variable = character(),
    Population_Mean = numeric(),
    Sample_Mean = numeric(),
    Absolute_Bias = numeric(),
    Relative_Bias_Percent = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (var in variables) {
    if (is.numeric(population[[var]]) && is.numeric(sample[[var]])) {
      pop_mean <- mean(population[[var]], na.rm = TRUE)
      sample_mean <- mean(sample[[var]], na.rm = TRUE)
      abs_bias <- abs(sample_mean - pop_mean)
      rel_bias <- (abs_bias / pop_mean) * 100
      
      bias_results <- rbind(bias_results, data.frame(
        Variable = var,
        Population_Mean = pop_mean,
        Sample_Mean = sample_mean,
        Absolute_Bias = abs_bias,
        Relative_Bias_Percent = rel_bias
      ))
    }
  }
  
  return(bias_results)
}

# Assess bias in our SRS sample
variables_to_check <- c("age", "income", "household_size")
bias_assessment <- assess_sampling_bias(population, srs_sample, variables_to_check)
print(bias_assessment)

# Acceptable bias thresholds (example criteria)
acceptable_relative_bias <- 5  # 5% relative bias threshold
bias_assessment$Acceptable <- bias_assessment$Relative_Bias_Percent < acceptable_relative_bias
print(bias_assessment)
```

---

## Coverage Assessment and Non-response Analysis

```{r coverage-assessment, eval=FALSE}
# Simulate non-response based on income (higher income = higher non-response)
simulate_nonresponse <- function(sample_data, nonresponse_rate = 0.2) {
  # Create non-response probability based on income
  prob_nonresponse <- pmin(0.4, 0.1 + (sample_data$income - min(sample_data$income)) / 
                          (max(sample_data$income) - min(sample_data$income)) * 0.3)
  
  # Generate non-response indicators
  nonresponse_indicator <- rbinom(nrow(sample_data), 1, prob_nonresponse)
  
  # Create responding sample
  responding_sample <- sample_data[nonresponse_indicator == 0, ]
  
  # Calculate response rate
  response_rate <- nrow(responding_sample) / nrow(sample_data)
  
  return(list(
    responding_sample = responding_sample,
    response_rate = response_rate,
    nonresponse_pattern = nonresponse_indicator
  ))
}

# Apply non-response simulation
nonresponse_sim <- simulate_nonresponse(srs_sample, 0.25)
responding_sample <- nonresponse_sim$responding_sample

cat("Original sample size:", nrow(srs_sample), "\n")
cat("Responding sample size:", nrow(responding_sample), "\n")
cat("Response rate:", round(nonresponse_sim$response_rate * 100, 1), "%\n")

# Compare estimates with and without non-response
original_design <- svydesign(ids = ~1, fpc = rep(N, nrow(srs_sample)), data = srs_sample)
responding_design <- svydesign(ids = ~1, fpc = rep(N, nrow(responding_sample)), data = responding_sample)

original_mean <- svymean(~income, original_design)
responding_mean <- svymean(~income, responding_design)
population_mean <- mean(population$income)

cat("Population mean income:", round(population_mean, 2), "\n")
cat("Full sample estimate:", round(coef(original_mean), 2), "\n")
cat("Responding sample estimate:", round(coef(responding_mean), 2), "\n")
cat("Non-response bias:", round(coef(responding_mean) - population_mean, 2), "\n")
```

---

## Sample Size Adequacy and Power Analysis

```{r power-analysis, eval=FALSE}
# Function for post-hoc power analysis
power_analysis_continuous <- function(sample_data, variable, 
                                    effect_size = 0.2, alpha = 0.05) {
  n <- nrow(sample_data)
  sample_sd <- sd(sample_data[[variable]], na.rm = TRUE)
  
  # Calculate achieved power for detecting specified effect size
  delta <- effect_size * sample_sd
  se <- sample_sd / sqrt(n)
  t_critical <- qt(1 - alpha/2, df = n - 1)
  
  # Power calculation
  power <- 1 - pt(t_critical - delta/se, df = n - 1) + 
           pt(-t_critical - delta/se, df = n - 1)
  
  return(list(
    sample_size = n,
    effect_size = effect_size,
    power = power,
    minimum_detectable_effect = t_critical * se
  ))
}

# Analyze power for income variable
income_power <- power_analysis_continuous(srs_sample, "income", effect_size = 0.1)
print(income_power)

# Sample size adequacy for subgroup analysis
subgroup_sizes <- table(srs_sample$education)
print("Subgroup sample sizes:")
print(subgroup_sizes)

# Check if subgroup sizes are adequate (rule of thumb: n ≥ 30)
adequate_subgroups <- subgroup_sizes >= 30
print("Adequate subgroup sizes:")
print(adequate_subgroups)
```

---

class: inverse, center, middle

# Module 7: Advanced SRS Applications
## Real-World Implementation Strategies

---

## Handling Complex Populations

```{r complex-populations, eval=FALSE}
# Simulate population with missing frame elements
create_complex_population <- function(N = 10000, coverage_rate = 0.85) {
  # Create full target population
  full_population <- data.frame(
    id = 1:N,
    age = round(rnorm(N, mean = 35, sd = 12)),
    income = round(rlnorm(N, meanlog = 10, sdlog = 0.5)),
    education = sample(c("Primary", "Secondary", "Tertiary"), 
                      N, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
    region = sample(c("Urban", "Rural"), N, replace = TRUE, prob = c(0.4, 0.6))
  )
  
  # Create sampling frame (incomplete coverage)
  frame_size <- round(N * coverage_rate)
  
  # Bias: urban areas more likely to be in frame
  urban_coverage <- 0.95
  rural_coverage <- 0.75
  
  urban_indices <- which(full_population$region == "Urban")
  rural_indices <- which(full_population$region == "Rural")
  
  frame_urban <- sample(urban_indices, 
                       round(length(urban_indices) * urban_coverage))
  frame_rural <- sample(rural_indices, 
                       round(length(rural_indices) * rural_coverage))
  
  frame_indices <- c(frame_urban, frame_rural)
  sampling_frame <- full_population[frame_indices, ]
  
  return(list(
    target_population = full_population,
    sampling_frame = sampling_frame,
    coverage_rate = nrow(sampling_frame) / N,
    urban_coverage = sum(sampling_frame$region == "Urban") / sum(full_population$region == "Urban"),
    rural_coverage = sum(sampling_frame$region == "Rural") / sum(full_population$region == "Rural")
  ))
}

# Create complex population scenario
complex_pop <- create_complex_population(N = 10000, coverage_rate = 0.85)

cat("Target population size:", nrow(complex_pop$target_population), "\n")
cat("Sampling frame size:", nrow(complex_pop$sampling_frame), "\n")
cat("Overall coverage rate:", round(complex_pop$coverage_rate * 100, 1), "%\n")
cat("Urban coverage rate:", round(complex_pop$urban_coverage * 100, 1), "%\n")
cat("Rural coverage rate:", round(complex_pop$rural_coverage * 100, 1), "%\n")
```

---

## Multi-Stage Area Sampling Setup

```{r multistage-setup, eval=FALSE}
# Prepare for multi-stage sampling (Day 3 preview)
create_geographic_frame <- function(n_regions = 10, n_districts_per_region = 5, 
                                   n_households_per_district = 100) {
  
  regions <- paste0("Region_", 1:n_regions)
  districts <- paste0("District_", 1:(n_regions * n_districts_per_region))
  
  # Create hierarchical frame
  geographic_frame <- expand.grid(
    Region = rep(regions, each = n_districts_per_region),
    District = districts,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      District_ID = 1:nrow(.),
      N_households = rpois(nrow(.), lambda = n_households_per_district),
      Urban_Rural = sample(c("Urban", "Rural"), nrow(.), 
                          replace = TRUE, prob = c(0.3, 0.7))
    )
  
  return(geographic_frame)
}

# Create geographic sampling frame
geo_frame <- create_geographic_frame(n_regions = 8, n_districts_per_region = 6)
print(head(geo_frame))

# Calculate frame totals
total_districts <- nrow(geo_frame)
total_households <- sum(geo_frame$N_households)

cat("Total districts in frame:", total_districts, "\n")
cat("Total households in frame:", total_households, "\n")
cat("Average households per district:", round(mean(geo_frame$N_households), 1), "\n")
```

---

## Quality Control Procedures

```{r quality-control, eval=FALSE}
# Comprehensive quality control function
survey_quality_control <- function(sample_data, design_object, population_benchmarks = NULL) {
  
  qc_results <- list()
  
  # 1. Sample size adequacy
  qc_results$sample_size <- nrow(sample_data)
  qc_results$min_recommended <- 384  # For 95% confidence, 5% margin of error
  qc_results$size_adequate <- qc_results$sample_size >= qc_results$min_recommended
  
  # 2. Response rates by key variables
  qc_results$missing_rates <- sapply(sample_data, function(x) sum(is.na(x)) / length(x))
  
  # 3. Weight diagnostics
  if ("weight" %in% names(sample_data)) {
    weights <- sample_data$weight
    qc_results$weight_diagnostics <- list(
      mean_weight = mean(weights),
      min_weight = min(weights),
      max_weight = max(weights),
      weight_cv = sd(weights) / mean(weights),
      extreme_weights = sum(weights > 4 * mean(weights) | weights < 0.25 * mean(weights))
    )
  }
  
  # 4. Distribution checks
  numeric_vars <- sapply(sample_data, is.numeric)
  qc_results$outlier_counts <- sapply(sample_data[numeric_vars], function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    sum(x < Q1 - 1.5 * IQR | x > Q3 + 1.5 * IQR, na.rm = TRUE)
  })
  
  # 5. Design consistency checks
  if (!is.null(design_object)) {
    qc_results$design_check <- list(
      reported_n = nrow(sample_data),
      design_n = nrow(design_object$variables),
      weights_sum = round(sum(weights(design_object))),
      population_estimate = round(svytotal(~1, design_object))
    )
  }
  
  return(qc_results)
}

# Apply quality control
qc_report <- survey_quality_control(srs_sample, srs_design)
print(qc_report)

# Generate quality report
generate_quality_report <- function(qc_results) {
  cat("=== SURVEY QUALITY CONTROL REPORT ===\n\n")
  
  cat("1. SAMPLE SIZE ADEQUACY\n")
  cat("   Sample size:", qc_results$sample_size, "\n")
  cat("   Minimum recommended:", qc_results$min_recommended, "\n")
  cat("   Adequate:", ifelse(qc_results$size_adequate, "YES", "NO"), "\n\n")
  
  cat("2. MISSING DATA RATES\n")
  for (var in names(qc_results$missing_rates)) {
    rate <- round(qc_results$missing_rates[var] * 100, 1)
    status <- ifelse(rate < 5, "GOOD", ifelse(rate < 10, "ACCEPTABLE", "CONCERN"))
    cat("   ", var, ":", rate, "% -", status, "\n")
  }
  
  if (!is.null(qc_results$weight_diagnostics)) {
    cat("\n3. WEIGHT DIAGNOSTICS\n")
    wd <- qc_results$weight_diagnostics
    cat("   Mean weight:", round(wd$mean_weight, 2), "\n")
    cat("   Weight range:", round(wd$min_weight, 2), "to", round(wd$max_weight, 2), "\n")
    cat("   Coefficient of variation:", round(wd$weight_cv, 3), "\n")
    cat("   Extreme weights (>4x or <0.25x mean):", wd$extreme_weights, "\n")
  }
  
  cat("\n4. OUTLIER DETECTION\n")
  for (var in names(qc_results$outlier_counts)) {
    count <- qc_results$outlier_counts[var]
    pct <- round(count / qc_results$sample_size * 100, 1)
    cat("   ", var, ":", count, "outliers (", pct, "%)\n")
  }
}

generate_quality_report(qc_report)
```

---

class: inverse, center, middle

# Module 8: Documentation and Reporting
## Professional Statistical Communication

---

## Survey Documentation Standards

```{r documentation-template, eval=FALSE}
# Survey documentation template
create_survey_documentation <- function(survey_design, sample_data, 
                                      methodology_details = list()) {
  
  doc <- list()
  
  # Survey Overview
  doc$overview <- list(
    survey_title = "SADC Household Survey - Advanced Sampling Methods Training",
    reference_period = "November 2025",
    target_population = "Households in SADC Member States",
    geographic_coverage = "National",
    sampling_method = "Simple Random Sampling Without Replacement"
  )
  
  # Methodological Details
  doc$methodology <- list(
    sampling_frame = "Complete household listing from national census",
    sample_design = "Single-stage simple random sampling",
    sample_size_target = methodology_details$target_n %||% nrow(sample_data),
    sample_size_achieved = nrow(sample_data),
    sampling_fraction = nrow(sample_data) / (methodology_details$population_size %||% 10000),
    selection_method = "Random number generation using R sample() function"
  )
  
  # Quality Indicators
  doc$quality <- list(
    response_rate = methodology_details$response_rate %||% 0.85,
    coverage_rate = methodology_details$coverage_rate %||% 0.90,
    item_nonresponse_income = round(sum(is.na(sample_data$income)) / nrow(sample_data), 3),
    weight_cv = if("weight" %in% names(sample_data)) sd(sample_data$weight)/mean(sample_data$weight) else NA
  )
  
  # Estimation Procedures
  doc$estimation <- list(
    estimator_type = "Horvitz-Thompson",
    variance_estimation = "Simple random sampling variance formula with FPC",
    confidence_level = "95%",
    software_used = "R survey package"
  )
  
  return(doc)
}

# Generate documentation
survey_doc <- create_survey_documentation(srs_design, srs_sample, 
                                        list(target_n = 500, 
                                             population_size = 10000,
                                             response_rate = 0.82))
print(survey_doc)
```

---

## Professional Results Tables

```{r results-tables, eval=FALSE}
# Function to create publication-ready results table
create_results_table <- function(design_object, variables, by_variable = NULL) {
  
  results_list <- list()
  
  for (var in variables) {
    if (is.null(by_variable)) {
      # Overall estimates
      if (is.numeric(design_object$variables[[var]])) {
        est <- svymean(as.formula(paste0("~", var)), design_object, na.rm = TRUE)
        total_est <- svytotal(as.formula(paste0("~", var)), design_object, na.rm = TRUE)
        
        results_list[[var]] <- data.frame(
          Variable = var,
          Category = "Overall",
          Mean = round(coef(est), 2),
          SE_Mean = round(SE(est), 2),
          Total = round(coef(total_est), 0),
          SE_Total = round(SE(total_est), 0),
          CI_Lower = round(confint(est)[1], 2),
          CI_Upper = round(confint(est)[2], 2)
        )
      } else {
        # For categorical variables
        prop_est <- svymean(as.formula(paste0("~", var)), design_object, na.rm = TRUE)
        total_est <- svytotal(as.formula(paste0("~", var)), design_object, na.rm = TRUE)
        
        categories <- names(coef(prop_est))
        results_list[[var]] <- data.frame(
          Variable = rep(var, length(categories)),
          Category = categories,
          Proportion = round(coef(prop_est), 3),
          SE_Proportion = round(SE(prop_est), 3),
          Total = round(coef(total_est), 0),
          SE_Total = round(SE(total_est), 0),
          CI_Lower = round(confint(prop_est)[,1], 3),
          CI_Upper = round(confint(prop_est)[,2], 3)
        )
      }
    } else {
      # Estimates by subgroups
      if (is.numeric(design_object$variables[[var]])) {
        est_by <- svyby(as.formula(paste0("~", var)), 
                       as.formula(paste0("~", by_variable)), 
                       design_object, svymean, na.rm = TRUE)
        
        results_list[[var]] <- data.frame(
          Variable = rep(var, nrow(est_by)),
          Category = rownames(est_by),
          Mean = round(est_by[,2], 2),
          SE_Mean = round(est_by[,3], 2),
          CI_Lower = round(est_by[,2] - 1.96 * est_by[,3], 2),
          CI_Upper = round(est_by[,2] + 1.96 * est_by[,3], 2)
        )
      }
    }
  }
  
  # Combine all results
  final_table <- do.call(rbind, results_list)
  rownames(final_table) <- NULL
  
  return(final_table)
}

# Generate results tables
income_results <- create_results_table(srs_design, "income")
education_results <- create_results_table(srs_design, "education")
income_by_education <- create_results_table(srs_design, "income", "education")

print("Income Estimates:")
print(income_results)

print("Education Distribution:")
print(education_results)

print("Income by Education Level:")
print(income_by_education)
```

---

## Statistical Report Generation

```{r report-generation, eval=FALSE}
# Function to generate executive summary
generate_executive_summary <- function(design_object, key_variables) {
  
  summary_stats <- list()
  
  # Sample characteristics
  summary_stats$sample_size <- nrow(design_object$variables)
  summary_stats$population_estimate <- round(sum(weights(design_object)))
  summary_stats$sampling_fraction <- summary_stats$sample_size / summary_stats$population_estimate
  
  # Key estimates
  for (var in key_variables) {
    if (is.numeric(design_object$variables[[var]])) {
      est <- svymean(as.formula(paste0("~", var)), design_object, na.rm = TRUE)
      summary_stats[[paste0(var, "_mean")]] <- round(coef(est), 2)
      summary_stats[[paste0(var, "_se")]] <- round(SE(est), 2)
      summary_stats[[paste0(var, "_cv")]] <- round(SE(est) / coef(est) * 100, 1)
    }
  }
  
  return(summary_stats)
}

# Generate executive summary
exec_summary <- generate_executive_summary(srs_design, c("income", "age", "household_size"))

# Format summary report
cat("=== EXECUTIVE SUMMARY ===\n\n")
cat("Survey Design: Simple Random Sampling\n")
cat("Sample Size:", exec_summary$sample_size, "\n")
cat("Population Estimate:", exec_summary$population_estimate, "\n")
cat("Sampling Fraction:", round(exec_summary$sampling_fraction * 100, 2), "%\n\n")

cat("KEY FINDINGS:\n")
cat("Average Income:", exec_summary$income_mean, "± ", exec_summary$income_se, 
    " (CV =", exec_summary$income_cv, "%)\n")
cat("Average Age:", exec_summary$age_mean, "± ", exec_summary$age_se, 
    " (CV =", exec_summary$age_cv, "%)\n")
cat("Average Household Size:", exec_summary$household_size_mean, "± ", exec_summary$household_size_se, 
    " (CV =", exec_summary$household_size_cv, "%)\n")
```

---

class: inverse, center, middle

# Module 9: Practical Exercise Session
## Hands-on Implementation Workshop

---

## Exercise 1: Complete SRS Workflow

**Objective**: Implement end-to-end simple random sampling workflow

**Your Task**:
1. Load provided household dataset
2. Design and implement SRS (n=300 from N=5000)
3. Calculate key population estimates
4. Assess sampling quality
5. Generate professional report

```{r exercise-1-setup, eval=FALSE}
# Exercise 1: Load your dataset here
# (Instructor will provide SADC_household_data.csv)

# Step 1: Data loading and exploration
household_data <- read.csv("../03-Data/SADC_household_data.csv")

# Your code here:
# - Explore the dataset structure
# - Identify key variables for analysis
# - Check data quality issues

# Step 2: Sample design
# Your code here:
# - Determine appropriate sample size
# - Implement SRS selection
# - Create survey design object

# Step 3: Estimation
# Your code here:
# - Calculate population estimates
# - Compute confidence intervals
# - Assess precision

# Step 4: Quality assessment
# Your code here:
# - Check representativeness
# - Assess bias
# - Evaluate weights

# Step 5: Reporting
# Your code here:
# - Create results table
# - Generate summary statistics
# - Document methodology
```

---

## Exercise 2: Comparative Analysis

**Objective**: Compare different sample sizes and their impact on precision

```{r exercise-2-template, eval=FALSE}
# Exercise 2: Sample size impact analysis

sample_sizes <- c(100, 200, 300, 500, 800, 1200)
population_size <- 10000

# Your task: For each sample size, calculate:
# 1. Standard error of mean income
# 2. 95% confidence interval width
# 3. Coefficient of variation
# 4. Design effect (should be 1 for SRS)

results_comparison <- data.frame(
  Sample_Size = sample_sizes,
  Standard_Error = numeric(length(sample_sizes)),
  CI_Width = numeric(length(sample_sizes)),
  CV_Percent = numeric(length(sample_sizes)),
  Design_Effect = numeric(length(sample_sizes))
)

# Your code here:
for (i in 1:length(sample_sizes)) {
  n <- sample_sizes[i]
  
  # Implement sampling and estimation
  # Fill in the results_comparison table
}

# Create visualization of results
# Your code here: Create plots showing relationship between
# sample size and precision measures
```

---

## Exercise 3: Real SADC Context Application

**Scenario**: You are tasked with designing a household expenditure survey for a SADC member state with the following characteristics:

- Population: 2.5 million households
- Available budget: $500,000
- Cost per completed interview: $150
- Expected response rate: 75%
- Required precision: ±2% for mean monthly expenditure
- Population variance estimate: $50,000²

```{r exercise-3-scenario, eval=FALSE}
# Exercise 3: Real-world application

# Given parameters
N_households <- 2500000
budget <- 500000
cost_per_interview <- 150
response_rate <- 0.75
margin_error <- 0.02  # 2% margin of error
population_variance <- 50000^2
confidence_level <- 0.95

# Your tasks:
# 1. Calculate required sample size for desired precision
# 2. Adjust for expected non-response
# 3. Calculate total cost and compare with budget
# 4. If over budget, recalculate with feasible sample size
# 5. Assess trade-offs between precision and cost

# Task 1: Sample size calculation
# Your code here:

# Task 2: Non-response adjustment
# Your code here:

# Task 3: Cost calculation
# Your code here:

# Task 4: Budget constraint analysis
# Your code here:

# Task 5: Final recommendations
# Your code here:
# Prepare a brief recommendation report with:
# - Recommended sample size
# - Expected precision
# - Cost breakdown
# - Quality trade-offs
```

---

class: inverse, center, middle

# Module 10: Day 1 Summary and Preview
## Consolidation and Forward Look

---

## Key Concepts Mastered Today

.pull-left[
**Theoretical Foundations**
- Probability sampling principles
- Simple random sampling theory
- Unbiasedness and efficiency
- Finite population correction
- Variance estimation

**Practical Skills**
- R environment configuration
- Survey package implementation
- Sample selection methods
- Weight calculation
- Quality assessment procedures
]

.pull-right[
**Professional Applications**
- Survey design documentation
- Results table creation
- Executive summary writing
- Quality control protocols
- International standard compliance

**Statistical Computing**
- Reproducible workflows
- Error handling
- Diagnostic procedures
- Visualization techniques
- Report automation
]

---

## Common Challenges and Solutions

```{r challenges-solutions, echo=FALSE}
challenges_df <- data.frame(
  Challenge = c(
    "Low response rates",
    "Frame coverage issues", 
    "Budget constraints",
    "Precision requirements",
    "Non-response bias",
    "Weight variability",
    "Subgroup analysis",
    "Quality documentation"
  ),
  Impact = c(
    "Reduces effective sample size",
    "Coverage bias in estimates",
    "Limited sample size options", 
    "Conflict with cost considerations",
    "Systematic estimation errors",
    "Inflated variance estimates",
    "Insufficient precision for domains",
    "Reduced credibility and replicability"
  ),
  Solution = c(
    "Increase initial sample, improve field procedures",
    "Frame evaluation and post-survey adjustments",
    "Optimize design efficiency, staged sampling",
    "Power analysis and precision-cost trade-offs",
    "Response propensity modeling, adjustments",
    "Weight trimming and calibration",
    "Stratified or clustered designs",
    "Standardized templates and procedures"
  ),
  stringsAsFactors = FALSE
)

kable(challenges_df, format = "html", 
      caption = "Common SRS Implementation Challenges") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  column_spec(1, bold = TRUE, color = "darkred") %>%
  column_spec(3, color = "darkgreen")
```

---

## Preview: Upcoming Days

**Day 2: Stratified Sampling**
- Stratification principles and benefits
- Optimal allocation strategies (Neyman, proportional)
- Implementation in complex populations
- Efficiency gains quantification

**Day 3: Cluster and Multi-stage Sampling**
- Cluster sampling theory and design effects
- Multi-stage sampling procedures
- Cost-efficiency optimization
- Geographic sampling applications

**Day 4: Advanced Weighting and Calibration**
- Post-stratification methods
- GREG estimation techniques
- Non-response adjustment procedures
- Variance estimation for complex designs

**Day 5: Quality Assurance and Applications**
- Comprehensive quality frameworks
- GIS integration techniques
- Real-world SADC case studies
- Project implementation strategies

---

## Homework Assignment

**Assignment**: Implement complete SRS analysis on provided dataset

**Deliverables** (due beginning of Day 2):
1. R script with documented code
2. Quality assessment report  
3. Executive summary (1 page)
4. Professional results table
5. Methodology documentation

**Dataset**: SADC_Training_Data.csv (available in course materials)
**Requirements**: Follow all quality standards and documentation procedures covered today

**Evaluation Criteria**:
- Technical accuracy (40%)
- Code quality and documentation (25%)
- Professional presentation (20%) 
- Methodological rigor (15%)

---

class: center, middle, inverse

# Questions and Discussion

### What aspects of SRS implementation are most relevant to your work context?

### Which software techniques will be most useful in your statistical office?

### What additional topics would enhance your survey methodology toolkit?

---

class: center, middle

# Thank You!

### Day 1: Foundations and Simple Random Sampling Complete

**Tomorrow**: Advanced Stratified Sampling Techniques  
**Time**: 9:00 AM - 5:00 PM  
**Materials**: Available in course repository

**Contact**: Dr. Endri Raço  
**Email**: e.raco@fimif.edu.al  
**Repository**: https://github.com/endri81/SADC-Advanced-Sampling-Workshop

---

class: inverse, center, middle

# Appendix: Additional Resources

---

## Essential References

**Core Textbooks**:
- Lohr, S.L. (2019). *Sampling: Design and Analysis*. 3rd Edition. Chapman & Hall/CRC
- Cochran, W.G. (1977). *Sampling Techniques*. 3rd Edition. Wiley
- Thompson, S.K. (2012). *Sampling*. 3rd Edition. Wiley
- Särndal, C.E., Swensson, B., Wretman, J. (2003). *Model Assisted Survey Sampling*. Springer

**R Programming Resources**:
- Lumley, T. (2010). *Complex Surveys: A Guide to Analysis Using R*. Wiley
- Valliant, R., Dever, J.A., Kreuter, F. (2018). *Practical Tools for Designing and Weighting Survey Samples*. 2nd Edition. Springer

**International Standards**:
- Eurostat (2013). *Handbook on Precision Requirements and Variance Estimation for ESS Households Surveys*
- UN Statistical Division (2005). *Handbook of Statistical Organization: The Operation and Organization of a Statistical Agency*
- OECD (2013). *OECD Framework for Statistics on the Distribution of Household Income, Consumption and Wealth*

**Survey Package Documentation**:
- R Core Team (2023). *R: A Language and Environment for Statistical Computing*
- Lumley, T. (2023). *survey: Analysis of Complex Survey Samples*. R package version 4.2

---

## R Code Repository Structure

**Scripts Organization**:
```
02-Scripts/
├── Day1-Setup-Demo.R           # R environment setup
├── Day1-SRS-Examples.R         # SRS implementation examples  
├── Day1-Functions.R            # Custom functions library
├── Day1-Quality-Control.R      # QC procedures
├── Day1-Visualization.R        # Plotting functions
└── Day1-Reporting.R            # Report generation
```

**Key Functions Developed Today**:
- `install_if_missing()`: Package management
- `read_survey_data()`: Data import with validation
- `weight_summary()`: Sampling weights diagnostics
- `assess_sampling_bias()`: Bias evaluation
- `simulate_nonresponse()`: Non-response simulation
- `power_analysis_continuous()`: Power calculations
- `survey_quality_control()`: Comprehensive QC
- `create_results_table()`: Professional tables
- `generate_executive_summary()`: Report summaries

---

## Data Files and Documentation

**Available Datasets**:
```
03-Data/
├── SADC_household_data.csv        # Training dataset (5,000 households)
├── population_frame.csv           # Complete population frame
├── data_dictionary.xlsx           # Variable definitions
├── sampling_weights.csv           # Pre-calculated weights example
└── geographic_codes.csv           # Regional classification
```

**Data Dictionary Key Variables**:
- `household_id`: Unique household identifier
- `region`: Geographic region (Urban/Rural)
- `income_monthly`: Monthly household income (local currency)
- `education_head`: Education level of household head
- `household_size`: Number of household members
- `age_head`: Age of household head
- `employment_status`: Employment status of household head

---

## Additional Learning Resources

**Online Courses**:
- Coursera: "Survey Data Collection and Analytics" (University of Maryland)
- edX: "Introduction to Survey Sampling" (MIT)
- IPUMS: "Survey Data Analysis Workshop Series"

**Software Tutorials**:
- Thomas Lumley's Survey Analysis in R Tutorial
- UCLA Statistical Methods and Data Analytics
- Stata Survey Data Reference Manual

**Professional Organizations**:
- American Association for Public Opinion Research (AAPOR)
- International Association of Survey Statisticians (IASS)  
- European Survey Research Association (ESRA)
- African Association of Statistical Agencies

**Journals for Advanced Reading**:
- Journal of Survey Statistics and Methodology
- Survey Methodology (Statistics Canada)
- Journal of Official Statistics
- International Statistical Review

---

## Software Installation Troubleshooting

**Common R Package Installation Issues**:

1. **Dependency Conflicts**:
```r
# Update R and packages
update.packages(ask = FALSE)
install.packages(c("survey", "sampling"), dependencies = TRUE)
```

2. **Compilation Errors**:
```r
# Install binary packages (Windows/Mac)
install.packages("survey", type = "binary")

# For Linux systems
sudo apt-get install r-base-dev
```

3. **Memory Issues**:
```r
# Increase memory limits
memory.limit(size = 8000)  # Windows
options(java.parameters = "-Xmx8g")  # For rJava dependencies
```

4. **Package Loading Problems**:
```r
# Clean restart
.rs.restartR()  # In RStudio
detach("package:conflicted_package", unload = TRUE)
library(survey)
```

**System Requirements**:
- R version ≥ 4.0.0
- RAM ≥ 4GB (8GB recommended)
- Available disk space ≥ 2GB
- Internet connection for package installation

---

## Next Steps and Continuous Learning

**Immediate Actions**:
1. Complete Day 1 homework assignment
2. Review and practice R code examples
3. Set up personal R environment following today's guidelines
4. Read Chapter 1-3 of Lohr (2019) for theoretical reinforcement

**Weekly Practice Recommendations**:
- Work with survey package on sample datasets
- Practice documentation and reporting procedures
- Implement quality control checklists
- Explore advanced R visualization techniques

**Professional Development Path**:
1. **Month 1**: Master basic SRS and stratified sampling
2. **Month 2**: Implement cluster sampling and multi-stage designs
3. **Month 3**: Advanced weighting and calibration techniques
4. **Month 6**: Design and implement complete household survey
5. **Year 1**: Contribute to methodology improvement in your statistical office

**Community Engagement**:
- Join R Survey Analysis Users Group
- Participate in SADC Statistical Harmonization Initiative
- Attend annual International Survey Statistics Conference
- Contribute to open-source survey methodology development

---

## Contact and Support

**Instructor Contact**:
- **Dr. Endri Raço, PhD**
- Email: e.raco@fimif.edu.al
- Phone: +355 68 20 61988
- Office Hours: By appointment

**Technical Support**:
- Course Repository: https://github.com/endri81/SADC-Advanced-Sampling-Workshop
- Issue Tracking: Submit issues via GitHub
- Discussion Forum: Course Slack channel
- Emergency Contact: Available during workshop hours

**SADC Project Coordination**:
- Project Reference: SADC/3/5/2/413
- SADC Secretariat Contact: Ms. Mercy Mikuwa
- Project Timeline: November 2025 - February 2026

---

class: center, middle, inverse

# End of Day 1

## Well done! You've completed the foundations of advanced sampling methods.

### See you tomorrow for Stratified Sampling Techniques!

**Remember**: Practice makes perfect. The more you work with these tools, the more proficient you'll become in implementing quality household surveys for your statistical office.

**Final Reminder**: All materials, datasets, and scripts are available in the course GitHub repository. Don't hesitate to reach out with questions between sessions.