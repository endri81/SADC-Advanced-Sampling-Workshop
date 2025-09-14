---
  title: "Day 5: Special Topics and Future Directions"
subtitle: "Part 1: Modern Innovations in Survey Sampling (Slides 1-75)"
author: "SADC Survey Sampling Workshop"
date: "`r Sys.Date()`"
output:
  xaringan::moon_reader:
  lib_dir: libs
css: [default, default-fonts]
nature:
  highlightStyle: github
highlightLines: true
countIncrementalSlides: false
ratio: "16:9"
slideNumberFormat: "%current%/%total%"
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE, 
  warning = FALSE, 
  message = FALSE,
  fig.width = 10, 
  fig.height = 4,
  fig.align = "center",
  cache = FALSE,
  comment = "#>"
)

# Load packages
library(tidyverse)
library(survey)
library(knitr)
library(kableExtra)
library(sampling)
library(lme4)

# Set seed for reproducibility
set.seed(2025)

# Create datasets for demonstrations
n_obs <- 10000

# Big data simulation
big_data <- data.frame(
  id = 1:n_obs,
  age = sample(18:80, n_obs, replace = TRUE),
  digital_trace = rpois(n_obs, lambda = 50),
  mobile_usage = rlnorm(n_obs, 3, 1),
  social_media = rbinom(n_obs, 1, 0.7),
  location_pings = rpois(n_obs, lambda = 100),
  transactions = rpois(n_obs, lambda = 20)
)

# Mixed mode data
mixed_mode <- data.frame(
  id = 1:1000,
  mode = sample(c("Web", "Phone", "F2F", "Mobile"), 1000, 
                replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
  response_time = c(rnorm(400, 15, 5), rnorm(300, 25, 8), 
                    rnorm(200, 45, 10), rnorm(100, 10, 3)),
  data_quality = c(rnorm(400, 0.85, 0.1), rnorm(300, 0.90, 0.08),
                   rnorm(200, 0.95, 0.05), rnorm(100, 0.80, 0.15))
)
```

---
  class: center, middle, inverse

# Day 5: Special Topics and Future Directions

## The Future of Survey Sampling

### "Innovation meets tradition"

---
  
  ## Welcome to Day 5!
  
  ### Final Day Agenda:
  
  Part 1: Modern Innovations (08:00-09:30)  
Part 2: Big Data Integration (09:45-11:15)  
Part 3: Machine Learning Applications (11:30-13:00)  
Part 4: Quality in Digital Age (14:00-15:30)  
Part 5: Future Directions & Wrap-up (15:45-17:00)  

**Bottom line:** Future-ready skills

---
  
  ## Journey Recap
  
  ```{r journey_recap}
journey <- data.frame(
  Day = paste("Day", 1:5),
  Focus = c("Foundations & SRS", "Stratification", "Clustering", 
            "Complex Designs", "Future & Innovation"),
  Key_Skills = c("Basic sampling", "Efficiency gains", "Practical designs",
                 "Integration", "Modern methods"),
  Readiness = c("20%", "40%", "60%", "80%", "100%")
)

kable(journey) %>%
  kable_styling(bootstrap_options = "striped", font_size = 10)
```

**Bottom line:** From basics to cutting-edge

---
  
  ## Today's Learning Objectives
  
  ### By end of Day 5, you will:
  
  1. **Understand** big data integration with surveys
2. **Apply** machine learning to sampling
3. **Design** mixed-mode strategies
4. **Evaluate** digital data quality
5. **Plan** future-ready surveys

**Bottom line:** Modern toolkit complete

---
  
  ## The Changing Landscape
  
  ```{r changing_landscape, fig.height=3.5}
# Timeline of survey evolution
timeline <- data.frame(
  Year = c(1940, 1960, 1980, 2000, 2010, 2020, 2025),
  Method = c("F2F only", "Phone enters", "CATI/CAPI", 
             "Web surveys", "Mobile", "AI-assisted", "Hybrid"),
  Response_Rate = c(95, 90, 75, 60, 45, 30, 35),
  Cost_Index = c(100, 80, 60, 40, 30, 25, 20)
)

timeline %>%
  pivot_longer(c(Response_Rate, Cost_Index), names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = Year, y = Value, color = Metric)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Evolution of Survey Methods",
       y = "Index Value")
```

**Bottom line:** Dramatic transformation

---
  
  ## Modern Challenges
  
  ```{r modern_challenges}
challenges <- data.frame(
  Challenge = c("Declining response rates", "Coverage issues", 
                "Mode effects", "Data privacy", "Cost pressures"),
  Traditional_Impact = c("High", "Medium", "Low", "Low", "Medium"),
  Current_Impact = c("Critical", "Critical", "High", "Critical", "High"),
  Solution_Available = c("Partial", "Partial", "Yes", "Emerging", "Yes")
)

kable(challenges) %>%
  kable_styling(font_size = 10)
```

**Bottom line:** New solutions needed

---
  
  ## Digital Data Explosion
  
  ```{r data_explosion, fig.height=3.5}
# Data volume growth
years <- 2010:2025
traditional <- rep(1, length(years))
digital <- exp((years - 2010) * 0.3)
combined <- traditional + digital

data.frame(
  Year = rep(years, 3),
  Volume = c(traditional, digital, combined),
  Type = rep(c("Traditional", "Digital", "Combined"), each = length(years))
) %>%
  ggplot(aes(x = Year, y = Volume, fill = Type)) +
  geom_area(alpha = 0.7, position = "identity") +
  theme_minimal() +
  labs(y = "Relative Data Volume",
       title = "Data Source Evolution")
```

**Bottom line:** Digital dominates volume

---
  
  ## Big Data vs Surveys
  
  ```{r bigdata_vs_surveys}
comparison <- data.frame(
  Aspect = c("Volume", "Velocity", "Coverage", "Accuracy", 
             "Representation", "Designed", "Cost"),
  Big_Data = c("Massive", "Real-time", "Partial", "Variable", 
               "Biased", "No", "Low marginal"),
  Surveys = c("Limited", "Periodic", "Controlled", "High", 
              "Representative", "Yes", "High marginal"),
  Hybrid = c("Large", "Frequent", "Enhanced", "High", 
             "Improved", "Partially", "Moderate")
)

kable(comparison) %>%
  kable_styling(font_size = 9)
```

**Bottom line:** Complementary strengths

---
  
  ## Data Integration Framework
  
  ```{r integration_framework}
cat("Integration Approaches:

1. SUBSTITUTION
   Big data replaces surveys
   Example: Scanner data for CPI

2. SUPPLEMENTATION  
   Big data adds variables
   Example: GPS mobility patterns

3. TRANSFORMATION
   Combined new products
   Example: Nowcasting models

4. VALIDATION
   Cross-checking sources
   Example: Employment verification")
```

**Bottom line:** Multiple integration modes

---
  
  ## Probability vs Non-Probability
  
  ```{r prob_nonprob, fig.height=3.5}
# Compare sample types
set.seed(2025)
prob_sample <- rnorm(500, 50, 10)
nonprob_sample <- rnorm(500, 55, 8)  # Biased upward

data.frame(
  Value = c(prob_sample, nonprob_sample),
  Type = rep(c("Probability", "Non-probability"), each = 500)
) %>%
  ggplot(aes(x = Value, fill = Type)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = 50, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Selection Bias in Non-probability Samples",
       subtitle = "True population mean = 50")
```

**Bottom line:** Non-probability biased

---
  
  ## Web Panel Challenges
  
  ```{r web_panels}
panel_issues <- data.frame(
  Issue = c("Self-selection", "Professional respondents", "Attrition bias",
            "Device effects", "Satisficing"),
  Prevalence = c("Very High", "High", "High", "Medium", "High"),
  Impact = c("Representation", "Data quality", "Longitudinal", 
             "Measurement", "Validity"),
  Mitigation = c("Quotas + weighting", "Quality checks", "Refreshment",
                 "Responsive design", "Attention checks")
)

kable(panel_issues) %>%
  kable_styling(font_size = 9)
```

**Bottom line:** Quality concerns serious

---
  
  ## Mobile Surveys
  
  ```{r mobile_surveys}
cat("Mobile Survey Advantages:

✓ Broad coverage (>90% in many countries)
✓ Real-time data collection
✓ GPS and sensor data
✓ Lower costs
✓ Multimedia capabilities

Challenges:

✗ Screen size limitations
✗ Completion rates lower
✗ Data costs for respondents
✗ Technical literacy required
✗ Device fragmentation")
```

**Bottom line:** Promising but complex

---
  
  ## SMS vs App Surveys
  
  ```{r sms_vs_app}
mobile_comparison <- data.frame(
  Feature = c("Reach", "Cost", "Data types", "Completion", "Setup"),
  SMS = c("Universal", "Very low", "Text only", "High", "Simple"),
  App = c("Smartphone only", "Low", "Rich media", "Medium", "Complex"),
  WhatsApp = c("High", "Very low", "Media + text", "High", "Simple")
)

kable(mobile_comparison) %>%
  kable_styling(font_size = 10)
```

**Bottom line:** Platform matters

---
  
  ## Knowledge Check #1
  
  ### Quick review:
  
  1. Response rates have _______ since 1940
2. Big data is _______ for representation  
3. Mobile coverage is > _______% in many countries
4. Non-probability samples have _______ bias

Think first!
  
  **Bottom line:** declined, biased, 90, selection

---
  
  ## Sensor Data Integration
  
  ```{r sensor_data}
# Types of sensor data
sensors <- data.frame(
  Type = c("GPS", "Accelerometer", "Air quality", "Smart meter", "Wearable"),
  Data = c("Location", "Movement", "Environment", "Energy use", "Health"),
  Frequency = c("Continuous", "Continuous", "Hourly", "15-min", "Real-time"),
  Privacy = c("High concern", "Medium", "Low", "Medium", "High concern"),
  Value = c("Mobility", "Activity", "Exposure", "Consumption", "Wellness")
)

kable(sensors) %>%
  kable_styling(font_size = 9)
```

**Bottom line:** Rich passive data

---
  
  ## Social Media Mining
  
  ```{r social_media, fig.height=3.5}
# Sentiment analysis example
posts <- data.frame(
  Platform = rep(c("Twitter", "Facebook", "Instagram"), each = 100),
  Sentiment = c(rnorm(100, 0, 1), rnorm(100, 0.5, 1.2), rnorm(100, 0.3, 0.8)),
  Volume = c(rpois(100, 50), rpois(100, 30), rpois(100, 40))
)

ggplot(posts, aes(x = Platform, y = Sentiment)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Sentiment Varies by Platform",
       y = "Sentiment Score")
```

**Bottom line:** Platform-specific biases

---
  
  ## Scanner and Transaction Data
  
  ```{r transaction_data}
cat("Transaction Data Applications:

Consumer Price Index:
- Real-time price tracking
- Millions of products
- Quality adjustment possible

Household Consumption:
- Actual purchases vs reported
- Complete expenditure picture
- Links to loyalty cards

Economic Indicators:
- Retail sales nowcasting
- Consumer confidence proxy
- Regional variations")
```

**Bottom line:** Objective behavioral data

---
  
  ## Administrative Data
  
  ```{r admin_data}
admin_sources <- data.frame(
  Source = c("Tax records", "Health records", "Education", 
             "Employment", "Benefits"),
  Coverage = c("Workers", "Universal", "Students", "Formal sector", "Recipients"),
  Quality = c("High", "High", "High", "Medium", "High"),
  Timeliness = c("Annual", "Real-time", "Annual", "Monthly", "Monthly"),
  Access = c("Restricted", "Very restricted", "Restricted", "Limited", "Limited")
)

kable(admin_sources) %>%
  kable_styling(font_size = 9)
```

**Bottom line:** High quality, access challenges

---
  
  ## Record Linkage
  
  ```{r record_linkage}
# Linkage quality by method
linkage_methods <- data.frame(
  Method = c("Exact ID", "Probabilistic", "Statistical matching", "ML-based"),
  Match_Rate = c("95%", "85%", "70%", "88%"),
  False_Positive = c("0.1%", "2%", "5%", "1%"),
  Complexity = c("Low", "Medium", "High", "High"),
  Privacy_Safe = c("No", "Partial", "Yes", "Partial")
)

kable(linkage_methods) %>%
  kable_styling(font_size = 10)
```

**Bottom line:** Trade-offs in methods

---
  
  ## Privacy-Preserving Linkage
  
  ```{r privacy_linkage}
cat("Privacy-Preserving Techniques:

1. Secure Multi-party Computation
   - Compute on encrypted data
   - No party sees individual records

2. Differential Privacy
   - Add calibrated noise
   - Guarantee privacy bounds

3. Homomorphic Encryption
   - Operations on encrypted data
   - Decrypt only results

4. Federated Learning
   - Train models locally
   - Share only parameters")
```

**Bottom line:** Privacy-utility balance

---
  
  ## Energy Check
  
  ### ⚡ Quick energizer (30 seconds):
  
  1. Stand and stretch
2. Look away from screen
3. Deep breaths × 3
4. Think: One digital data source you'd like to use

Share with neighbor!

**Bottom line:** Stay focused!

---

## Satellite and Remote Sensing

```{r satellite_data, fig.height=3.5}
# Applications of satellite data
applications <- data.frame(
  Application = c("Crop yield", "Urban growth", "Poverty mapping", 
                 "Disaster impact", "Deforestation"),
  Accuracy = c(85, 90, 70, 95, 92),
  Cost = c(20, 15, 25, 10, 18),
  Traditional_Cost = c(100, 100, 100, 100, 100)
)

applications %>%
  pivot_longer(c(Cost, Traditional_Cost), names_to = "Method", values_to = "Cost") %>%
  ggplot(aes(x = Application, y = Cost, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Relative Cost", title = "Satellite vs Traditional Survey Costs")
```

**Bottom line:** Cost-effective for area estimates

---

## Network Data

```{r network_data}
# Mobile network analysis
network_metrics <- data.frame(
  Metric = c("Call patterns", "Movement flows", "Social contacts",
            "Economic activity", "Emergency detection"),
  Data_Source = c("CDR", "Cell towers", "Contact graphs", 
                 "Mobile money", "Anomaly detection"),
  Update = c("Daily", "Real-time", "Weekly", "Daily", "Real-time"),
  Survey_Equivalent = c("Social survey", "Transport survey", "Network survey",
                       "Income survey", "None")
)

kable(network_metrics) %>%
  kable_styling(font_size = 9)
```

**Bottom line:** Behavioral insights at scale

---

## Web Scraping

```{r web_scraping}
cat("Web Scraping Applications:

Price Monitoring:
- Real estate listings
- Job advertisements  
- Product prices
- Service rates

Labor Market:
- Job postings analysis
- Skill demands
- Wage trends
- Regional variations

Economic Indicators:
- Restaurant bookings
- Hotel occupancy
- Flight searches
- Retail availability")
```

**Bottom line:** Real-time economic pulse

---

## Internet of Things (IoT)

```{r iot_integration, fig.height=3.5}
# IoT data volume projection
years <- 2020:2030
devices <- c(10, 15, 22, 33, 48, 70, 100, 145, 200, 280, 380)
data_volume <- devices^1.5

data.frame(Year = years, Devices = devices, Volume = data_volume) %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y = Devices), color = "blue", size = 1.2) +
  geom_line(aes(y = Volume/10), color = "red", size = 1.2) +
  scale_y_continuous(
    name = "Billions of Devices",
    sec.axis = sec_axis(~.*10, name = "Data Volume (ZB)")
  ) +
  theme_minimal() +
  labs(title = "IoT Growth Projection")
```

**Bottom line:** Exponential data growth

---

## Combining Probability and Big Data

```{r combining_sources}
# Framework for combination
combination <- data.frame(
  Approach = c("Calibration", "Multiple frame", "Model-based", "Hybrid design"),
  Description = c("Weight to big data totals", "Overlap management",
                 "Big data as auxiliary", "Sequential mixed"),
  Example = c("Weight to Google trends", "Phone + address frames",
             "SAE with satellite", "Screen then survey"),
  Complexity = c("Low", "Medium", "High", "Medium")
)

kable(combination) %>%
  kable_styling(font_size = 9)
```

**Bottom line:** Multiple combination strategies

---

## Case Study: Google Mobility

```{r google_mobility, fig.height=3.5}
# Mobility data during COVID
dates <- seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "week")
baseline <- 100
lockdown <- c(rep(100, 10), rep(40, 8), seq(45, 85, length.out = 20), rep(90, 15))
survey <- c(rep(100, 10), rep(35, 8), seq(40, 80, length.out = 20), rep(85, 15))

data.frame(
  Date = rep(dates, 2),
  Mobility = c(lockdown, survey),
  Source = rep(c("Google", "Survey"), each = length(dates))
) %>%
  ggplot(aes(x = Date, y = Mobility, color = Source)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  labs(y = "Mobility Index", title = "Comparing Mobility Data Sources")
```

**Bottom line:** High correlation, different levels

---

## Nowcasting with Mixed Data

```{r nowcasting}
cat("Nowcasting Framework:

Inputs:
- Survey data (lagged, high quality)
- Big data (real-time, noisy)
- Admin data (accurate, delayed)

Model:
State Space or Mixed Frequency
- Handle different frequencies
- Weight by reliability
- Uncertainty quantification

Output:
- Current estimate
- Confidence intervals
- Revision history")
```

**Bottom line:** Timely estimates possible

---

## Machine Learning for Sampling

```{r ml_sampling}
ml_applications <- data.frame(
  Task = c("Frame construction", "Stratification", "Response propensity",
          "Imputation", "Coding", "Quality control"),
  Traditional = c("Manual listing", "Geographic", "Logistic regression",
                 "Hot deck", "Manual", "Rules-based"),
  ML_Method = c("Satellite + CV", "Clustering", "Random Forest",
               "Deep learning", "NLP", "Anomaly detection"),
  Improvement = c("90% cost reduction", "20% variance reduction", 
                 "15% accuracy gain", "30% better", "95% automated", "2x detection")
)

kable(ml_applications) %>%
  kable_styling(font_size = 8)
```

**Bottom line:** ML enhances every step

---

## Adaptive Sampling with AI

```{r adaptive_ai, fig.height=3.5}
# Simulated adaptive sampling
iterations <- 1:20
random_eff <- 100 - 2 * iterations
adaptive_eff <- 100 * exp(-iterations/10)

data.frame(
  Iteration = rep(iterations, 2),
  Efficiency = c(random_eff, adaptive_eff),
  Method = rep(c("Random", "AI-Adaptive"), each = 20)
) %>%
  ggplot(aes(x = Iteration, y = Efficiency, color = Method)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  labs(y = "Cost per Quality Unit",
       title = "AI-Adaptive Sampling Efficiency")
```

**Bottom line:** AI optimizes resource use

---

## Natural Language Processing

```{r nlp_applications}
cat("NLP in Surveys:

Automated Coding:
- Open-ended responses
- Occupation/industry coding  
- Topic modeling
- Sentiment analysis

Quality Control:
- Inconsistency detection
- Gibberish identification
- Copy-paste detection
- Language verification

Chatbot Interviews:
- Natural conversation
- Adaptive questioning
- 24/7 availability
- Multi-language support")
```

**Bottom line:** Text analysis automated

---

## Computer Vision Applications

```{r computer_vision}
cv_uses <- data.frame(
  Application = c("Dwelling quality", "Crop classification", 
                 "Traffic counting", "Retail monitoring"),
  Data = c("Street view", "Satellite", "Camera feeds", "Store video"),
  Accuracy = c("85%", "92%", "95%", "88%"),
  vs_Survey = c("Similar", "Better", "Better", "Similar"),
  Cost_Ratio = c("1:10", "1:20", "1:50", "1:30")
)

kable(cv_uses) %>%
  kable_styling(font_size = 10)
```

**Bottom line:** Visual data valuable

---

## Blockchain for Surveys

```{r blockchain}
blockchain_benefits <- data.frame(
  Feature = c("Immutable responses", "Instant incentives", "Consent management",
             "Data marketplace", "Audit trail"),
  Benefit = c("Trust", "Response rate", "GDPR compliance", 
             "Monetization", "Transparency"),
  Challenge = c("Scalability", "Volatility", "Complexity", 
               "Regulation", "Energy use"),
  Maturity = c("Pilot", "Operational", "Testing", "Concept", "Operational")
)

kable(blockchain_benefits) %>%
  kable_styling(font_size = 9)
```

**Bottom line:** Promising but early

---

## Synthetic Data Generation

```{r synthetic_data, fig.height=3.5}
# Compare real vs synthetic
set.seed(2025)
real <- data.frame(x = rnorm(100), y = rnorm(100))
real$group <- ifelse(real$x + real$y > 0, "A", "B")

synthetic <- data.frame(
  x = rnorm(100, mean(real$x), sd(real$x)),
  y = rnorm(100, mean(real$y), sd(real$y))
)
synthetic$group <- ifelse(synthetic$x + synthetic$y > 0.1, "A", "B")

rbind(
  mutate(real, Type = "Real"),
  mutate(synthetic, Type = "Synthetic")
) %>%
  ggplot(aes(x = x, y = y, color = group)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~Type) +
  theme_minimal() +
  labs(title = "Real vs Synthetic Data")
```

**Bottom line:** Preserves patterns, protects privacy

---

## Digital Divide Concerns

```{r digital_divide}
divide_issues <- data.frame(
  Dimension = c("Access", "Skills", "Usage", "Quality", "Relevance"),
  Young_Urban = c("95%", "High", "Intensive", "High", "High"),
  Elderly_Rural = c("40%", "Low", "Minimal", "Low", "Low"),
  Impact = c("Coverage bias", "Non-response", "Mode effects", 
            "Measurement error", "Missing data")
)

kable(divide_issues) %>%
  kable_styling(font_size = 10)
```

**Bottom line:** Digital not universal

---

## Exercise: Design Mixed Mode

### Your task (5 minutes):

Design a mixed-mode survey combining:
- Traditional survey (n=1000)
- Big data source
- Admin records

For measuring:
- Employment status
- Income
- Consumption

Sketch your approach!

**Bottom line:** Practice integration

---

## Exercise Solution

```{r exercise_solution}
solution <- data.frame(
  Component = c("Survey", "Mobile data", "Tax records", "Scanner data"),
  Measure = c("All variables", "Work location", "Formal income", "Consumption"),
  Frequency = c("Quarterly", "Continuous", "Annual", "Daily"),
  Coverage = c("Representative", "80% population", "Formal sector", "Urban retail"),
  Integration = c("Base", "Auxiliary", "Validation", "Supplement")
)

kable(solution) %>%
  kable_styling(font_size = 9)
```

**Bottom line:** Each source contributes

---

## Group Discussion

### Share insights (3 minutes):

1. What digital data exists in your country?
2. Main barriers to using it?
3. Most promising application?

Report key insight!

**Bottom line:** Learn from peers

---

## Quality in Digital Age

```{r digital_quality}
cat("Digital Era Quality Framework:

Accuracy:
- Measurement validity
- Algorithmic bias
- Representation errors

Timeliness:
- Real-time processing
- Nowcasting accuracy
- Revision policies

Coherence:
- Cross-source consistency
- Time series breaks
- Geographic comparability

Accessibility:
- Machine readable
- API availability
- Documentation quality")
```

**Bottom line:** Quality redefined

---

## Algorithmic Bias

```{r algorithmic_bias, fig.height=3.5}
# Simulate biased algorithm
true_need <- rnorm(1000, 50, 15)
group <- rbinom(1000, 1, 0.3)
predicted <- true_need + ifelse(group == 1, -5, 5) + rnorm(1000, 0, 5)

data.frame(
  True = true_need,
  Predicted = predicted,
  Group = factor(group, labels = c("Majority", "Minority"))
) %>%
  ggplot(aes(x = True, y = Predicted, color = Group)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Algorithmic Bias Against Minority Group")
```

**Bottom line:** Bias amplified by ML

---

## Ethical Considerations

```{r ethics}
ethical_framework <- data.frame(
  Principle = c("Informed consent", "Privacy", "Transparency", 
               "Fairness", "Accountability"),
  Traditional = c("Clear", "Protected", "Documented", "Designed", "Clear chain"),
  Digital = c("Complex", "Vulnerable", "Black box", "Hidden bias", "Diffuse"),
  Solution = c("Layered consent", "Privacy by design", "Explainable AI",
              "Bias audits", "Governance frameworks")
)

kable(ethical_framework) %>%
  kable_styling(font_size = 9)
```

**Bottom line:** Ethics more complex

---

## Regulatory Landscape

```{r regulations}
cat("Key Regulations Impacting Surveys:

GDPR (Europe):
- Explicit consent required
- Right to deletion
- Data minimization
- Privacy by design

CCPA (California):
- Consumer rights
- Opt-out provision
- Data broker registration

National Laws:
- Data localization
- Sector-specific rules
- Cross-border restrictions

Emerging:
- AI regulations
- Algorithmic accountability")
```

**Bottom line:** Compliance critical

---

## Part 1 Summary

### You've learned:
  
  ✅ Big data integration approaches  
✅ Digital data sources  
✅ Privacy-preserving methods  
✅ ML applications in sampling  
✅ Ethical considerations  

**Bottom line:** Digital transformation understood!
  
  ---
  
  ## Key Messages
  
  ```{r key_messages}
messages <- data.frame(
  Number = 1:5,
  Message = c("Big data complements, doesn't replace surveys",
              "Quality concerns different for digital data",
              "Privacy and ethics paramount",
              "ML enhances but needs oversight",
              "Digital divide affects representation")
)

kable(messages) %>%
  kable_styling(font_size = 10)
```

**Bottom line:** Balance innovation with rigor

---
  
  ## Quick Review
  
  ### Test yourself:
  
  1. Big data is _______ for coverage
2. ML can reduce costs by _______%
3. Privacy-preserving technique: _______
4. Digital divide affects _______

**Bottom line:** partial, 90, differential privacy, representation

---
  
  ## Break Time!
  
  ## ☕ 15-Minute Break
  
  ### Coming next:
  - Part 2: Deep dive into big data
- Hands-on integration
- Case studies

### Challenge:
Think of one survey to enhance with big data

**Bottom line:** Rest and reflect

---
  
  class: center, middle, inverse

# End of Part 1

## Modern Innovations Covered

### Next: Big Data Integration Deep Dive

---