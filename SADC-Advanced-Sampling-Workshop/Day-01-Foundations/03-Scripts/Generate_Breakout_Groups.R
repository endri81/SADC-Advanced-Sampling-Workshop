# ===============================================================
# BREAKOUT ROOM ASSIGNMENT GENERATOR
# Save as: Day-01-Foundations/03-Scripts/Generate_Breakout_Groups.R
# ===============================================================

# Function to create balanced breakout groups
create_breakout_groups <- function(participant_file = NULL, 
                                   n_participants = 30,
                                   group_size = 5,
                                   skill_balanced = TRUE) {
  
  cat("================================================\n")
  cat("BREAKOUT GROUP ASSIGNMENT GENERATOR\n")
  cat("================================================\n\n")
  
  # Generate or load participant list
  if (is.null(participant_file)) {
    # Generate example participants
    set.seed(2025)
    participants <- data.frame(
      participant_id = sprintf("P%03d", 1:n_participants),
      name = paste("Participant", 1:n_participants),
      country = sample(c("South Africa", "Botswana", "Zimbabwe", "Zambia", 
                         "Namibia", "Mozambique", "Malawi", "Tanzania"),
                       n_participants, replace = TRUE),
      organization = sample(c("National Statistics Office", "Ministry of Health",
                              "Ministry of Finance", "University", "Research Institute"),
                            n_participants, replace = TRUE),
      experience_years = sample(1:15, n_participants, replace = TRUE),
      r_skill = sample(c("Beginner", "Intermediate", "Advanced"), 
                       n_participants, replace = TRUE,
                       prob = c(0.4, 0.4, 0.2)),
      stringsAsFactors = FALSE
    )
  } else {
    participants <- read.csv(participant_file)
  }
  
  # Calculate number of groups
  n_groups <- ceiling(n_participants / group_size)
  
  if (skill_balanced) {
    # Sort by skill level for balanced distribution
    skill_order <- c("Advanced", "Intermediate", "Beginner")
    participants$skill_rank <- match(participants$r_skill, skill_order)
    participants <- participants[order(participants$skill_rank, 
                                       participants$experience_years, 
                                       decreasing = TRUE), ]
    
    # Assign to groups in snake pattern for balance
    group_assignments <- numeric(n_participants)
    for (i in 1:n_participants) {
      row_num <- ceiling(i / n_groups)
      if (row_num %% 2 == 1) {
        # Odd rows: left to right
        group_assignments[i] <- ((i - 1) %% n_groups) + 1
      } else {
        # Even rows: right to left
        group_assignments[i] <- n_groups - ((i - 1) %% n_groups)
      }
    }
    participants$group <- group_assignments
  } else {
    # Random assignment
    participants$group <- sample(rep(1:n_groups, length.out = n_participants))
  }
  
  # Add group names
  group_names <- paste("Group", LETTERS[1:n_groups])
  participants$group_name <- group_names[participants$group]
  
  # Sort by group for output
  participants <- participants[order(participants$group, participants$participant_id), ]
  
  # Generate group summary
  group_summary <- aggregate(
    cbind(n = participant_id, 
          avg_experience = experience_years) ~ group_name,
    data = participants,
    FUN = function(x) {
      if (is.character(x[1])) length(x) else round(mean(x), 1)
    }
  )
  
  # Skill distribution by group
  skill_dist <- table(participants$group_name, participants$r_skill)
  
  # Country distribution by group
  country_dist <- table(participants$group_name, participants$country)
  
  return(list(
    participants = participants,
    group_summary = group_summary,
    skill_distribution = skill_dist,
    country_distribution = country_dist,
    n_groups = n_groups
  ))
}

# Generate breakout groups
groups <- create_breakout_groups(n_participants = 30, 
                                 group_size = 5, 
                                 skill_balanced = TRUE)

# Display results
cat("GROUP ASSIGNMENTS\n")
cat("=================\n\n")

for (g in 1:groups$n_groups) {
  group_name <- paste("Group", LETTERS[g])
  group_members <- groups$participants[groups$participants$group == g, ]
  
  cat(group_name, ":\n")
  cat(rep("-", 50), "\n", sep = "")
  
  for (i in 1:nrow(group_members)) {
    cat(sprintf("  %s - %s (%s, %d years exp, %s)\n",
                group_members$participant_id[i],
                group_members$name[i],
                group_members$country[i],
                group_members$experience_years[i],
                group_members$r_skill[i]))
  }
  cat("\n")
}

# Display summary statistics
cat("\nGROUP BALANCE SUMMARY\n")
cat("=====================\n")
print(groups$group_summary)

cat("\nSKILL DISTRIBUTION BY GROUP\n")
cat("===========================\n")
print(groups$skill_distribution)

# Save assignments to CSV
write.csv(groups$participants, 
          "Day-01-Foundations/Breakout_Group_Assignments.csv",
          row.names = FALSE)

cat("\n✓ Breakout group assignments saved to Breakout_Group_Assignments.csv\n")

# Create exercise rotation schedule
cat("\n\nEXERCISE ROTATION SCHEDULE\n")
cat("==========================\n")

exercises <- c("Sample Size Calculation",
               "SRS Implementation", 
               "Confidence Intervals",
               "Weight Calculation",
               "Quality Assessment")

rotation_schedule <- matrix(NA, nrow = groups$n_groups, ncol = length(exercises))
rownames(rotation_schedule) <- paste("Group", LETTERS[1:groups$n_groups])
colnames(rotation_schedule) <- paste("Exercise", 1:length(exercises))

for (g in 1:groups$n_groups) {
  rotation_schedule[g, ] <- exercises[((g-1):(g+3)) %% length(exercises) + 1]
}

print(rotation_schedule)

# Save rotation schedule
write.csv(rotation_schedule,
          "Day-01-Foundations/Exercise_Rotation_Schedule.csv")

cat("\n✓ Exercise rotation schedule saved\n")