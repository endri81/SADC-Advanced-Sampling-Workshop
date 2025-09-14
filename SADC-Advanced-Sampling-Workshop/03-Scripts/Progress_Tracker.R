# ===============================================================
# PARTICIPANT PROGRESS TRACKING DASHBOARD
# Save as: Day-01-Foundations/03-Scripts/Progress_Tracker.R
# ===============================================================

# Function to track participant progress
track_progress <- function() {
  
  cat("================================================\n")
  cat("DAY 1 PROGRESS TRACKING DASHBOARD\n")
  cat("================================================\n\n")
  
  # Initialize progress tracking matrix
  modules <- c("Setup", "Introduction", "Theory", "SRS", "Sample Size",
               "Estimation", "Weights", "Quality", "Application")
  
  # Get participant list
  if (file.exists("Day-01-Foundations/Breakout_Group_Assignments.csv")) {
    participants <- read.csv("Day-01-Foundations/Breakout_Group_Assignments.csv")
  } else {
    # Create sample participants
    participants <- data.frame(
      participant_id = sprintf("P%03d", 1:30),
      name = paste("Participant", 1:30),
      group_name = rep(paste("Group", LETTERS[1:6]), each = 5)
    )
  }
  
  n_participants <- nrow(participants)
  n_modules <- length(modules)
  
  # Initialize progress matrix (0 = not started, 1 = in progress, 2 = complete)
  progress_matrix <- matrix(0, nrow = n_participants, ncol = n_modules)
  rownames(progress_matrix) <- participants$participant_id
  colnames(progress_matrix) <- modules
  
  # Simulate some progress (in real use, this would be updated throughout the day)
  set.seed(Sys.time())
  for (i in 1:n_participants) {
    # Everyone completes setup
    progress_matrix[i, 1] <- 2
    
    # Varying progress through modules
    completed <- sample(2:7, 1)
    if (completed > 1) {
      progress_matrix[i, 2:min(completed, n_modules)] <- 2
    }
    if (completed < n_modules) {
      progress_matrix[i, min(completed + 1, n_modules)] <- sample(0:1, 1)
    }
  }
  
  # Calculate summary statistics
  module_completion <- colSums(progress_matrix == 2) / n_participants * 100
  participant_progress <- rowSums(progress_matrix == 2) / n_modules * 100
  
  # Overall statistics
  overall_stats <- list(
    total_participants = n_participants,
    avg_progress = mean(participant_progress),
    min_progress = min(participant_progress),
    max_progress = max(participant_progress),
    modules_fully_complete = sum(module_completion == 100),
    at_risk_participants = sum(participant_progress < 50)
  )
  
  # Group statistics
  participants$individual_progress <- participant_progress
  group_stats <- aggregate(individual_progress ~ group_name, 
                           data = participants, 
                           FUN = function(x) c(mean = mean(x), 
                                               min = min(x), 
                                               max = max(x)))
  
  return(list(
    progress_matrix = progress_matrix,
    module_completion = module_completion,
    participant_progress = participant_progress,
    overall_stats = overall_stats,
    group_stats = group_stats
  ))
}

# Generate progress report
progress <- track_progress()

# Display dashboard
cat("OVERALL PROGRESS STATISTICS\n")
cat("===========================\n")
cat("Total Participants:", progress$overall_stats$total_participants, "\n")
cat("Average Progress:", round(progress$overall_stats$avg_progress, 1), "%\n")
cat("Range:", round(progress$overall_stats$min_progress, 1), "% -", 
    round(progress$overall_stats$max_progress, 1), "%\n")
cat("At Risk (<50%):", progress$overall_stats$at_risk_participants, "participants\n\n")

cat("MODULE COMPLETION RATES\n")
cat("=======================\n")
module_report <- data.frame(
  Module = names(progress$module_completion),
  Completion = paste0(round(progress$module_completion, 1), "%")
)
print(module_report, row.names = FALSE)

cat("\nGROUP PERFORMANCE\n")
cat("=================\n")
print(progress$group_stats)

# Create visual progress chart
create_progress_chart <- function(progress_matrix) {
  # Convert to data frame for plotting
  progress_df <- as.data.frame(progress_matrix)
  progress_df$participant <- rownames(progress_df)
  
  # Reshape for plotting
  library(reshape2)
  progress_long <- melt(progress_df, id.vars = "participant", 
                        variable.name = "module", value.name = "status")
  
  # Set status labels
  progress_long$status_label <- factor(progress_long$status,
                                       levels = c(0, 1, 2),
                                       labels = c("Not Started", "In Progress", "Complete"))
  
  # Create heatmap
  library(ggplot2)
  p <- ggplot(progress_long, aes(x = module, y = participant, fill = status_label)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_manual(values = c("Not Started" = "#f0f0f0",
                                 "In Progress" = "#ffeb3b", 
                                 "Complete" = "#4caf50")) +
    labs(title = "Day 1: Participant Progress Tracker",
         x = "Module",
         y = "Participant",
         fill = "Status") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(size = 8))
  
  return(p)
}

# Generate and save progress chart
library(ggplot2)
progress_chart <- create_progress_chart(progress$progress_matrix)
ggsave("Day-01-Foundations/06-Outputs/Progress_Chart.png", 
       progress_chart, width = 10, height = 8, dpi = 150)

cat("\n✓ Progress chart saved to 06-Outputs/Progress_Chart.png\n")

# Identify participants needing support
at_risk <- which(progress$participant_progress < 50)
if (length(at_risk) > 0) {
  cat("\n⚠ PARTICIPANTS NEEDING SUPPORT\n")
  cat("===============================\n")
  for (i in at_risk) {
    cat(sprintf("%s: %.1f%% complete\n", 
                names(progress$participant_progress)[i],
                progress$participant_progress[i]))
  }
}

# Save progress report
progress_report <- list(
  timestamp = Sys.time(),
  overall_stats = progress$overall_stats,
  module_completion = progress$module_completion,
  group_stats = progress$group_stats
)

saveRDS(progress_report, 
        file = paste0("Day-01-Foundations/06-Outputs/Progress_Report_",
                      format(Sys.time(), "%Y%m%d_%H%M"),
                      ".rds"))

cat("\n✓ Progress report saved\n")