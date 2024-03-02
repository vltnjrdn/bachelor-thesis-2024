library(tidyverse)
library(ggpubr)
library(rstatix)

results_JOL_Recall <- compare_Recall_Confidence(result_df[, c("ID", "Condition", "mean_Recall", "mean_Confidence")])

compare_Recall_Confidence <- function(data) {
  # Convert Confidence to the same scale as Recall
  data$mean_Confidence <- data$mean_Confidence / 100
  
  # Initialize lists to store results
  conditions <- unique(data$Condition)
  comparison_results <- list()
  
  # Iterate over each condition
  for (condition in conditions) {
    
    # Subset data for the current condition
    condition_data <- data[data$Condition == condition, ]
    
    condition_data <- condition_data %>%
      gather(key = "group", value = "Percent", mean_Confidence, mean_Recall)
    
    summary_stats <- condition_data %>%
      group_by(group) %>%
      get_summary_stats(Percent, type = "mean_sd")
    
    # Perform pairwise t-test
    t_test_results <- t.test(Percent ~ group, data = condition_data, paired = TRUE)
    
    # Calculate effect sizes (Cohen's d)
    effect_sizes <- condition_data  %>% cohens_d(Percent ~ group, paired = TRUE)
    
    # Store results
    comparison_results[[condition]] <- list(
      summary_stats = summary_stats,
      t_test_results = t_test_results,
      effect_sizes = effect_sizes
    )
    
    # Print results
    cat("Results for condition:", condition, "\n")
    cat("\nSummary_Stats:\n")
    print(summary_stats)
    cat("Pairwise t-test results (Bonferroni corrected):\n")
    print(t_test_results)
    cat("\nEffect sizes (Cohen's d):\n")
    print(effect_sizes)
    cat("\n")
  }
  
  return(comparison_results)
}
