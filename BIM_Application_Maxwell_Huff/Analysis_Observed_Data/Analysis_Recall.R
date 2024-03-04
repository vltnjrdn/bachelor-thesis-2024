library(car)
library(tibble)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(rstatix)

Analysis_Recall <- function(Recall_df) {
  # prepare data
  Recall_data <- Recall_df %>%
    select(ID, everything()) %>%
    as_tibble()

  # compute summary statistics
  summary_stats <- Recall_data %>%
    group_by(Condition) %>%
    get_summary_stats(mean_Recall, type = "mean_sd")

  ## check assumptions

  # check for outliers
  outliers <- Recall_data %>%
    group_by(Condition) %>%
    identify_outliers(mean_Recall)

  # initialize a variable to store outlier IDs
  outlier_ids <- NULL

  # Check if outliers were found
  if (!is.null(outliers)) {
    # Store outlier IDs
    outlier_ids <- outliers$ID
  }

  # assumption of normality
  normality <- Recall_data %>%
    group_by(Condition) %>%
    shapiro_test(mean_Recall)

  # Perform ANOVA or Friedman test with outliers
  if(all(normality$p > 0.05)) {
    ## repeated measures ANOVA

    res_aov <- anova_test(data = Recall_data, dv = mean_Recall, wid = ID, within = Condition)
    anova_table <- get_anova_table(res_aov)

    # posthoc tests
    pwc <- Recall_data %>%
      pairwise_t_test(
        mean_Recall ~ Condition, paired = TRUE,
        p.adjust.method = "bonferroni"
      ) # significant differences in mean_Recall when comparing the forward, backward and symmetrical Condition to the unrelated Condition

    # Return results
    results_with_outliers <- list(summary_stats = summary_stats,
                                  outliers = outliers,
                                  normality = normality,
                                  anova_table = anova_table,
                                  pairwise_tests = pwc,
                                  test_used = "ANOVA")

  } else {
    ## Friedman Test

    res.fried <- Recall_data %>% friedman_test(mean_Recall ~ Condition |ID)

    # effect size
    effect_size <- Recall_data %>% friedman_effsize(mean_Recall ~ Condition |ID)

    # posthoc tests
    pwc <- Recall_data %>%
      pairwise_t_test(
        mean_Recall ~ Condition, paired = TRUE,
        p.adjust.method = "bonferroni"
      ) # significant differences between every Condition, except for symmetrical and forward

    # Return results
    results_with_outliers <- list(summary_stats = summary_stats,
                                  outliers = outliers,
                                  normality = normality,
                                  friedman_test = res.fried,
                                  effect_size = effect_size,
                                  pairwise_tests = pwc,
                                  test_used = "Friedman")
  }


  # Remove outliers if they were found
  if (length(outlier_ids) > 0) {
    Recall_data_no_outliers <- Recall_data %>%
      filter(!(ID %in% outlier_ids))

    # Check assumptions again after removing outliers
    normality_no_outliers <- Recall_data_no_outliers %>%
      group_by(Condition) %>%
      shapiro_test(mean_Recall)

    # Perform ANOVA or Friedman test without outliers
    if(all(normality_no_outliers$p > 0.05)) {
      ## repeated measures ANOVA

      res_aov_no_outliers <- anova_test(data = Recall_data_no_outliers, dv = mean_Recall, wid = ID, within = Condition)
      anova_table_no_outliers <- get_anova_table(res_aov_no_outliers)

      # posthoc tests
      pwc_no_outliers <- Recall_data_no_outliers %>%
        pairwise_t_test(
          mean_Recall ~ Condition, paired = TRUE,
          p.adjust.method = "bonferroni"
        )

      # Return results without outliers
      results_no_outliers <- list(summary_stats = summary_stats,
                                  outliers_removed = TRUE,
                                  normality = normality_no_outliers,
                                  anova_table = anova_table_no_outliers,
                                  pairwise_tests = pwc_no_outliers,
                                  test_used = "ANOVA")

    } else {
      ## Friedman Test

      res.fried_no_outliers <- Recall_data_no_outliers %>% friedman_test(mean_Recall ~ Condition |ID)

      # effect size
      effect_size_no_outliers <- Recall_data_no_outliers %>% friedman_effsize(mean_Recall ~ Condition |ID)

      # posthoc tests
      pwc_no_outliers <- Recall_data_no_outliers %>%
        pairwise_t_test(
          mean_Recall ~ Condition, paired = TRUE,
          p.adjust.method = "bonferroni"
        ) # significant differences between every Condition, except for symmetrical and forward

      # Return results without outliers
      results_no_outliers <- list(summary_stats = summary_stats,
                                  outliers_removed = TRUE,
                                  normality = normality_no_outliers,
                                  friedman_test = res.fried_no_outliers,
                                  effect_size = effect_size_no_outliers,
                                  pairwise_tests = pwc_no_outliers,
                                  test_used = "Friedman")
    }
  } else {
    results_no_outliers <- NULL
  }

  # Return both results with and without outliers
  results <- list(results_with_outliers = results_with_outliers,
                  results_no_outliers = results_no_outliers)

  return(results)
}
