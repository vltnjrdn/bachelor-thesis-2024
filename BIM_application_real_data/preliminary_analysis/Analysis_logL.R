library(car)
library(tibble)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(rstatix)

Analysis_logL <- function(all_conditions_list) {
  
  ## repeated measures ANOVA for logL
  # Initialize an empty list to store the second columns
  logL_list <- list()
  
  # Extract the first column from each data frame in the list
  for (df_name in names(all_conditions_list)) {
    logL_list[[df_name]] <- all_conditions_list[[df_name]][[5]]
  }
  
  # Data preparation
  logL_df <- as.data.frame(logL_list)
  
  logL_tibble <- logL_df %>%
    mutate(ID = row_number()) %>%
    select(ID, everything()) %>%
    as_tibble()
  
  logL_data <- logL_tibble %>%
    gather(key = "condition", value = "logL", forward, backward, symmetrical) %>%
    convert_as_factor(ID, condition)
  
  # compute summary statistics
  
  summary_stats <- logL_data %>%
    group_by(condition) %>%
    get_summary_stats(logL, type = "mean_sd")
  
  ## Check assumptions
  
  # check for outliers
  
  outliers <- logL_data %>%
    group_by(condition) %>%
    identify_outliers(logL) # no extreme outliers found
  
  # assumption of normality
  
  normality <- logL_data %>%
    group_by(condition) %>%
    shapiro_test(logL)
  
  # Perform ANOVA if normality assumptions hold, otherwise perform Friedman test
  if(all(normality$p > 0.05)) {
    ## repeated measures ANOVA
    
    res_aov <- anova_test(data = logL_data, dv = logL, wid = ID, within = condition)
    anova_table <- get_anova_table(res_aov)
    
    ## posthoc tests
    
    pwc <- logL_data %>%
      pairwise_t_test(
        logL ~ condition, paired = TRUE,
        p.adjust.method = "bonferroni"
      ) # significant differences in logL when comparing the forward, backward and symmetrical condition to the unrelated condition
    
    # Return results
    results <- list(summary_stats = summary_stats,
                    outliers = outliers,
                    normality = normality,
                    anova_table = anova_table,
                    pairwise_tests = pwc,
                    test_used = "ANOVA")
    
  } else {
    ## Friedman Test
    
    res.fried <- logL_data %>% friedman_test(logL ~ condition |ID)
    
    # effect size
    
    effect_size <- logL_data %>% friedman_effsize(logL ~ condition |ID)
    
    ## posthoc tests
    
    pwc <- logL_data %>%
      pairwise_t_test(
        logL ~ condition, paired = TRUE,
        p.adjust.method = "bonferroni"
      ) # significant differences between every condition, except for symmetrical and forward
    
    # Return results
    results <- list(summary_stats = summary_stats,
                    outliers = outliers,
                    normality = normality,
                    friedman_test = res.fried,
                    effect_size = effect_size,
                    pairwise_tests = pwc,
                    test_used = "Friedman")
  }
  
  return(results)
}