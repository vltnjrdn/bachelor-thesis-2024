library(car)
library(tibble)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(rstatix)

Analysis_Pexp <- function(all_conditions_list) {
  
  ## repeated measures ANOVA for Pexp
  # Initialize an empty list to store the second columns
  Pexp_list <- list()
  
  # Extract the first column from each data frame in the list
  for (df_name in names(all_conditions_list)) {
    Pexp_list[[df_name]] <- all_conditions_list[[df_name]][[1]]
  }
  
  # Data preparation
  Pexp_df <- as.data.frame(Pexp_list)
  
  Pexp_tibble <- Pexp_df %>%
    mutate(ID = row_number()) %>%
    select(ID, everything()) %>%
    as_tibble()
  
  Pexp_data <- Pexp_tibble %>%
    gather(key = "condition", value = "Pexp", forward, backward, symmetrical) %>%
    convert_as_factor(ID, condition)
  
  # compute summary statistics
  
  summary_stats <- Pexp_data %>%
    group_by(condition) %>%
    get_summary_stats(Pexp, type = "mean_sd")
  
  ## Check assumptions
  
  # check for outliers
  
  outliers <- Pexp_data %>%
    group_by(condition) %>%
    identify_outliers(Pexp) # no extreme outliers found
  
  # assumption of normality
  
  normality <- Pexp_data %>%
    group_by(condition) %>%
    shapiro_test(Pexp)
  
  # Perform ANOVA if normality assumptions hold, otherwise perform Friedman test
  if(all(normality$p > 0.05)) {
    ## repeated measures ANOVA
    
    res_aov <- anova_test(data = Pexp_data, dv = Pexp, wid = ID, within = condition)
    anova_table <- get_anova_table(res_aov)
    
    ## posthoc tests
    
    pwc <- Pexp_data %>%
      pairwise_t_test(
        Pexp ~ condition, paired = TRUE,
        p.adjust.method = "bonferroni"
      ) # significant differences in Pexp when comparing the forward, backward and symmetrical condition to the unrelated condition
    
    # Visualization: box plots with p-values
    bxp <- ggboxplot(Pexp_data, x = "condition", y = "Pexp", add = "point")
    
    pwc_bxp <- pwc %>% add_xy_position(x = "condition")
    bxp <- bxp +
      stat_pvalue_manual(pwc_bxp) +
      stat_pvalue_manual(pwc_bxp) +
      ggtitle(expression(P[exp]))
    
    # Return results
    results <- list(summary_stats = summary_stats,
                    outliers = outliers,
                    normality = normality,
                    anova_table = anova_table,
                    pairwise_tests = pwc,
                    test_used = "ANOVA",
                    plot = bxp)
    
  } else {
    ## Friedman Test
    
    res.fried <- Pexp_data %>% friedman_test(Pexp ~ condition |ID)
    
    # effect size
    
    effect_size <- Pexp_data %>% friedman_effsize(Pexp ~ condition |ID)
    
    ## posthoc tests
    
    pwc <- Pexp_data %>%
      pairwise_t_test(
        Pexp ~ condition, paired = TRUE,
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