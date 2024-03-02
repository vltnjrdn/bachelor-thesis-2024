# install.packages("ggpubr")
# install.packages("rstatix")

library(car)
library(tibble)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(rstatix)

Analysis_mu_m <- function(all_conditions_list) {
  
  ## repeated measures ANOVA for mu_m
  # Initialize an empty list to store the second columns
  mu_m_list <- list()
  
  # Extract the first column from each data frame in the list
  for (df_name in names(all_conditions_list)) {
    mu_m_list[[df_name]] <- all_conditions_list[[df_name]][[3]]
  }
  
  # Data preparation
  mu_m_df <- as.data.frame(mu_m_list)
  
  mu_m_tibble <- mu_m_df %>%
    mutate(ID = row_number()) %>%
    select(ID, everything()) %>%
    as_tibble()
  
  mu_m_data <- mu_m_tibble %>%
    gather(key = "condition", value = "mu_m", forward, backward, symmetrical) %>%
    convert_as_factor(ID, condition)
  
  # Delete ID 23 (participant 29), for this participant all recall trials are the same
  
  mu_m_data <- mu_m_data %>%
    filter(ID != 23 ) %>%
    mutate(ID = rep(1:(length(ID)/3), length.out = n()))  
  
  # compute summary statistics
  
  summary_stats <- mu_m_data %>%
    group_by(condition) %>%
    get_summary_stats(mu_m, type = "mean_sd")
  
  ## Check assumptions
  
  # check for outliers
  
  outliers <- mu_m_data %>%
    group_by(condition) %>%
    identify_outliers(mu_m) # no extreme outliers found
  
  # assumption of normality
  
  normality <- mu_m_data %>%
    group_by(condition) %>%
    shapiro_test(mu_m)
  
  # Perform ANOVA if normality assumptions hold, otherwise perform Friedman test
  if(all(normality$p > 0.05)) {
    ## repeated measures ANOVA
    
    res_aov <- anova_test(data = mu_m_data, dv = mu_m, wid = ID, within = condition)
    anova_table <- get_anova_table(res_aov)
    
    ## posthoc tests
    
    pwc <- mu_m_data %>%
      pairwise_t_test(
        mu_m ~ condition, paired = TRUE,
        p.adjust.method = "bonferroni"
      ) # significant differences in mu_m when comparing the forward, backward and symmetrical condition to the unrelated condition
    
    # Visualization: box plots with p-values
    bxp <- ggboxplot(mu_m_data, x = "condition", y = "mu_m", add = "point")
    pwc_bxp <- pwc %>% add_xy_position(x = "condition")
    bxp <- bxp +
      stat_pvalue_manual(pwc_bxp) +
      labs(caption = get_pwc_label(pwc_bxp)) +
      ggtitle(bquote(mu[m]))
    
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
    
    res.fried <- mu_m_data %>% friedman_test(mu_m ~ condition |ID)
    
    # effect size
    
    effect_size <- mu_m_data %>% friedman_effsize(mu_m ~ condition |ID)
    
    ## posthoc tests
    
    pwc <- mu_m_data %>%
      pairwise_t_test(
        mu_m ~ condition, paired = TRUE,
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