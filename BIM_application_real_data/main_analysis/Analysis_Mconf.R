  library(car)
  library(tibble)
  library(dplyr)
  library(tidyverse)
  library(ggpubr)
  library(rstatix)
  
  Analysis_Mconf <- function(all_conditions_list) {
    
    ## repeated measures ANOVA for Mconf
    # Initialize an empty list to store the second columns
    Mconf_list <- list()
    
    # Extract the first column from each data frame in the list
    for (df_name in names(all_conditions_list)) {
      Mconf_list[[df_name]] <- all_conditions_list[[df_name]][[2]]
    }
    
    # Data preparation
    Mconf_df <- as.data.frame(Mconf_list)
    
    Mconf_tibble <- Mconf_df %>%
      mutate(ID = row_number()) %>%
      select(ID, everything()) %>%
      as_tibble()
    
    Mconf_data <- Mconf_tibble %>%
      gather(key = "condition", value = "Mconf", forward, backward, symmetrical) %>%
      convert_as_factor(ID, condition)
    
    # compute summary statistics
    
    summary_stats <- Mconf_data %>%
      group_by(condition) %>%
      get_summary_stats(Mconf, type = "mean_sd")
    
    ## Check assumptions
    
    # check for outliers
    
    outliers <- Mconf_data %>%
      group_by(condition) %>%
      identify_outliers(Mconf) # no extreme outliers found
    
    # Check for outliers
    outliers <- Mconf_data %>%
      group_by(condition) %>%
      identify_outliers(Mconf)
    
    # Initialize a variable to store outlier IDs
    outlier_ids <- NULL
    
    # Check if outliers were found
    if (!is.null(outliers)) {
      # Store outlier IDs
      outlier_ids <- outliers$ID
    }
    
    # assumption of normality
    
    normality <- Mconf_data %>%
      group_by(condition) %>%
      shapiro_test(Mconf)
    
    # Perform ANOVA or Friedman test with outliers
    if(all(normality$p > 0.05)) {
      ## repeated measures ANOVA
      
      res_aov <- anova_test(data = Mconf_data, dv = Mconf, wid = ID, within = condition)
      anova_table <- get_anova_table(res_aov)
      
      ## posthoc tests
      
      pwc <- Mconf_data %>%
        pairwise_t_test(
          Mconf ~ condition, paired = TRUE,
          p.adjust.method = "bonferroni"
        ) 
      
      # Return results
      results_with_outliers <- list(summary_stats = summary_stats,
                                    outliers = outliers,
                                    normality = normality,
                                    anova_table = anova_table,
                                    pairwise_tests = pwc,
                                    test_used = "ANOVA")
      
    } else {
      ## Friedman Test
      
      res.fried <- Mconf_data %>% friedman_test(Mconf ~ condition |ID)
      
      # effect size
      
      effect_size <- Mconf_data %>% friedman_effsize(Mconf ~ condition |ID)
      
      ## posthoc tests
      
      pwc <- Mconf_data %>%
        pairwise_t_test(
          Mconf ~ condition, paired = TRUE,
          p.adjust.method = "bonferroni"
        ) # significant differences between every condition, except for symmetrical and forward
      
      
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
      Mconf_data_no_outliers <- Mconf_data %>%
        filter(!(ID %in% outlier_ids))
      
      # Check assumptions again after removing outliers
      normality_no_outliers <- Mconf_data_no_outliers %>%
        group_by(condition) %>%
        shapiro_test(Mconf)
      
      # Perform ANOVA or Friedman test without outliers
      if(all(normality_no_outliers$p > 0.05)) {
        ## repeated measures ANOVA
        
        res_aov_no_outliers <- anova_test(data = Mconf_data_no_outliers, dv = Mconf, wid = ID, within = condition)
        anova_table_no_outliers <- get_anova_table(res_aov_no_outliers)
        
        ## posthoc tests
        
        pwc_no_outliers <- Mconf_data_no_outliers %>%
          pairwise_t_test(
            Mconf ~ condition, paired = TRUE,
            p.adjust.method = "bonferroni"
          ) # significant differences in Mconf when comparing the forward, backward and symmetrical condition to the unrelated condition

        # Visualization: box plots with p-values
        bxp <- ggboxplot(Mconf_data, x = "condition", y = "Mconf", add = "point")
        pwc_bxp <- pwc %>% add_xy_position(x = "condition")
        bxp <- bxp +
          stat_pvalue_manual(pwc_bxp) +
          stat_pvalue_manual(pwc_bxp) +
          labs(caption = get_pwc_label(pwc_bxp)) +
          ggtitle(expression(M[conf]))
        
        # Return results without outliers
        results_no_outliers <- list(summary_stats = summary_stats,
                                    outliers_removed = TRUE,
                                    normality = normality_no_outliers,
                                    anova_table = anova_table_no_outliers,
                                    pairwise_tests = pwc_no_outliers,
                                    res_aov_no_outliers = res_aov_no_outliers,
                                    test_used = "ANOVA",
                                    plot = bxp)
        
      } else {
        ## Friedman Test
        
        res.fried_no_outliers <- Mconf_data_no_outliers %>% friedman_test(Mconf ~ condition |ID)
        
        # effect size
        
        effect_size_no_outliers <- Mconf_data_no_outliers %>% friedman_effsize(Mconf ~ condition |ID)
        
        ## posthoc tests
        
        pwc_no_outliers <- Mconf_data_no_outliers %>%
          pairwise_t_test(
            Mconf ~ condition, paired = TRUE,
            p.adjust.method = "bonferroni"
          ) # significant differences between every condition, except for symmetrical and forward
        
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
