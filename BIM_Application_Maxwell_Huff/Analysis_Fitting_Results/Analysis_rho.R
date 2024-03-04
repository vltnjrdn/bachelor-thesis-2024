library(car)
library(tibble)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(rstatix)

Analysis_rho <- function(all_conditions_list) {

  ## repeated measures ANOVA for rho

  # initialize an empty list to store the data
  rho_list <- list()

  # extract the first column from each data frame in the list
  for (df_name in names(all_conditions_list)) {
    rho_list[[df_name]] <- all_conditions_list[[df_name]][[4]]
  }

  # data preparation
  rho_df <- as.data.frame(rho_list)

  rho_tibble <- rho_df %>%
    mutate(ID = row_number()) %>%
    select(ID, everything()) %>%
    as_tibble()

  rho_data <- rho_tibble %>%
    gather(key = "condition", value = "rho", forward, backward, symmetrical) %>%
    convert_as_factor(ID, condition)

  # delete ID 23 (participant 29), for this participant all recall trials are the same
  rho_data <- rho_data %>%
    filter(ID != 23 ) %>%
    mutate(ID = rep(1:(length(ID)/3), length.out = n()))

  # compute summary statistics
  summary_stats <- rho_data %>%
    group_by(condition) %>%
    get_summary_stats(rho, type = "mean_sd")

  ## check assumptions

  # check for outliers
  outliers <- rho_data %>%
    group_by(condition) %>%
    identify_outliers(rho) # no extreme outliers found

  # assumption of normality
  normality <- rho_data %>%
    group_by(condition) %>%
    shapiro_test(rho)

  # perform ANOVA if normality assumptions hold, otherwise perform Friedman test
  if(all(normality$p > 0.05)) {

    # repeated measures ANOVA
    res_aov <- anova_test(data = rho_data, dv = rho, wid = ID, within = condition)
    anova_table <- get_anova_table(res_aov)

    # posthoc tests
    pwc <- rho_data %>%
      pairwise_t_test(
        rho ~ condition, paired = TRUE,
        p.adjust.method = "bonferroni"
      )

    # Visualization: box plots with p-values
    bxp <- ggboxplot(rho_data, x = "condition", y = "rho", add = "point")
    pwc_bxp <- pwc %>% add_xy_position(x = "condition")
    bxp <- bxp +
      stat_pvalue_manual(pwc_bxp) +
      labs(caption = get_pwc_label(pwc_bxp)) +
      ggtitle(bquote(rho))

    # Return results
    results <- list(summary_stats = summary_stats,
                    outliers = outliers,
                    normality = normality,
                    anova_table = anova_table,
                    pairwise_tests = pwc,
                    test_used = "ANOVA",
                    plot = bxp)

  } else {
    # Friedman Test
    res.fried <- rho_data %>% friedman_test(rho ~ condition |ID)

    # effect size
    effect_size <- rho_data %>% friedman_effsize(rho ~ condition |ID)

    # posthoc tests
    pwc <- rho_data %>%
      pairwise_t_test(
        rho ~ condition, paired = TRUE,
        p.adjust.method = "bonferroni"
      ) #

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
