# install.packages("gridExtra")
library(gridExtra)
library(readr)

setwd('./BIM_application_real_data/main_analysis/')

# Read data from Excel files
data_forward_result <- read_csv("exp3_forward_result.csv")
data_backward_result <- read_csv("exp3_backward_result.csv")
data_symmetrical_result <- read_csv("exp3_symmetrical_result.csv")
data_unrelated_result <- read_csv("exp3_unrelated_result.csv")

# Create a list containing all the data frames
all_conditions_list <- list(forward = data_forward_result,
                            backward = data_backward_result,
                            symmetrical = data_symmetrical_result,
                            unrelated = data_unrelated_result)

# see how may logL turned out to be positive
positive_row_numbers <- lapply(all_conditions_list, function(df) {
  positive_rows <- which(df[, 5] > 0)
  return(positive_rows)
})

# Print the positive row numbers for each condition
print(positive_row_numbers)

# we decided to work without the unrelated condition and only use those
# participants that have a negative value in all three remaining conditions

# Remove the unrelated condition
all_conditions_list <- all_conditions_list[-4]

# Recalculate positive row numbers without the deleted element
positive_row_numbers <- lapply(all_conditions_list, function(df) {
  positive_rows <- which(df[, 5] > 0)
  return(positive_rows)
})

# Combine all positive row numbers into a single vector
all_positive_row_numbers <- unlist(positive_row_numbers)
positive_row_numbers <- unique(all_positive_row_numbers)
print(positive_row_numbers)

all_conditions_list <- lapply(all_conditions_list, function(df) {
  df <- df[-positive_row_numbers, ]
  return(df)
})

sink(file = "Parameter_Analysis_Output.txt")
results_Pexp <- Analysis_Pexp(all_conditions_list)
print("Results_Pexp")
print(results_Pexp)
results_Mconf <- Analysis_Mconf(all_conditions_list)
print("Results_Mconf")
print(results_Mconf)
results_mu_m <- Analysis_mu_m(all_conditions_list)
print("Results_mu_m")
print(results_mu_m)
results_rho <- Analysis_rho(all_conditions_list)
print("Results_rho")
print(results_rho)
# results_logL <- Analysis_logL(all_conditions_list)
# print("Results_logL")
# print(results_logL)
sink(file = NULL)

print("Analysis results have been saved to Parameter_Analysis_Output.txt")

plots <- list(results_Pexp$plot, results_Mconf$results_no_outliers$plot, results_mu_m$plot, results_rho$plot)

# Anordnen der Plots
gtable <- grid.arrange(grobs = plots, ncol = 2, nrow = 2, padding = unit(5, "in"))

# Speichern der Plots
ggsave(
  file = "ANOVA_plots.png",
  plot = gtable,
  width = 12,
  height = 12,
  units = "in",
  dpi = 300
)