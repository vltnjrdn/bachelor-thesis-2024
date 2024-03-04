# ANOVAs (or Friedman tests if assumption of normality isn't met) to compare mean Parameters
# between conditions for the observed data from Maxwell & Huff.
# If the data contains outliers, the analysis will be done with and without them.
# This function requires Analysis_Pexp, Analysis_Mconf, Analysis_mu_m and Analysis_rho to be loaded into the global environment.

## INPUTS
# * fitting results
#
# Saved results of the multi_fit_BIM function. Our results can be found in the Fitting_Results_Data folder.


## OUTPUTS
# * Result file
#
# A text file called "Parameter_Analysis.txt" containing all the results for all 4 Analysis.
#
# * Plots
#
# A png containing a gtable with the Analysis of rho, mu_m, Mconf and Pexp.

# install.packages("readr")
# install.packages("gridExtra")
library(gridExtra)
library(readr)

setwd('./BIM_Application_Maxwell_Huff/Analysis_Fitting_Results/')

# read data from Excel files
data_forward_result <- read_csv("./BIM_Application_Maxwell_Huff/Fitting_Results_Data/exp3_forward_result.csv")
data_backward_result <- read_csv("./BIM_Application_Maxwell_Huff/Fitting_Results_Data/exp3_backward_result.csv")
data_symmetrical_result <- read_csv("./BIM_Application_Maxwell_Huff/Fitting_Results_Data/exp3_symmetrical_result.csv")
data_unrelated_result <- read_csv("./BIM_Application_Maxwell_Huff/Fitting_Results_Data/exp3_unrelated_result.csv")

# create a list containing all the data frames
all_conditions_list <- list(forward = data_forward_result,
                            backward = data_backward_result,
                            symmetrical = data_symmetrical_result,
                            unrelated = data_unrelated_result)

# see how may logL turned out to be positive
positive_row_numbers <- lapply(all_conditions_list, function(df) {
  positive_rows <- which(df[, 5] > 0)
  return(positive_rows)
})

# print the positive row numbers for each condition
print(positive_row_numbers)

# we decided to work without the unrelated condition and only use those
# participants that have a negative value in all three remaining conditions

# remove the unrelated condition
all_conditions_list <- all_conditions_list[-4]

# recalculate positive row numbers without the deleted element
positive_row_numbers <- lapply(all_conditions_list, function(df) {
  positive_rows <- which(df[, 5] > 0)
  return(positive_rows)
})

# combine all positive row numbers into a single vector
all_positive_row_numbers <- unlist(positive_row_numbers)
positive_row_numbers <- unique(all_positive_row_numbers)
print(positive_row_numbers)

all_conditions_list <- lapply(all_conditions_list, function(df) {
  df <- df[-positive_row_numbers, ]
  return(df)
})

sink(file = "Parameter_Analysis.txt")
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
sink(file = NULL)

print("Analysis results have been saved to 'Parameter_Analysis.txt'")

plots <- list(results_Pexp$plot, results_Mconf$results_no_outliers$plot, results_mu_m$plot, results_rho$plot)

# arrange plots
gtable <- grid.arrange(grobs = plots, ncol = 2, nrow = 2, padding = unit(5, "in"))

# save png
ggsave(
  file = "Analysis_Results.png",
  plot = gtable,
  width = 12,
  height = 12,
  units = "in",
  dpi = 300
)
