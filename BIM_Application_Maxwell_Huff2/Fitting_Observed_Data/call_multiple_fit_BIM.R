# Fit BIM to data from recall tasks with continuous confidence ratings on a continuous scale.
# This function requires BIM_error, BIM_error_padding and multiple_fit_BIM to be loaded into the global environment.
# Padding is applied automatically if the value of rho is bigger than 0.98 or smaller than -0.98.

## INPUTS
# * nPt
# number of participants per condition, set to 32
#
# * observed_data
#
# Observed data for the forward, backward, symmetrical and unrelated condition for each participant. One file per condition.
# The data can be found in the Observed_Data folder.


## OUTPUTS
#
# * four result files
#
# four csv files containing a matrix with 5 columns for the results of the four parameters and the logL
# (Pexp, Mconf, mu_m, rho, logL) for each Pt for each condition


# install.packages("readxl")
library(readxl)

nPt <- 32

setwd('../BIM_Application_Maxwell_Huff/')

# Read data from Excel files
data_forward <- read_excel('../BIM_Application_Maxwell_Huff/Observed_Data/exp3_forward.xlsx', col_names = TRUE)
data_backward <- read_excel('../BIM_Application_Maxwell_Huff/Observed_Data/exp3_backward.xlsx', col_names = TRUE)
data_symmetrical <- read_excel('../BIM_Application_Maxwell_Huff/Observed_Data/exp3_symmetrical.xlsx', col_names = TRUE)
data_unrelated <- read_excel('../BIM_Application_Maxwell_Huff/Observed_Data/exp3_unrelated.xlsx', col_names = TRUE)

# Create a list containing all the data frames
data_list <- list(
  data_forward = data_forward,
  data_backward = data_backward,
  data_symmetrical = data_symmetrical,
  data_unrelated = data_unrelated
  )

# Iterate over each data frame in the list
for (df_name in names(data_list)) {
  observed_data <- data_list[[df_name]][, c('Confidence', 'Recall')]

  # modify the values in column "Recall"
  observed_data$Recall[observed_data$Recall == 100] <- 1

  # initialize matrix to store results
  result_matrix <- matrix(NA, nrow = nPt, ncol = 5,
                          dimnames = list(NULL, c("Pexp", "Mconf", "mu_m", "rho", "logL")))

  for (i in 1:nPt) {
    # extract data for current person
    start_row <- (i - 1) * nrow(observed_data) / nPt + 1
    end_row <- i * nrow(observed_data) / nPt
    Pt_data <- observed_data[start_row:end_row, , drop = FALSE]
    Pt_data <- as.data.frame(Pt_data)

    # call fit_bim function
    fit_result <- fit_bim(Pt_data, padding = 0, df_name, i)

    # store results in result_matrix
    result_matrix[i, 1:4] <- fit_result$params
    result_matrix[i, 5] <- fit_result$logL
  }

  # write the result matrix to a CSV file
  write.csv(result_matrix, paste0(df_name, "_result.csv"), row.names = FALSE)
}
