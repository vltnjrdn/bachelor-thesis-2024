library(readxl)

setwd('./BIM_application_real_data/fitting_observed_data/')

# Read data from Excel files
data_forward <- read_excel('./BIM_application_real_data/observed_data/exp3_forward.xlsx', col_names = TRUE)
data_backward <- read_excel('./BIM_application_real_data/observed_data/exp3_backward.xlsx', col_names = TRUE)
data_symmetrical <- read_excel('./BIM_application_real_data/observed_data/exp3_symmetrical.xlsx', col_names = TRUE)
data_unrelated <- read_excel('./BIM_application_real_data/observed_data/exp3_unrelated.xlsx', col_names = TRUE)

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

  # Modify the values in column "Recall"
  observed_data$Recall[observed_data$Recall == 100] <- 1

  # Perform analysis
  result_matrix <- multi_fitBIM(observed_data, nVpn = 32, df_name)

  # # Write the result matrix to a CSV file
  # write.csv(result_matrix, paste0(df_name, "_better_result.csv"), row.names = FALSE)
}