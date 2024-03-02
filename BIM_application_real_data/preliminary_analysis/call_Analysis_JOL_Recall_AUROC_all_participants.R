library(readxl)
library(pROC)


setwd('./BIM_application_real_data/preliminary_analysis/')

# Read data from Excel files
data_forward <- read_excel('./BIM_application_real_data/observed_data/exp3_forward.xlsx', col_names = TRUE)
data_backward <- read_excel('./BIM_application_real_data/observed_data/exp3_backward.xlsx', col_names = TRUE)
data_symmetrical <- read_excel('./BIM_application_real_data/observed_data/exp3_symmetrical.xlsx', col_names = TRUE)
data_unrelated <- read_excel('./BIM_application_real_data/observed_data/exp3_unrelated.xlsx', col_names = TRUE)

# Create a list containing all the modified data frames
data_list <- list(
  forward = data_forward,
  backward = data_backward,
  symmetrical = data_symmetrical,
  unrelated = data_unrelated
)

# Create an empty data frame to store the results
result_df <- data.frame(ID = numeric(),
                        Condition = character(),
                        mean_Recall = numeric(),
                        mean_Confidence = numeric(),
                        AUROC = numeric(),
                        stringsAsFactors = FALSE)

# Iterate over each data frame in the list
for (i in seq_along(data_list)) {
  df_name <- names(data_list)[i]
  observed_data <- data_list[[df_name]][, c("Confidence", "Recall")]

  # Modify the values in column "Recall"
  observed_data$Recall[observed_data$Recall == 100] <- 1

  nVpn <- (nrow(observed_data) / 40)

  for (j in 1:nVpn) {
    # Extract data for current person
    start_row <- (j - 1) * nrow(observed_data) / nVpn + 1
    end_row <- j * nrow(observed_data) / nVpn
    person_data <- observed_data[start_row:end_row, , drop = FALSE]

    # Compute mean recall and mean confidence
    mean_Recall <- mean(person_data$Recall)
    mean_Confidence <- mean(person_data$Confidence)

    # Compute AUROC if possible
    if (sum(person_data$Recall == 0) == 0 || sum(person_data$Recall == 1) == 0) {
      AUROC <- NA
    } else {
      roc_obj <- roc(response = person_data$Recall,
                     predictor = person_data$Confidence,
                     levels = c(0, 1),
                     quiet = TRUE)
      AUROC <- auc(roc_obj)
    }

    # Append results to the result data frame
    result_df <- rbind(result_df, data.frame(ID = j,
                                             Condition = df_name,
                                             mean_Recall = mean_Recall,
                                             mean_Confidence = mean_Confidence,
                                             AUROC = AUROC))
  }
}

# Print the updated matrix
print(result_df)

na_auroc_rows <- subset(result_df, is.na(AUROC))
ID_NA <-  na_auroc_rows$ID
cleaned_result_df <- result_df[result_df$ID != ID_NA, ]

sink(file = "PreResults_Output.txt")
results_Recall <- Analysis_Recall(result_df[, c("ID", "Condition", "mean_Recall")])
print("Results Recall")
print(results_Recall)
results_Confidence <- Analysis_Confidence(result_df[, c("ID", "Condition", "mean_Confidence")])
print("Results Confidence")
print(results_Confidence)
sink(file = NULL)

print("Analysis results have been saved to PreResults_Output.txt")
