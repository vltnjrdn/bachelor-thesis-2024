# ANOVAs (or Friedman tests if assumption of normality isn't met) to compare mean recall, confidence and AUROC
# between conditions for the observed data from Maxwell & Huff.
# If the data contains outliers, the analysis will be done with and without them.
# This function requires Analysis_Recall, Analysis_Confidence and Analysis_AUROC to be loaded into the global environment.

## INPUTS
# * observed_data
#
# Observed data for the forward, backward, symmetrical and unrelated condition for each participant. One file per condition.
# The data can be found in the Observed_Data folder.


## OUTPUTS
# * Result file
#
# A text file called "Analysis_Observed_Data_AD.txt" containing all the results for all 3 Analysis.

# install.packages(readxl)
# install.packages(pROC)
library(readxl)
library(pROC)


setwd('./BIM_application_real_data/preliminary_analysis/')

# read data from Excel files
data_forward <- read_excel('../BIM_Application_Maxwell_Huff/Observed_data/exp3_forward.xlsx', col_names = TRUE)
data_backward <- read_excel('../BIM_Application_Maxwell_Huff/Observed_data/exp3_backward.xlsx', col_names = TRUE)
data_symmetrical <- read_excel('../BIM_Application_Maxwell_Huff/Observed_data/exp3_symmetrical.xlsx', col_names = TRUE)
data_unrelated <- read_excel('../BIM_Application_Maxwell_Huff/Observed_data/exp3_unrelated.xlsx', col_names = TRUE)

# Create a list containing all the modified data frames
data_list <- list(
  forward = data_forward,
  backward = data_backward,
  symmetrical = data_symmetrical,
  unrelated = data_unrelated
)

# create an empty data frame to store the results
result_df <- data.frame(ID = numeric(),
                        Condition = character(),
                        mean_Recall = numeric(),
                        mean_Confidence = numeric(),
                        AUROC = numeric(),
                        stringsAsFactors = FALSE)

# iterate over each data frame in the list
for (i in seq_along(data_list)) {
  df_name <- names(data_list)[i]
  observed_data <- data_list[[df_name]][, c("Confidence", "Recall")]

  # modify the values in column "Recall"
  observed_data$Recall[observed_data$Recall == 100] <- 1

  nVpn <- (nrow(observed_data) / 40)

  for (j in 1:nVpn) {
    # extract data for current person
    start_row <- (j - 1) * nrow(observed_data) / nVpn + 1
    end_row <- j * nrow(observed_data) / nVpn
    person_data <- observed_data[start_row:end_row, , drop = FALSE]

    # compute mean recall and mean confidence
    mean_Recall <- mean(person_data$Recall)
    mean_Confidence <- mean(person_data$Confidence)

    # compute AUROC if possible
    if (sum(person_data$Recall == 0) == 0 || sum(person_data$Recall == 1) == 0) {
      AUROC <- NA
    } else {
      roc_obj <- roc(response = person_data$Recall,
                     predictor = person_data$Confidence,
                     levels = c(0, 1),
                     quiet = TRUE)
      AUROC <- auc(roc_obj)
    }

    # append results to the result data frame
    result_df <- rbind(result_df, data.frame(ID = j,
                                             Condition = df_name,
                                             mean_Recall = mean_Recall,
                                             mean_Confidence = mean_Confidence,
                                             AUROC = AUROC))
  }
}

# delete NA for the AUROC dataset
na_auroc_rows <- subset(result_df, is.na(AUROC))
ID_NA <-  na_auroc_rows$ID
cleaned_result_df <- result_df[result_df$ID != ID_NA, ]

# save results in txt
sink(file = "Analysis_Observed_Data_AD.txt")
results_Recall <- Analysis_Recall(result_df[, c("ID", "Condition", "mean_Recall")])
print("Results Recall")
print(results_Recall)
results_Confidence <- Analysis_Confidence(result_df[, c("ID", "Condition", "mean_Confidence")])
print("Results Confidence")
print(results_Confidence)
results_AUROC <- Analysis_AUROC(result_df[, c("ID", "Condition", "AUROC")])
print("Results AUROC")
print(results_AUROC)
sink(file = NULL)

print("Analysis results have been saved to 'Analysis_Observed_Data_AD.txt'")
