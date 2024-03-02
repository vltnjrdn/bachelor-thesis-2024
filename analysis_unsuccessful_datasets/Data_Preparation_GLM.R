library(readxl)

observed_data <- read_excel('./BIM_application_real_data/observed_data/exp3_unrelated.xlsx', col_names = TRUE)
print(observed_data)

# Modify the values in column 2
observed_data <- observed_data[, c("Confidence", "Recall")]
observed_data$Recall[observed_data$Recall == 100] <- 1
observed_data <- as.data.frame(observed_data)
num_persons <- 32

# Schleife durchführen, um Daten für jede Person zu speichern
for (i in 1:num_persons) {
  # Daten für jede Person auswählen
  person_data <- observed_data[((i - 1) * 40 + 1):(i * 40),]
  print(person_data)

  fit_result <- fit_bim(person_data, 0)

  print(fit_result$logL)

  logL <- ifelse((fit_result$logL > 0), "pos", "neg")

  # Dateiname zusammenstellen
  file_name <- paste0("Vpn", i, "_unrelated_", logL, ".txt")

  file_path <- file.path('./analysis_unsuccessful_datasets/tagged_data_exp3', file_name)

  # Daten als Datei speichern
  write.table(
    person_data,
    file = file_path,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE
  )
}
