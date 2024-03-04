# Jeder einzelne Datensatz wird eingelesen.
# Aus dem Dateinamen werden ID, Bedingung und ob der Datensatz ein "failure" war oder nicht extrahiert
# Für jeden Datensatz werden mean_confidence/recall, variance_confidence/recall und cor zwischen confidence und recall berechnet 
# Für jeden Datensatz wird eine AUROC durchgeführt
# Die Ergebnisse werden in einer Matrix mit 8 Spalten brechnet: "ID", "failure", "Condition", "mean_recall", "mean_confidence", "variance_recall", "variance_confidence", "cor", "AUROC"
# Die Die Matrix wird in einmal in einen Dataframe mit NA values in cor und AUROC umgewandelt und einmal in einen dataframe ohne NAs
# Modelle werden spezifiziert. Dabei waren nur model_confidence und model_recall erfolgreich.

# install.packages("pROC")
# install.packages("Matrix")
library(pROC)
library(lme4)
library(Matrix)
library(tidyverse)
library(performance)

# Setze den Pfad zum Ordner, in dem die Datensätze gespeichert sind
daten_ordner <- ("C:/Users/Valentina/OneDrive - Universitaet Bern/Bachelor/multi_fitBIM/tagged_data_exp3")

# Liste alle Dateien im Ordner auf
dateien <- list.files(daten_ordner, full.names = TRUE)

# Initialisiere leere Matrix für die Ergebnisse
matrix_results <- matrix(NA, nrow = length(dateien), ncol = 9,
                         dimnames = list(NULL, c("ID", "failure", "Condition", "mean_recall", "mean_confidence", "variance_recall", "variance_confidence", "cor", "AUROC")))

# Funktion zur Extraktion von ID, failure und Condition aus dem Dateinamen definieren
extract_info <- function(filename) {
  filename <- gsub(".txt", "", filename)  # Entferne die Dateiendung
  parts <- unlist(strsplit(filename, "_"))  # Teile den Dateinamen an "_"
  ID <- as.numeric(gsub("[^0-9]", "", parts[1]))  # Extrahiere die ID und konvertiere sie in numerischen Wert
  failure <- ifelse(grepl("pos", filename), 1, 0)  # Überprüfe, ob "pos" im Dateinamen enthalten ist
  Condition <- parts[2]  # Der zweite Teil des Dateinamens ist die Bedingung
  return(c(ID, failure, Condition))
}

# Funktion zur Berechnung von Statistiken definieren
compute_stats <- function(data) {
  mean_recall <- mean(data$Recall)
  mean_confidence <- mean(data$Confidence)
  variance_recall <- var(data$Recall)
  variance_confidence <- var(data$Confidence)
  cor_recall_confidence <- cor(data$Recall, data$Confidence)
  auroc <- compute_auroc(data)
  return(c(mean_recall, mean_confidence, variance_recall, variance_confidence, cor_recall_confidence, auroc))
}

# Funktion zur Berechnung von AUROC definieren
compute_auroc <- function(data) {
  # Überprüfe, ob mindestens ein Fall für jede Klasse vorhanden ist
  if (sum(data$Recall == 0) == 0 | sum(data$Recall == 1) == 0) {
    return(NA)  # Wenn nicht, gib NA zurück
  }
  # Berechne die AUROC nur, wenn ausreichende Daten vorhanden sind
  roc_obj <- roc(response = data$Recall, 
                 predictor = data$Confidence, 
                 levels = c(0, 1), 
                 qiet = TRUE)
  return(auc(roc_obj))
}

# Iteriere über alle Dateien und führe Berechnungen durch
for (i in 1:length(dateien)) {
  # Lese die Daten aus der Datei ein
  daten <- read.csv(dateien[i])
  # Extrahiere Informationen aus dem Dateinamen
  info <- extract_info(basename(dateien[i]))
  # Füge ID, failure und Condition zur Ergebnismatrix hinzu
  matrix_results[i, 1:3] <- info
  # Füge die berechneten Statistiken zur Ergebnismatrix hinzu
  matrix_results[i, 4:9] <- compute_stats(daten)
}

# Konvertiere Matrix in Datenrahmen
matrix_results <- as.data.frame(matrix_results) 
matrix_results$ID <- as.numeric(matrix_results$ID)
matrix_results$failure <- as.numeric(matrix_results$failure)
matrix_results$mean_recall <- as.numeric(matrix_results$mean_recall)
matrix_results$mean_confidence <- as.numeric(matrix_results$mean_confidence)
matrix_results$variance_recall <- as.numeric(matrix_results$variance_recall)
matrix_results$variance_confidence <- as.numeric(matrix_results$variance_confidence)

df <- matrix_results
df <- na.omit(df)
View(df)

# Spezifizieren der Modelle
library(tidyverse)
glimpse(df)

df$AUROC <- as.numeric(as.character(df$AUROC))
df

print(df$AUROC)

# Convert factors and scale predictors
df <- df |>
  mutate(ID = as.factor(ID),
         Condition = as.factor(Condition),
         mean_confidence = mean_confidence - mean(mean_confidence),
         variance_confidence = variance_confidence - mean(variance_confidence),
         mean_recall = mean_recall - mean(mean_recall),
         variance_recall = variance_recall - mean(variance_recall),
         AUROC = AUROC - mean(AUROC))

# modell_with_recall
model_with_recall <- glm(failure ~ mean_recall + variance_recall + mean_confidence + variance_confidence + AUROC + Condition, family = binomial, data = df)
car::vif(model_with_recall) 
summary(model_with_recall)

plot <- check_model(model_with_recall)
# Diagramm als PNG-Datei speichern
png("check_model_with_recall.png", width = 800, height = 600)
print(plot)
dev.off()
df

# final model without recall
mca <- glm(failure ~ mean_confidence + variance_confidence + Condition + AUROC, family = binomial, data = df)
summary(mca)
print(car::vif(mca))

plot <- check_model(model_with_recall)
# Diagramm als PNG-Datei speichern
png("check_final_model.png", width = 800, height = 600)
print(plot)
dev.off()
df

# Model improvement
null_model <- glm(failure ~ 1, family = binomial, data = df)
full_model <- glm(failure ~ mean_confidence + variance_confidence + Condition + AUROC, family = binomial, data = df)
lr_test <- anova(null_model, full_model, test = "Chisq")
print(lr_test)
