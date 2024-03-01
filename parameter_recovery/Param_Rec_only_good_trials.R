# Load required libraries
# install.packages("reshape2")
library(MASS)  # For the mvrnorm function
library(base)
library(ggplot2)
library(gridExtra)
library(reshape2)

# Set seed
set.seed(as.numeric(format(Sys.time(), "%H%M%S")))

# Number of trials for each simulation
## if ntrial = 10 following error message is likely: "Performance for all trials is the same. Estimation of parameters mu_m and rho is inaccurate."
ntrial <- 50

# Number of simulations
nsimulation <- 1000

# Range for each parameter
range_Pexp <- c(0.1, 0.9)
range_Mconf <- c(0.1, 0.9)
range_mu_m <- c(-2, 2)
range_rho <- c(-0.9, 0.9)

# Initialize matrices for true and fitted parameter, as well as the simulated data matrix
true_params <- matrix(0, nrow = nsimulation, ncol = 4)  # True value of parameters
fit_params <- matrix(0, nrow = nsimulation, ncol = 4)  # Fitted parameters
data <- array(0, dim = c(ntrial, 2, nsimulation))  # Simulation dataset

# parameter recovery loop
for (i in 1:nsimulation) {
  
  # Set parameter value
  Pexp <- runif(1, range_Pexp[1], range_Pexp[2])
  Mconf <- runif(1, range_Mconf[1], range_Mconf[2])
  mu_m <- runif(1, range_mu_m[1], range_mu_m[2])
  rho <- runif(1, range_rho[1], range_rho[2])
  
  true_params[i,] <- c(Pexp, Mconf, mu_m, rho)
  
  # Simulate data using BIM_simulation function
  observed_data <- BIM_simulation(Pexp, Mconf, mu_m, rho, ntrial)
  
  data[,,i] <- observed_data
  
  # Fit BIM model using fit_bim function
  fitting_result <- fit_bim(observed_data, 0)
  
  # extract the parameter 
  temp1 <- fitting_result$params
  
  # Check and apply padding correction for rho if temp1[4] is not NA
  if (!is.na(temp1[4]) && abs(temp1[4]) > 0.98) {
    fitting_result_padding <- fit_bim(observed_data, 1)
    temp1 <- fitting_result_padding$params
  }
  
  fit_params[i,] <- temp1
}

## create 4 matrices for each parameter with the true and fitted params
corr.matrix.mu_m <- data.frame(true = true_params[,3], fitted = fit_params[,3])
corr.matrix.rho <- data.frame(true = true_params[,4], fitted = fit_params[,4])

# Finde die Zeilen mit NA-Werten im ersten Dataframe (corr.matrix.Pexp)
na_rows <- which(is.na(corr.matrix.mu_m$fitted))

# Entferne diese Zeilen aus allen Dataframes
corr.matrix.mu_m <- corr.matrix.mu_m[-na_rows, ]
corr.matrix.rho <- corr.matrix.rho[-na_rows, ]

# asses parameter recovery by calculating the correlation
param_recovery <- function(true_params, fitted_params) {
  
  fitted_params <- na.omit(fitted_params)
  
  correlation <- cor(true_params, fitted_params, use = "pairwise.complete.obs")
  
  if (correlation > 0.9) {
    quality <- "Excellent"
  } else if (correlation > 0.75) {
    quality <- "Good"
  } else {
    quality <- "Poor"
  }
  
  return(list(correlation = correlation, quality = quality))
}

# Entfernen Sie NAs aus den fitted-Parametern für alle Parameter gleichzeitig

# extract results
result_mu_m <- param_recovery(corr.matrix.mu_m$true, corr.matrix.mu_m$fitted)
result_rho <- param_recovery(corr.matrix.rho$true, corr.matrix.rho$fitted)

# save current working directory
## Ensure this becomes the designated target directory!
wd <- ("C:/Users/Valentina/OneDrive - Universitaet Bern/Bachelor/ParamRec")
setwd(wd)
# generate target directory for the plots and the Results
folder_name <- sprintf("Results_Parameter_Recovery_only_good_trials_%.d_%.d__%s", ntrial, nsimulation, format(Sys.time(), "%Y.%m.%d_%H.%M.%S"))
dir.create(folder_name, showWarnings = FALSE)
setwd(folder_name)

## Correlation Results

# Save a text file with the results in the target directory
sink(paste0("Results_Parameter_Recovery_", ntrial, "_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".txt", sep = ""))

cat(
  "Results of Parameter Recovery:\n",
  paste("mu_m - Correlation:", result_mu_m$correlation, "Quality:", result_mu_m$quality, "\n"),
  paste("rho - Correlation:", result_rho$correlation, "Quality:", result_rho$quality, "\n")
)

sink()

## correlation plots

# Generate a list for the 25 plots
plots <- list()

# Function to plot correlation between true and fitted values

plot_correlation <- function(corr.matrix, title, xlim, ylim, x_ticks, y_ticks) {
  
  # Plot
  plot <- ggplot(data = corr.matrix, mapping = aes(x = true, y = fitted)) +
    geom_point(colour = "orange", alpha = 0.4, size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(title = title, x = "true value", y = "fitted value") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_cartesian(
      xlim = xlim,
      ylim = ylim) +
    scale_x_continuous(
      breaks = x_ticks, 
      limits = xlim) +
    scale_y_continuous(
      breaks = y_ticks, 
      limits = ylim)
  
  return(plot)
}

# Plot correlation for each parameter
plots[[3]] <- plot_correlation(corr.matrix.mu_m, (bquote(paste(mu[m], " corrected"))), c(-2, 2), c(-5, 5), c(seq(-2, 2, by = 0.5)), c(seq(-5, 5, by = 2.5)))
plots[[4]] <- plot_correlation(corr.matrix.rho, (bquote(paste(rho, " corrected"))), c(-1, 1), c(-1, 1), c(seq(-1, 1, by = 0.5)), c(seq(-1, 1, by = 0.5)))

#generate gtable(all 4 plots)
gtable <- grid.arrange(grobs = plots, ncol = 2, nrow = 2)

#save gtable(all 4 plots) in the target directory
ggsave(
  paste0("Correlation Plots_corrected_", ntrial, ".png", sep = ""),
  gtable,
  width = 13,
  height = 7,
  units = "in",
  dpi = 300
)

setwd(wd)