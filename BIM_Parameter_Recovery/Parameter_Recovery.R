# Parameter Recovery for the BIM model
# Simulates random data from true parameters, fits the model to the data and calculates the correlation between fitted with and true parameters.
# This function requires BIM_error, BIM_error_padding, fit_BIM_hookejeeves and BIM_simulation to be loaded into the global environment.

## INPUTS
# * ntrial
#
# Number of trial per simulated data set (recommend to set it from 10-100 with increments of 10), default is 10.
#
# * nsimulations
#
# Number of simulated datasets per parameter recovery, default is set to 1000.

## OUTPUTS
# * parameter recovery results
#
# Folder containing a .txt with the correlation between true and fitted values for each parameter and the quality of the parameter recovery
# as well as a png with a gtable containing four plots with the correlation for each parameter for each simulated data set.

# install.packages("reshape2")
# install.packages("MASS")
# install.packages("ggplot")
# install.packages("gridExtra")
# install.packages("reshape2")
library(MASS)
library(base)
library(ggplot2)
library(gridExtra)
library(reshape2)

# Set seed
set.seed(as.numeric(format(Sys.time(), '%H%M%S')))

# Number of trials for each simulation
## if ntrial = 10 following error message is likely: "Performance for all trials is the same. Estimation of parameters mu_m and rho is inaccurate."
ntrial <- 10

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

  true_params[i, ] <- c(Pexp, Mconf, mu_m, rho)

  # simulate data using BIM_simulation function
  observed_data <- BIM_simulation(Pexp, Mconf, mu_m, rho, ntrial)

  data[, , i] <- observed_data

  # fit BIM model using fit_bim function
  fitting_result <- fit_bim(observed_data, 0)

  # extract the parameters
  temp1 <- fitting_result$params

  # check rho and apply padding correction if needed
  if (abs(temp1[4]) > 0.98) {
    fitting_result_padding <- fit_bim(observed_data, 1)
    temp1 <- fitting_result_padding$params
  }

  fit_params[i, ] <- temp1
}

# create 4 matrices for each parameter with the true and fitted params
corr.matrix.Pexp <- data.frame(true = true_params[, 1], fitted = fit_params[, 1])
corr.matrix.Mconf <- data.frame(true = true_params[, 2], fitted = fit_params[, 2])
corr.matrix.mu_m <- data.frame(true = true_params[, 3], fitted = fit_params[, 3])
corr.matrix.rho <- data.frame(true = true_params[, 4], fitted = fit_params[, 4])

# asses parameter recovery by calculating the correlation
param_recovery <- function(true_params, fitted_params) {
  correlation <- cor(true_params, fitted_params)

  if (correlation > 0.9) {
    quality <- 'Excellent'
  } else if (correlation > 0.75) {
    quality <- 'Good'
  } else {
    quality <- 'Poor'
  }

  return(list(correlation = correlation, quality = quality))
}

# extract results
result_Pexp <- param_recovery(corr.matrix.Pexp$true, corr.matrix.Pexp$fitted)
result_Mconf <- param_recovery(corr.matrix.Mconf$true, corr.matrix.Mconf$fitted)
result_mu_m <- param_recovery(corr.matrix.mu_m$true, corr.matrix.mu_m$fitted)
result_rho <- param_recovery(corr.matrix.rho$true, corr.matrix.rho$fitted)

# save current working directory
wd <- '../BIM_Parameter_Recovery/'
setwd(wd)

# generate folder for the plots and the Results
folder_name <- sprintf(
  'Results_Parameter_Recovery_%.d_%.d__%s',
  ntrial,
  nsimulation,
  format(Sys.time(), '%Y.%m.%d_%H.%M.%S')
)

dir.create(folder_name, showWarnings = FALSE)
setwd(folder_name)

## correlation Results

# save a text file with the results in the target directory
sink(paste0(
  'Results_Parameter_Recovery',
  ntrial,
  '_',
  format(Sys.time(), '%Y.%m.%d_%H.%M.%S'),
  '.txt',
  sep = ''
))

cat(
  'Results of Parameter Recovery:\n',
  paste(
    'Pexp - Correlation:',
    result_Pexp$correlation,
    'Quality:',
    result_Pexp$quality,
    '\n'
  ),
  paste(
    'Mconf - Correlation:',
    result_Mconf$correlation,
    'Quality:',
    result_Mconf$quality,
    '\n'
  ),
  paste(
    'mu_m - Correlation:',
    result_mu_m$correlation,
    'Quality:',
    result_mu_m$quality,
    '\n'
  ),
  paste(
    'rho - Correlation:',
    result_rho$correlation,
    'Quality:',
    result_rho$quality,
    '\n'
  )
)

sink()

## correlation plots

# generate a list for the 4 plots
plots <- list()

# function for plotting the correlation between true and fitted values
plot_correlation <-
  function(corr.matrix,
           title,
           xlim,
           ylim,
           x_ticks,
           y_ticks) {

    # plotting
    plot <- ggplot(data = corr.matrix, mapping = aes(x = true, y = fitted)) +
            geom_point(colour = 'orange', alpha = 0.4, size = 2) +
            geom_smooth(method = 'lm', se = FALSE, color = 'black') +
            labs(title = title, x = 'true value', y = 'fitted value') +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5)) +
            coord_cartesian(xlim = xlim, ylim = ylim) +
            scale_x_continuous(breaks = x_ticks, limits = xlim) +
            scale_y_continuous(breaks = y_ticks, limits = ylim)

    return(plot)
  }

# correlation plot for each parameter
plots[[1]] <- plot_correlation(corr.matrix.Pexp,
                               (expression(P[exp])),
                               c(0.1, 0.9),
                               c(0, 1),
                               c(seq(0.1, 0.9, by = 0.1)),
                               c(seq(0, 1, by = 0.2)))

plots[[2]] <- plot_correlation(corr.matrix.Mconf,
                               (expression(M[conf])),
                               c(0.1, 0.9),
                               c(0, 1),
                               c(seq(0.1, 0.9, by = 0.1)),
                               c(seq(0, 1, by = 0.2)))

plots[[3]] <- plot_correlation(corr.matrix.mu_m,
                               (bquote(mu[m])),
                               c(-2, 2),
                               c(-5, 5),
                               c(seq(-2, 2, by = 0.5)),
                               c(seq(-5, 5, by = 2.5)))

plots[[4]] <- plot_correlation(corr.matrix.rho,
                               (bquote(rho)),
                               c(-1, 1),
                               c(-1, 1),
                               c(seq(-1, 1, by = 0.5)),
                               c(seq(-1, 1, by = 0.5)))

# generate gtable
gtable <- grid.arrange(grobs = plots, ncol = 2, nrow = 2)

# save gtable in the target directory
ggsave(
  paste0('Correlation Plots', ntrial, '.png', sep = ''),
  gtable,
  width = 13,
  height = 7,
  units = 'in',
  dpi = 300
)
