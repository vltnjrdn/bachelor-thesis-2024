# Function to fit the simulated data.
# Do not use this function directly. Instead use Parameter_Recover_filtered_data.

# install.packages("adagio")
library(adagio)

fit_bim <- function(observed_data, padding) {
  #define warnings
  w <- 0

  if (!exists('padding') || is.null(padding)) {
    padding <- 0
  }

  if (!padding %in% c(0, 1)) {
    stop('Padding must be set as 0 or 1.')
  }

  if (length(unique(observed_data[, 1])) == 1) {
    warning('Confidence ratings for all trials are the same. Estimation of parameters Pexp and rho is inaccurate.')
    w <- 1
  }

  unique_performance <- unique(observed_data[, 2])
  if (length(unique_performance) == 1) {
    warning('Performance for all trials is the same. Skipping fitting for this dataset.')
    return(list(
      params = NA,
      logL = NA,
      w = 1
    ))  # Return NA values for parameters and log likelihood
  }

  # set up initial parameter values
  Pexp <- 0.5
  Mconf <- 0.5
  mu_m <- 0
  rho <- 0

  params <- c(Pexp, Mconf, mu_m, rho)

  # set lower and upper parameter bound for the optimization
  lb <- c(0, 0, -5, -1)
  ub <- c(1, 1, 5, 1)

  #fit the model with hookejeeves
  hj_output <- hookejeeves(
    x0 = params,
    f = function(params)
      bim_error(params, observed_data),
    lb = lb,
    ub = ub,
    tol = 1e-08,
    target = Inf,
    maxfeval = Inf,
    info = FALSE
  )

  params <- hj_output$xmin

  ## if padding == 1, fit the model again with padding correction

  if (padding == 1) {
    # fit the model with hookejeeves (padding)
    hj_output_padding <- hookejeeves(
      x0 = params,
      f = function(params)
        bim_error(params, observed_data),
      lb = lb,
      ub = ub,
      tol = 1e-08,
      target = Inf,
      maxfeval = Inf,
      info = FALSE
    )

    params_padding <- hj_output_padding$xmin

    # extract rho
    params[4] <- params_padding[4]
  }

  # Define warning for padding
  if (!is.na(params[4]) && padding == 0 && abs(params[4]) > 0.98) {
    warning('The estimated value of rho is at edge. Fitting again with padding corrrection')
    w <- 1
  }

  # generate log likelihood
  err <- bim_error(params, observed_data)
  logL <- -err

  # create a matrix to store parameters along with their respective names
  params_matrix <- matrix(params, nrow = 1, byrow = TRUE)
  colnames(params_matrix) <- c('Pexp', 'Mconf', 'mu_m', 'rho')

  return(list(params = params_matrix,
              logL = logL,
              w = w))
}
