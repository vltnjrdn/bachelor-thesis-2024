#install.packages("pso")
library(pso)

# The fitting function for BIM applied to recall tasks with continuous confidence.
# Please do not run this function directly. Use the call_fit_BIM function instead.

fit_bim <- function(observed_data, padding) {
  # define warnings
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

  if (length(unique(observed_data[, 2])) == 1) {
    warning('Performance for all trials is the same. Estimation of parameters mu_m and rho is inaccurate.')
    w <- 1
  }

  # set up initial parameter values
  Pexp <- 0.5
  Mconf <- 0.5
  mu_m <- 0
  rho <- 0

  params <- c(Pexp, Mconf, mu_m, rho)

  # set lower and upper parameter bound for the optimization
  lb <- c(0, 0,-5,-1)
  ub <- c(1, 1, 5, 1)

  # fit the model with particle swarm optimization
  psoptim_output <- psoptim(
    par = params,
    fn = function(params)
      bim_error(params, observed_data),
    lower = lb,
    upper = ub,
    control = list(
      s = 1000,
      fnscale = 1,
      maxf = -Inf,
      maxit = Inf
    )
  )

  params <- psoptim_output$par

  # if padding == 1, fit the model again with padding correction
  if (padding == 1) {
    # paricle swarm optimization (padding)
    psoptim_output_padding <- psoptim(
      par = params,
      fn = function(params)
        bim_error_padding(params, observed_data),
      lower = lb,
      upper = ub,
      control = list(
        s = 1000,
        fnscale = 1,
        maxf = -Inf,
        maxit = 1000
      )
    )

    params_padding <- psoptim_output_padding$par

    # extract rho
    params[4] <- params_padding[4]
  }

  # define warning for padding
  if (padding == 0 && abs(params[4]) > 0.98) {
    warning('The estimated value of rho is at edge. Consider setting padding = 1.')
    w <- 1
  }

  # generate log likelihood
  err <- bim_error(params, observed_data)
  logL <- -err

  return(list(params = params, logL = logL, w = w))
}
