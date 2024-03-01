# install.packages("numDeriv")

library(numDeriv)

fit_bim <- function(observed_data, padding) {
  #define warnings
  w <- 0

  if (!exists('padding') || is.null(padding)) {
    padding <- 0
  }

  if (!padding %in% c(0, 1)) {
    stop('Padding must be set as 0 or 1')
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

  # set lower and upper parameter bounds for the optimization
  lb <- c(0, 0, -5, -1)
  ub <- c(1, 1, 5, 1)

  ### fit the model with general-purpose optimization

  # gradient for the optim function
  gradient_bim_error <- function(params) {
    gradient <- grad(
      func = function(params)
        bim_error(params, observed_data),
      x = params,
    )

    return(gradient)
  }

  # general purpose optimization
  optim_output <- optim(
    par = params,
    fn = function(params)
      bim_error(params, observed_data),
    method = 'L-BFGS-B',
    gr = gradient_bim_error,
    lower = lb,
    upper = ub,
    control = list(
      trace = FALSE,
      maxit = 1000,
      fnscale = 1,
      factr = 1e-10
    )
  )

  params <- optim_output$par

  ## if padding == 1, fit the model again with padding correction
  if (padding == 1) {
    # gradient for the optim function (padding)
    gradient_bim_error_padding <- function(params) {
      gradient <- grad(
        func = function(params)
          bim_error(params, observed_data),
        x = params,
      )

      return(gradient)
    }

    # general purpose optimization (padding)
    optim_output_padding <- optim(
      par = params,
      fn = function(params)
        bim_error(params, observed_data),
      method = 'L-BFGS-B',
      gr = gradient_bim_error,
      lower = lb,
      upper = ub,
      control = list(
        trace = FALSE,
        maxit = 1000,
        fnscale = 1,
        factr = 1e-10
      )
    )

    params_padding <- optim_output_padding$par

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

  return(list(params = params,
              logL = logL,
              w = w))
}
