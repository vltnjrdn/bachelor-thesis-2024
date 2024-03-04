# Fitting FUnction to fit multiple participants.
# Do not use this function directly. Use call_multiple_fit_BIM instead.

# install.packages("adagio")
library(adagio)

fit_bim <- function(observed_data, padding, df_name, i) {
  #define warnings
  w <- 0
  npadding <-0

  if (!exists("padding") || is.null(padding)) {
    padding <- 0
  }

  if (!padding %in% c(0, 1)) {
    stop("padding must be set as 0 or 1")
  }

  if (length(unique(observed_data[, 1])) == 1) {
    warning('Confidence ratings for all trials are the same. Estimation of parameters Pexp and rho is inaccurate.')
    w <- 1
  }

  if (length(unique(observed_data[, 2])) == 1) {
    warning(paste0(
      "Performance for all trials in ID ", i, ", ", df_name," is the same. Estimation of parameters mu_m and rho is inaccurate."))
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

  # fit the model with hookejeeves
  hj_output <-
    hookejeeves(x0 = params,
                f = function(params)
                  bim_error(params, observed_data),
                lb = lb,
                ub = ub,
                tol = 1e-08,
                target = Inf,
                maxfeval = Inf,
                info = FALSE)

  params <- hj_output$xmin

  # apply padding correction if rho is higher than 0.98 or smaller than -0.98
  if (abs(params[4]) > 0.98) {
    # fit the model with hookejeeves (padding)
    hj_output_padding <-
      hookejeeves(x0 = params,
                  f = function(params)
                    bim_error(params, observed_data),
                  lb = lb,
                  ub = ub,
                  tol = 1e-08,
                  target = Inf,
                  maxfeval = Inf,
                  info = FALSE)

    params_padding <- hj_output_padding$xmin

    # extract rho
    params[4] <- params_padding[4]
  }

  # generate log likelihood
  err <- bim_error(params, observed_data)
  logL <- -err

  # create a matrix to store parameters along with their respective names
  params_matrix <- matrix(params, nrow = 1, byrow = TRUE)
  colnames(params_matrix) <- c("Pexp", "Mconf", "mu_m", "rho")

  return(list(params = params_matrix, logL = logL, w = w))
}
