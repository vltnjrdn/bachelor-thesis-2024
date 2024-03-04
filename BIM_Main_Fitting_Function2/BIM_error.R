# The error function for BIM applied to recall tasks with continuous confidence.
# Please do not run this function directly. Use the call_fit_BIM function instead.

bim_error <- function(params, observed_data) {
  ## get data and parameters
  # parameters
  Pexp <- params[1]
  Mconf <- params[2]
  mu_m <- params[3]
  rho <- params[4]

  # data
  conf <- observed_data[, 1]
  rec <- observed_data[, 2]

  ntrial <- length(conf)

  # check bounds for parameters
  if (rho < -0.99 ||
      rho > 0.99 ||
      Pexp < 0.01 ||
      Pexp > 0.99 ||
      Mconf < 0.01 ||
      Mconf > 0.99 ||
      mu_m < -5 ||
      mu_m > 5) {
    err <- 10000

    return(err)
  }

  # transform the parameters Pexp and Mconf into two new parameters (a and b)
  sigmal <- sqrt(1 / Pexp - 1)
  wavg <- qnorm(Mconf) * sqrt(1 + sigmal ^ 2 + 1 / (sigmal ^ 2))

  a <- 1 / (sigmal * sqrt(sigmal ^ 2 + 1))
  b <- wavg / sqrt(sigmal ^ 2 + 1)

  # create a grid for x0
  count <- 100  # steps in grid
  lb_x0 <- -5  # lower bound for x0
  ub_x0 <- 5  # upper bound for x0
  rx0 <- (ub_x0 - lb_x0) / (count - 1) # step size of the grid

  X0 <- seq(lb_x0, ub_x0, by = rx0)
  X0 <- matrix(rep(X0, each = ntrial), ncol = count, byrow = FALSE)

  # replicate the observed data across all trials 100 times for every point in the x0_grid
  conf_new <- matrix(rep(conf, each = count), ncol = count, byrow = TRUE) / 100
  rec_new <- matrix(rep(rec, each = count), ncol = count, byrow = TRUE)

  # calculate the likelihood of every x0 in the grid
  npdf_x0 <- rx0 * dnorm(X0)

  # calculate the likelihood of recall for each trial and each x0 in the grid
  lik_rec <- (1 - pnorm(0, mu_m + rho * X0, sqrt(1 - rho ^ 2)))

  ## calculate the the predicted confidence for each trial and each x0 in the grid
  pred_conf <- pnorm(X0 * a + b)

  # calculate the overall log likelihood for each trial by
  # multiplication of the likelihood of x0, the probability of the observed confidence given the predicted confidence
  # and the likelihood of recall or no recall
  grid_loglik <- npdf_x0 *
                 dnorm(conf_new, pred_conf, 0.025) *
                 (rec_new * lik_rec + (1 - rec_new) *
                 (1 - lik_rec))

  # Check whether some of the row_sums in the loglik_padding grid = 0 and extract their indices
  row_sums <- rowSums(grid_loglik)
  zero_indices <- which(row_sums == 0)

  # add a small value to this row_sums to avoid log(0)
  if (length(zero_indices) > 0) {
    grid_loglik[zero_indices, ] <- grid_loglik[zero_indices, ] + 1e-10
  }

  loglik <- log(rowSums(grid_loglik))

  #calculate error
  err <- (-1) * sum(loglik)  # sum of negative log likelihood

  return(err)
}
