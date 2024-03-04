# The error function with padding correction for BIM applied to recall tasks with continuous confidence.
# Please do not run this function directly. Instead, use call_multiple_fit_BIM.

bim_error_padding <- function(params, observed_data) {
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
  if (rho < -0.99 || rho > 0.99 ||
      Pexp < 0.01 || Pexp > 0.99 ||
      Mconf < 0.01 || Mconf > 0.99 ||
      mu_m < -5 || mu_m > 5) {
    err <- 10000
    return(err)
  }

  # transform the parameters Pexp and Mconf into two new parameters (a and b)
  sigmal <- sqrt(1 / Pexp - 1)
  wavg <- qnorm(Mconf) * sqrt(1 + sigmal ^ 2 + 1 / (sigmal ^ 2))

  a <- 1 / (sigmal * sqrt(sigmal ^ 2 + 1))
  b <- wavg / sqrt(sigmal ^ 2 + 1)

  # create grid for x0
  count <- 100  # steps in grid
  lb_x0 <- -5  # lower bound for x0
  ub_x0 <- 5  # upper bound for x0
  rx0 <- (ub_x0 - lb_x0) / (count - 1) # step size of the grid

  X0 <- seq(lb_x0, ub_x0, by = rx0)
  X0 <- matrix(rep(X0, ntrial), ncol = count)

  # calculate normal pdf for the x0 grid
  npdf <- rx0 * dnorm(X0)

  # calculate the likelihood for recall for each trial
  lik_rec <- (1 - pnorm(-mu_m, rho * X0, sqrt(1 - rho ^ 2)))

  # replicate the observed data across all trials 100 times for every point in the x0_grid
  conf_new <- matrix(rep(conf, each = count), ncol = count)
  rec_new <- matrix(rep(rec, each = count), ncol = count)

  # calculate the log likelihood for each trial
  grid_loglik <- npdf *
                 dnorm(conf_new / 100, pnorm(X0 * a + b), 0.025) *
                 (rec_new * lik_rec + (1 - rec_new) * (1 - lik_rec))

  # check whether some of the row_sums in the loglik_padding grid = 0 and extract their indices
  row_sums <- rowSums(grid_loglik)
  zero_indices <- which(row_sums == 0)

  # add a small value to this row_sums to avoid log(0)
  if (length(zero_indices) > 0) {
    grid_loglik[zero_indices, ] <- grid_loglik[zero_indices, ] + 1e-10
  }

  loglik <- log(rowSums(grid_loglik))

  err <- (-1) * sum(loglik)

  ## calculate the log likelihood for the padding trials

  # duplicate all confidence trials with recall = 0 for one set and recall = 1 for the second
  conf_padding <- c(conf, conf)
  rec_padding <- c(rep(0, ntrial), rep(1, ntrial))

  ntrial_padding <- length(conf_padding)

  # create a grid for x0
  X0 <- seq(lb_x0, ub_x0, by = rx0)
  X0_padding <- matrix(rep(X0, each = ntrial_padding),
                       ncol = count,
                       byrow = TRUE)

  # calculate normal pdf for the x0 grid
  npdf <- rx0 * dnorm(X0_padding)

  # calculate the likelihood for recall for each trial
  lik_rec <- (1 - pnorm(0, mu_m + rho * X0_padding, sqrt(1 - rho ^ 2)))

  # replicate the observed data across all trials 100 times for every point in the x0_grid
  conf_padding_new <- matrix(rep(conf_padding, each = count),
                             ncol = count,
                             byrow = TRUE)
  rec_padding_new <- matrix(rep(rec_padding, each = count),
                            ncol = count,
                            byrow = TRUE)

  ## calculate the log likelihood for each trial
  grid_loglik_padding <- npdf *
                         dnorm(conf_padding_new / 100, pnorm(X0_padding * a + b), 0.025) *
                         (rec_padding_new * lik_rec + (1 - rec_padding_new) * (1 - lik_rec))

  # Check whether some of the row_sums in the loglik_padding grid = 0 and extract their indices
  row_sums <- rowSums(grid_loglik)
  zero_indices <- which(row_sums == 0)

  # add a small value to this row_sums to avoid log(0)
  if (length(zero_indices) > 0) {
    grid_loglik[zero_indices, ] <- grid_loglik[zero_indices, ] + 1e-10
  }

  loglik_padding <- log(rowSums(grid_loglik_padding))

  # Correct the log likelihood based on memory performance. Estimation of rho
  # is more likely to be inaccurate when performance is more extreme (i.e.,
  # more close to 0 or 1) and a larger correction is added
  err_padding <- -sum(loglik_padding) / ntrial_padding * abs(mean(rec) - 0.5)

  err <- err + err_padding

  return(err)
}
