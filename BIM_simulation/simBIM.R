# function for the BIM simulation. Do not run this function directly.
# Use simBIM_Pexp_Mconf or simBIM_mu_m_rho instead.
BIM_simulation <- function(Pexp, Mconf, mu_m, rho, ntrial) {
  # transform parameters
  sigmal <- sqrt(1 / Pexp - 1)
  wavg <- qnorm(Mconf) * sqrt(1 + sigmal ^ 2 + 1 / (sigmal ^ 2))
  a <- 1 / (sigmal * sqrt(sigmal ^ 2 + 1))
  b <- wavg / sqrt(sigmal ^ 2 + 1)

  # prepare n*2 matrix of zeros for the observed data
  observed_data <- matrix(0, ncol = 2, nrow = ntrial)

  # generate a random sample x from a bivariate normal distribution
  # The covariance matrix is [1 rho; rho 1], specifying the variances and the covariance between the two variables.
  for (trial in 1:ntrial) {
    x <- mvrnorm(1, mu = c(mu_m, 0), Sigma = matrix(c(1, rho, rho, 1), nrow = 2))

    x_rec <- x[1]
    x0 <- x[2]

    # generate confidence values
    conf_pre <- pnorm(x0 * a + b) * 100
    conf <- rnorm(1, mean = conf_pre, sd = 0.025)
    conf <- pmin(pmax(conf, 0), 100)

    observed_data[trial, 1] <- conf

    # generate performance values
    observed_data[trial, 2] <- ifelse(x_rec > 0, 1, 0)
  }

  return(observed_data)
}
