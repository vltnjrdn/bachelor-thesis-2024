# function for quality checks. Can be used to compare different fit_BIM functions.
# runs the fit_BIM function n_interation times for the same data set and computes the mean values and variances for the fitted parameters and the logL
# runs the fit_BIM function again n_iteration times for the same data set to measure the mean and the variance of the elapsed time

fit_result <- fit_bim(observed_data, padding = 0)
print(fit_result)
class(observed_data)

### quality checks

## number of iterations
n_iterations <- 100

## mean and variances of the parameters and the logL

# prepare matrix/list
params <- matrix(0, nrow = n_iterations, ncol = 4)
logL <- list()

# run the fit_BIM function ntrial times
for (i in 1:n_iterations) {
  fit_result <- fit_bim(observed_data, padding = 0)

  # save params
  params[i, ] <- fit_result$params

  # save logL
  logL[i] <- fit_result$logL
}

# average and variance params
params_mean <- colMeans(params)
params_var <- apply(params, 2, var)

# average and variance logL
logL_mean <- mean(unlist(logL))
logL_var <- var(unlist(logL))

# output of the results
cat(
  'Results:\n',
  'average parameters:\n',
  params_mean,
  '\n',
  'variance parameters:\n',
  params_var,
  '\n',
  'average log-likelihood:\n',
  logL_mean,
  '\n',
  'variance log-liklihood:\n',
  logL_var,
  '\n'
)


## time measurement

# prepare list
timings <- list()

for (i in 1:n_iterations) {
  # elapsed time for each iteration
  timing_result <- system.time(fit_bim(observed_data, padding = 0))

  # save elapsed time
  timings[i] <- timing_result[3]
}

# mean elapsed time
average_time <- mean(unlist(timings))

# variance elapsed time
variance_time <- var(unlist(timings))

cat(
  'average elapsed time:\n',
  average_time,
  '\n',
  'variance elapsed time:\n',
  variance_time
)
