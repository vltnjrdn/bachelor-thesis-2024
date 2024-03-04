# Fit BIM to data from recall tasks with continuous confidence ratings on a continuous scale.
# This function requires BIM_error, BIM_error_padding and fit_BIM to be loaded into the global environment.
# Can be used with any version of the BIM fitting function.

## INPUTS
# * observed_data
#
# N-by-2 dataframe containing data of confidence ratings and recall performance.
# N represents the total number of trials, and each row represents a trial.
# The first column is confidence rating and the second column is recall performance in each trial.
#
# Confidence ratings should be on a continuous scale from 0 (not confident at all) to 100 (completely confident).
# Recall performance should be 0 (incorrect) or 1 (correct).
#
# * padding
#
# Add a small correction to data during model fitting through setting padding = 1.
# There is no padding correction when padding = 0. Default value is 0.
# setting padding = 1 is only recommended when the fitted value of rho is at edge
# (i.e., > 0.98 or < -0.98) with padding = 0.
# Padding correction can slightly improve the performance of parameter recovery.

## OUTPUTS
#
# * params
#
# A vector containing fitted value of the parameters in BIM
# (from left to right: Pexp, Mconf, mu_m and rho)
#
# * logL
# Log likelihood of the data fit.
#
# * w
# w = 1 when any warning message is triggered.
# w = 0 when there is no warning message.

# read your observed data
filepath <- '../BIM_Main_Fitting_Function/Observed_Data_Example.txt'
observed_data <- as.data.frame(read.csv(filepath))

# fit the model
fit_result <- fit_bim(observed_data, padding = 0)
print(fit_result)
