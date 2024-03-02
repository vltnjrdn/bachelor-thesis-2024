multi_fitBIM <- function(data, nVpn, df_name) {
  
  # Initialize matrix to store results
  result_matrix <- matrix(NA, nrow = nVpn, ncol = 5, 
                          dimnames = list(NULL, c("Pexp", "Mconf", "mu_m", "rho", "logL")))
  
  for (i in 1:nVpn) {
    # Extract data for current person
    start_row <- (i - 1) * nrow(data) / nVpn + 1
    end_row <- i * nrow(data) / nVpn
    observed_data <- data[start_row:end_row, , drop = FALSE]
    observed_data <- as.data.frame(observed_data)
    
    # Call fit_bim function
    fit_result <- fit_bim(observed_data, padding = 0, df_name, i)
    
    # Store results in result_matrix
    result_matrix[i, 1:4] <- fit_result$params
    result_matrix[i, 5] <- fit_result$logL
  }
  
  return(result_matrix)
}
