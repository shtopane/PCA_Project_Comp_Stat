
# Generates data
# N: number of observations
# variable_count: number of regressors to have
# error_term_constant: A constant used to transform the randomly generated error term by the formula:
#   sqrt(c * N) * epsilon
#   default is 4
dgp <- function(N,
                variable_count,
                mu,
                number_of_factors = 6,
                c_tilde = 4,
                case,
                inflate_beta = FALSE,
                error_term_constant = 4,
                beta_factor = 9,
                X_covariance_cluster_proportion = 0.20) {
  
  # Mu is random by design
  if (is.null(mu)) {
    mu <- rnorm(variable_count)
  }
  
  # Error term
  eps <- rnorm(N)
  
  # X's covariance input matrix
  X_covariance_raw <- rnorm(variable_count * N)
  X_covariance_raw_length <- length(X_covariance_raw)
  
  # Where to create correlations in the data
  X_cluster_index <-
    round(X_covariance_raw_length * X_covariance_cluster_proportion)
  
  X_covariance_raw_transformed <- X_covariance_raw

  if (X_cluster_index != 0) {
    # Make a cluster of X variables more alike
    X_covariance_raw_transformed[1:X_cluster_index] <- make_data_correlated(X_covariance_raw, X_cluster_index)
  }
  
  covariance_input_matrix <-
    matrix(X_covariance_raw_transformed, ncol = variable_count)
  
  # Create X covariance matrix with dimensions the covariance_input_matrix. Used in mvrnorm
  
  # size = 2000: this is the fastest one according to the library author. 
  # (used to determine how many units per group to create when calculating covariance)
  X_covariance <-
    bigcor(covariance_input_matrix, size = 2000, fun = "cov")
  
  # Transform "ff" format to matrix
  X_covariance <-
    X_covariance[1:nrow(X_covariance), 1:ncol(X_covariance)]
  
  X <-
    mvrnorm(N,
            mu = mu,
            Sigma = X_covariance,
            empirical = TRUE)
  
  # BETAS
  betas <- rnorm(variable_count)
  
  betas_cluster_index <-
    round(variable_count * X_covariance_cluster_proportion)
  
  betas_with_cluster <- betas
  
  if (inflate_beta == TRUE || betas_cluster_index != 0) {
    # Inflates beta max value.
    betas_max_value <- max(betas) * beta_factor
    
    # Select betas which will be tightly coupled with the response. In this sense we create a strong or weak factor
    # structure
    betas_with_cluster[1:betas_cluster_index] <- make_data_correlated(betas, betas_cluster_index, max_el = betas_max_value)
    
    # Flip the sign of the beta if X is negative
    if (max(X[, 1]) < 0) {
      betas_with_cluster[1] <- -betas_with_cluster[1]
    }
  }
  
  # Y & Data
  Y <- X %*% betas_with_cluster + eps
  data <- data.frame(Y, X)
  
  return(data)
}
