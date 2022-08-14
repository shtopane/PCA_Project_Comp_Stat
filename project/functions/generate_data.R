# Generates data
# N: number of observations
# variable_count: number of regressors to have
# error_term_constant: A constant used to transform the randomly generated error term by the formula:
#   sqrt(c * N) * epsilon
#   default is 4
dgp <- function(N,
                variable_count,
                mu,
                error_term_constant = 4,
                BETAS_INCREASE_FACTOR = 9,
                X_COVARIANCE_CLUSTER_PROPORTION = 0.20) {

  # Mu is random by design(?)
  if (is.null(mu)) {
    mu <- rnorm(test_env)
  }
  
  # Error term
  eps <- rnorm(N)
  eps <- sqrt(error_term_constant * N) * eps
  
  # X's
  X_covariance_raw <- rnorm(variable_count * N)
  
  # TODO: TEST
  X_cluster_index <-
    round(length(X_covariance_raw) * X_COVARIANCE_CLUSTER_PROPORTION)
  
  X_covariance_raw_transformed <- X_covariance_raw
  
  if (X_cluster_index != 0) {
    # Make a cluster of X variables more alike
    X_covariance_raw_transformed[1:X_cluster_index] <-
      X_covariance_raw_transformed[1:X_cluster_index] - runif(X_cluster_index, min = 0, max = 1)
  }
  
  covariance_input_matrix <-
    matrix(X_covariance_raw_transformed, ncol = variable_count)
  # size = 2000: this is the fastest one according to the library author. (used to determine how many units per group to create when calculating covariance)
  X_covariance <-
    bigcor(covariance_input_matrix, size = 2000, fun = "cov")
  
  # Transform "ff" format to matrix [EXTRACT INTO FUNCTION]
  X_covariance <-
    X_covariance[1:nrow(X_covariance), 1:ncol(X_covariance)]
  
  X <-
    mvrnorm(N,
            mu = mu,
            Sigma = X_covariance,
            empirical = TRUE)
  
  # BETAS
  betas <- rnorm(variable_count)
  # Select betas which will be tightly coupled with the response. In this sense we create a strong or weak factor
  # structure
  betas_cluster_index <-
    round(variable_count * X_COVARIANCE_CLUSTER_PROPORTION)
  betas_with_cluster <- betas
  
  
  if (betas_cluster_index != 0) {
    betas_max_value <- max(betas) * BETAS_INCREASE_FACTOR
    betas_with_cluster[1:betas_cluster_index] <-
      seq((betas_max_value - sd(betas)), betas_max_value, len = betas_cluster_index)
    
    # TODO: Flip the sign of the beta if X is negative(?)
    # NOT SURE IF CORRECT
    if (max(X[, 1]) < 0) {
      betas_with_cluster[1] <- -betas_with_cluster[1]
    }
  }
  
  
  # Y & Data
  Y <- X %*% betas_with_cluster + eps
  data <- data.frame(Y, X)
  
  return(data)
}
