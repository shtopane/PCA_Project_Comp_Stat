
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
                BETAS_INCREASE_FACTOR = 9,
                X_COVARIANCE_CLUSTER_PROPORTION = 0.20) {
  
  # Mu is random by design(?)
  if (is.null(mu)) {
    mu <- rnorm(test_env)
  }
  
  # Error term
  eps <- rnorm(N)
  # eps <- sqrt(error_term_constant * N) * eps
  
  # Inflates the variance of Y. If no transformations to data are done and we just add this vector
  # when constructing Y, our MSE gets inflated as well(so, we miss by a lot)
  # eps_transformed <- get_transformed_error_term(
  #   r = number_of_factors,
  #   N = N,
  #   c_tilde = c_tilde,
  #   eps = eps
  # )
  
  # X's
  X_covariance_raw <- rnorm(variable_count * N)
  X_covariance_raw_length <- length(X_covariance_raw)
  
  
  X_cluster_index <-
    round(X_covariance_raw_length * X_COVARIANCE_CLUSTER_PROPORTION)
  
  X_covariance_raw_transformed <- X_covariance_raw

  if (X_cluster_index != 0) {
    # Make a cluster of X variables more alike
    X_covariance_raw_transformed[1:X_cluster_index] <- introduce_correlations(X_covariance_raw, X_cluster_index)
  }
  
  covariance_input_matrix <-
    matrix(X_covariance_raw_transformed, ncol = variable_count)
  
  # size = 2000: this is the fastest one according to the library author. 
  # (used to determine how many units per group to create when calculating covariance)
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
  
  betas_cluster_index <-
    round(variable_count * X_COVARIANCE_CLUSTER_PROPORTION)
  
  betas_with_cluster <- betas
  
  if (inflate_beta == TRUE || betas_cluster_index != 0) {
    # make this 1 variable very important for Y
    betas_max_value <- max(betas) * BETAS_INCREASE_FACTOR
    
    # Select betas which will be tightly coupled with the response. In this sense we create a strong or weak factor
    # structure
    
    betas_with_cluster[1:betas_cluster_index] <- introduce_correlations(betas, betas_cluster_index, max_el = betas_max_value)
      
    
    
    # Flip the sign of the beta if X is negative
    if (max(X[, 1]) < 0) {
      betas_with_cluster[1] <- -betas_with_cluster[1]
    }
  }
  
  
  # Y & Data
  Y <- X %*% betas_with_cluster + eps
  data <- data.frame(Y, X)
  
  # pdf(file = paste("images/data_plots/Y_and_variance", case, ".pdf"))
  # par(mfrow = c(2, 2))
  # # (Y, col = "black", main = paste("Y, Variance of Y: ", round(var(Y), digits = 2)))
  # corrplot::corrplot(cor(data), title = paste("Variance of Y: ", round(var(Y), digits = 2)), mar=c(0,0,1,0))
  # dev.off()
  
  return(data)
}
