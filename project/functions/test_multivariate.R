test_env <- new.env()
library("MASS")

set.seed(199)

test_env$N <- 192
# Number of Variables
test_env$p <- 108

# Make covariance matrix
# 0.189 0.181 0.
test_env$sigma <- rbind(
  c(0.189, 0.181, 0.173, 0.153, 0.122),
  c(0.181, 0.166, 0.151, 0.111, 0.090),
  c(0.207, 0.151, 0.155, 0.104, 0.060),
  c(0.153, 0.111, 0.104, 0.094, 0.040),
  c(0.122, 0.090, 0.060, 0.040, 0.176)
)
#test_env$sigma <- matrix(test_env$cc, nrow = test_env$p, ncol = test_env$p)

test_env$cov_matrix <- rbind(
  c(0.189, 0.181, 0.173, 0.153, 0.122),
  c(0.181, 0.166, 0.151, 0.111, 0.090),
  c(0.207, 0.151, 0.155, 0.104, 0.060),
  c(0.153, 0.111, 0.104, 0.094, 0.040),
  c(0.122, 0.090, 0.060, 0.040, 0.176)
)
test_env$sigma <- t(test_env$cov_matrix) %*% test_env$cov_matrix
test_env$sd <- runif(test_env$p, min = -0.99, max = 1)
test_env$X <- mvrnorm(test_env$N, mu = test_env$mu, Sigma = Sigma)
# Error
test_env$eps <- rnorm(test_env$N)
test_env$eps <- sqrt(4 * test_env$N) * test_env$eps
# BETAS
# With this coefficients it seems that all components are important
# -3.6, 5.1, 3.2, -0.9, 0.04
test_env$betas <- c(0.001, 15.1, -0.0002, 15.3, 0.04)
test_env$y <- test_env$X %*% test_env$betas + test_env$eps
test_env$data <- data.frame("Y" = test_env$y,
                            "X" = test_env$X)

# Plot data
# First two variables are highly correlated with Y, and the other 3 seem more like noise, but again there is some correlation
plot(test_env$X[, 4], test_env$y, col = c("red", "blue"))
cor(x = test_env$X[, 5], y = test_env$y)




# TEST: Create big covariance matrix
library("propagate") # lib for creating big correlation or covariance matrices

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
                INCREASE_FACTOR = 1,
                BETAS_INFLUENCE_PROPORTION = 0.03,
                BETAS_INCREASE_FACTOR = 9,
                X_COVARIANCE_CLUSTER_PROPORTION = 0.20) {
  # CONSTANTS ----
  X_COVARIANCE_CLUSTER_PROPORTION <-
    X_COVARIANCE_CLUSTER_PROPORTION#0.20
  INCREASE_FACTOR <- INCREASE_FACTOR#1
  BETAS_INFLUENCE_PROPORTION <- BETAS_INFLUENCE_PROPORTION#0.03
  BETAS_INCREASE_FACTOR <- BETAS_INCREASE_FACTOR#9
  # END CONSTANTS ----
  
  # Mu is random by design(?)
  if(is.null(mu)){
    mu <- rnorm(test_env)  
  }
  
  # Error term
  eps <- rnorm(N)
  eps <- sqrt(error_term_constant * N) * eps
  
  # X's
  X_covariance_raw <- rnorm(variable_count * N)
  
  X_cluster_index <-
    round(length(X_covariance_raw) * X_COVARIANCE_CLUSTER_PROPORTION)
  X_covariance_raw_transformed <- X_covariance_raw
  
  X_covariance_raw_transformed[1:X_cluster_index] <-
    seq(
      from = max(X_covariance_raw) * INCREASE_FACTOR,
      to = (max(X_covariance_raw) - sd(X_covariance_raw)),
      len = X_cluster_index
    )
  
  # Maybe just a random? PLAY WITH THIS
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
  # Important factors
  betas_cluster_index <-
    round(variable_count * BETAS_INFLUENCE_PROPORTION)
  betas_with_cluster <- betas
  betas_max_value <- max(betas) * BETAS_INCREASE_FACTOR
  
  betas_with_cluster[1:betas_cluster_index] <-
    seq((betas_max_value - sd(BETAS)), betas_max_value, len = betas_cluster_index)
  
  # Y & Data
  Y <- X %*% betas_with_cluster + test_env$eps
  data <- data.frame(Y, X)
  
  return(data)
}

data_trial <- dgp(N = test_env$N, variable_count = test_env$p, mu = test_env$mu)
  # CLUSTER_INDEX <- sample.int(test_env$N, round(test_env$N * 0.2))
  # ts.plot(TEST_X)
  
  
  
  
  PCR <-
  pcr(
    TEST_Y ~ .,
    data = DATA,
    scale = TRUE,
    subset = TRAIN,
    validation = "CV"
  )
# summary(PCR)
# biplot(PCR, which = "loadings")
# plot(RMSEP(PCR), legendpos = "topright")
# cor(x = TEST_X[, 22], y = TEST_Y)

# TEST WITH PLSR
PLSR <-
  plsr(
    TEST_Y ~ .,
    data = DATA,
    subset = TRAIN,
    scale = TRUE,
    validation = "CV"
  )
