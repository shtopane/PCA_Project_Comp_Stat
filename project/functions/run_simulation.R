








# test_env$cov_matrix <- rbind(
#   c(0.189, 0.181, 0.173, 0.153, 0.122),
#   c(0.181, 0.166, 0.151, 0.111, 0.090),
#   c(0.207, 0.151, 0.155, 0.104, 0.060),
#   c(0.153, 0.111, 0.104, 0.094, 0.040),
#   c(0.122, 0.090, 0.060, 0.040, 0.176)
# )
#
# test_env$sigma <- t(test_env$cov_matrix) %*% test_env$cov_matrix
# test_env$sd <- runif(test_env$p, min = -0.99, max = 1)
# test_env$X <- mvrnorm(test_env$N, mu = test_env$mu, Sigma = Sigma)
# # Error
# test_env$eps <- rnorm(test_env$N)
# test_env$eps <- sqrt(4 * test_env$N) * test_env$eps
#
# # BETAS
# # With this coefficients it seems that all components are important
# # -3.6, 5.1, 3.2, -0.9, 0.04
# test_env$betas <- c(0.001, 15.1, -0.0002, 15.3, 0.04)
# test_env$y <- test_env$X %*% test_env$betas + test_env$eps
# test_env$data <- data.frame("Y" = test_env$y,
#                             "X" = test_env$X)
#
# # Plot data
# # First two variables are highly correlated with Y, and the other 3 seem more like noise, but again there is some correlation
# plot(test_env$X[, 4], test_env$y, col = c("red", "blue"))
# cor(x = test_env$X[, 5], y = test_env$y)


# TEST: Create big covariance matrix
library("propagate") # lib for creating big correlation or covariance matrices
library("MASS")
library("corrplot") # display correlations
source("project/functions/helpers.R")
source("project/functions/generate_data.R")

test_env <- new.env()

set.seed(199)

# DEMO ----
test_env$N <- 200 #192
# Number of Variables
test_env$p <- 15#108


# Generate mean for variables
test_env$mu <-
  rnorm(test_env$p) #rep(1, test_env$p) # runif(test_env$p, min = -1, max = 2.5)

test_env$correlated_variables_with_response <- c(
  # strong factors are 1 variable
  get_percentage_of_number(1, test_env$p),
  # strong factors are 1/3 of dataset
  get_percentage_of_number(round(test_env$p / 3), test_env$p),
  # strong factors are all of the dataset
  get_percentage_of_number(round(test_env$p / 1), test_env$p),
  0
)

# NOTES:
# BIG CORRELATION BETWEEN Y & first X's:

# X_COVARIANCE_CLUSTER_PROPORTION = 0.04(one variable): DOES THE TRICK: PCR BREAKS!
train <- sample(test_env$N, test_env$N / 2)
test  <- -train

test_env$data_trial <-
  dgp(
    N = test_env$N,
    variable_count = test_env$p,
    mu = test_env$mu,
    X_COVARIANCE_CLUSTER_PROPORTION = test_env$correlated_variables_with_response[4]
  )

# SEPERATE X FROM Y
test_env$x <-
  model.matrix(Y ~ ., data = test_env$data_trial)[, -1] # throw out intercept
test_env$y <- test_env$data_trial$Y

test_env$data_corr <- cor(test_env$data_trial)
corrplot(test_env$data_corr, method = "circle")

PCR <-
  pcr(
    Y ~ .,
    data = test_env$data_trial,
    subset = train,
    scale = TRUE,
    validation = "CV"
  )

#summary(PCR)
plot(RMSEP(PCR), legendpos = "topright")

test_env$PCR_MIN_RMSEP_COMP <- get_optimal_num_of_components(PCR)

pcr.pred <-
  predict(PCR, test_env$x[test, ], ncomp = test_env$PCR_MIN_RMSEP_COMP)
get_MSE(pcr.pred, test_env$y[test])


# TEST WITH PLSR
PLSR <-
  plsr(
    Y ~ .,
    data = test_env$data_trial,
    subset = train,
    scale = TRUE,
    validation = "CV"
  )
# biplot(PLSR, which = "loadings")
plot(RMSEP(PLSR), legendpos = "topright")

test_env$PLSR_MIN_RMSEP_COMP <- get_optimal_num_of_components(PLSR)
plsr.pred <-
  predict(PLSR, test_env$x[test, ], ncomp = test_env$PLSR_MIN_RMSEP_COMP)
get_MSE(plsr.pred, test_env$y[test])
# END DEMO ----

# @var simulation_option_list
# variables related to the simulation itself: parameters that vary between runs like N, p, runs, ets
# @var dgp_option_list
simulation <-
  function(dgp_option_list = list(error_term_constant = 4,
                                  BETAS_INCREASE_FACTOR = 9),
           simulation_option_list = list(N = 200, p = 15, runs = 100)) {
    set.seed(13456)
    print(simulation_option_list$N)
    print(simulation_option_list$p)
    print(simulation_option_list$runs)
    N <- simulation_option_list$N
    # Number of Variables
    p <- simulation_option_list$p
    
    # Split train and test sample
    train <- sample(N, N / 2)
    test  <- -train
    
    # Generate mean of variables
    mu <- rnorm(p)
    # Simulation runs
    runs <- simulation_option_list$runs
    
    # Proportion of correlated variables with the response variable
    # % of p is giving us 1 variable, 5 variables and so on
    # 1%, 5%, 25%, 0%
    correlated_variables_with_response <- c(
      # strong factors are 1 variable
      get_percentage_of_number(1, p),
      # strong factors are 1/3 of data set
      get_percentage_of_number(round(p / 3), p),
      # strong factors are all of the data set
      get_percentage_of_number(round(p / 1), p),
      0
    )
    
    correlated_variables_with_response_length <-
      length(correlated_variables_with_response)
    # Create MSE vectors
    MSE_PCR <-
      matrix(ncol = correlated_variables_with_response_length, nrow = runs)
    MSE_PLSR <-
      matrix(ncol = correlated_variables_with_response_length, nrow = runs)
    PCR_ncomp_tracker <-
      matrix(ncol = correlated_variables_with_response_length, nrow = runs)
    PLSR_ncomp_tracker <-
      matrix(ncol = correlated_variables_with_response_length, nrow = runs)
    
    # Draw data
    # Simulate runs times with the 4 correlated variables proportions
    for (i in 1:runs) {
      for (j in 1:correlated_variables_with_response_length) {
        # Draw data ----
        dgp_data <-
          dgp(
            N = N,
            variable_count = p,
            mu = mu,
            error_term_constant = dgp_option_list$error_term_constant,
            BETAS_INCREASE_FACTOR = dgp_option_list$BETAS_INCREASE_FACTOR,
            X_COVARIANCE_CLUSTER_PROPORTION = correlated_variables_with_response[j]
          )
        # ----
        
        # Seperate X's from Y's
        x <-
          model.matrix(Y ~ ., data = dgp_data)[, -1] # throw out intercept
        y <- dgp_data$Y
        
        # Do PCR ----
        PCR <-
          pcr(
            Y ~ .,
            data = dgp_data,
            subset = train,
            scale = TRUE,
            validation = "CV"
          )
        
        PCR_ncomp <- get_optimal_num_of_components(PCR)
        
        # TODO: Track ncomp
        PCR_ncomp_tracker[i, j] <- PCR_ncomp
        
        PCR_predict <- predict(PCR, x[test, ], ncomp = PCR_ncomp)
        # Save MSE
        MSE_PCR[i, j] <- get_MSE(PCR_predict, y[test])
        # ----
        
        # Do PLSR
        PLSR <-
          plsr(
            Y ~ .,
            data = dgp_data,
            subset = train,
            scale = TRUE,
            validation = "CV"
          )
        
        PLSR_ncomp <- get_optimal_num_of_components(PLSR)
        
        # TODO: Track ncomp
        PLSR_ncomp_tracker[i, j] <- PLSR_ncomp
        
        PLSR_predict <- predict(PLSR, x[test, ], ncomp = PLSR_ncomp)
        # Save MSE
        MSE_PLSR[i, j] <- get_MSE(PLSR_predict, y[test])
        # ----
      }
    }
    
    return(
      list(
        "MSE_PCR" = MSE_PCR,
        "MSE_PLSR" = MSE_PLSR,
        "PCR_ncomp" = PCR_ncomp_tracker,
        "PLSR_ncomp" = PLSR_ncomp_tracker
      )
    )
    
  }

# Simulation 0 with default parameters
test_env$dgp_options_default <-
  list(error_term_constant = 4,
       BETAS_INCREASE_FACTOR = 9)
test_env$simulation_options_default <-
  list(N = 200, p = 15, runs = 50)

test_env$MSE <-
  simulation(simulation_option_list = test_env$simulation_options_default)
test_env$MSE_stats <- save_mean_mse_and_return_stats(test_env$MSE)


plot_MSE_comparison_boxplot(test_env$MSE, test_env$MSE_stats)

# Simulation 1
# Simulate more variables
test_env$simulation_options_1 <- test_env$simulation_options_default
test_env$simulation_options_1$p <- 108
test_env$simulation_options_1$N <- 192
test_env$simulation_options_1$runs <- 10

# Surprisingly, here PCR is worse with cases 2,3
# So, where 1/3 of the variables are correlated with Y
# and where all variables are correlated with Y
test_env$simulation_results1 <-
  simulation(simulation_option_list = test_env$simulation_options_1)
test_env$MSE_stats1 <-
  save_mean_mse_and_return_stats(test_env$simulation_results1)
# Get mean components used by each method
# PCR: 15
# PLSR: 1
test_env$ncomp_mean1 <-
  get_mean_number_of_components_by_method(
    list(PCR_ncomp = test_env$simulation_results1$PCR_ncomp, PLSR_ncomp = test_env$simulation_results1$PLSR_ncomp))
    
plot_MSE_comparison_boxplot(test_env$simulation_results1, test_env$MSE_stats1)

# Simulation 2: small number of variables: 5
test_env$simulation_options_2 <- test_env$simulation_options_default
test_env$simulation_options_2$p <- 5
test_env$simulation_options_2$N <- 100


test_env$simulation_results2 <-
  simulation(simulation_option_list = test_env$simulation_options_2)

test_env$MSE_stats2 <-
  save_mean_mse_and_return_stats(test_env$simulation_results2)

test_env$ncomp_mean2 <-
  get_mean_number_of_components_by_method(
    list(PCR_ncomp = test_env$simulation_results2$PCR_ncomp, PLSR_ncomp = test_env$simulation_results2$PLSR_ncomp))

plot_MSE_comparison_boxplot(test_env$simulation_results2, test_env$MSE_stats2)

# Simulation 3: Realistic case + error_constant decreased to 1
test_env$dgp_options_3 <- test_env$dgp_options_default
test_env$dgp_options_3$error_term_constant <- 1

test_env$simulation_results3 <-
  simulation(simulation_option_list = test_env$simulation_options_1, dgp_option_list = test_env$dgp_options_3)

test_env$MSE_stats3 <-
  save_mean_mse_and_return_stats(test_env$simulation_results2)

test_env$ncomp_mean3<-
  get_mean_number_of_components_by_method(
    list(PCR_ncomp = test_env$simulation_results3$PCR_ncomp, PLSR_ncomp = test_env$simulation_results3$PLSR_ncomp))

plot_MSE_comparison_boxplot(test_env$simulation_results3, test_env$MSE_stats3)
