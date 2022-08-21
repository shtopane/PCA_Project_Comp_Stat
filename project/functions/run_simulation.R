library("propagate") # lib for creating big correlation or covariance matrices
library("MASS")
library("pls")
library("corrplot") # display correlations

# source("project/functions/helpers.R")
# source("project/functions/generate_data.R")

# test_env <- new.env()
# 
# set.seed(199)
# 
# 
# # DEMO ----
# test_env$N <- 100
# # Number of Variables
# test_env$p <- 15
# 
# 
# # Generate mean for variables
# test_env$mu <-
#   rnorm(test_env$p) #rep(1, test_env$p) # runif(test_env$p, min = -1, max = 2.5)
# 
# test_env$correlated_variables_with_response <- c(
#   # correlated vars are 1 variable
#   get_percentage_of_number(1, test_env$p),
#   # correlated vars are 1/3 of dataset
#   get_percentage_of_number(round(test_env$p / 3), test_env$p),
#   # correlated vars 1/10
#   get_percentage_of_number(round(test_env$p / 10), test_env$p),
#   # correlated vars are all of the dataset
#   get_percentage_of_number(round(test_env$p / 1), test_env$p),
#   0
# )
# 
# # NOTES:
# # BIG CORRELATION BETWEEN Y & first X's:
# 
# # X_COVARIANCE_CLUSTER_PROPORTION = 0.04(one variable): DOES THE TRICK: PCR BREAKS!
# train <- sample(test_env$N, test_env$N / 2)
# test  <- -train
# 
# test_env$data_trial <-
#   dgp(
#     N = test_env$N,
#     variable_count = test_env$p,
#     mu = test_env$mu,
#     inflate_beta = FALSE,
#     BETAS_INCREASE_FACTOR = 2,
#     X_COVARIANCE_CLUSTER_PROPORTION = test_env$correlated_variables_with_response[5]
#   )
# 
# plot.ts(test_env$data_trial[, 1])
# 
# # SEPERATE X FROM Y
# test_env$x <-
#   model.matrix(Y ~ ., data = test_env$data_trial)[, -1] # throw out intercept
# test_env$y <- test_env$data_trial$Y
# 
# test_env$data_corr <- cor(test_env$data_trial)
# corrplot::corrplot(test_env$data_corr)
# 
# PCR <-
#   pcr(
#     Y ~ .,
#     data = test_env$data_trial,
#     subset = train,
#     scale = TRUE,
#     validation = "CV"
#   )
# 
# #summary(PCR)
# 
# 
# test_env$PCR_MIN_RMSEP_COMP <- get_optimal_num_of_components(PCR)
# 
# pcr.pred <-
#   predict(PCR, test_env$x[test,], ncomp = test_env$PCR_MIN_RMSEP_COMP)
# get_MSE(pcr.pred, test_env$y[test])
# 
# 
# # TEST WITH PLSR
# PLSR <-
#   plsr(
#     Y ~ .,
#     data = test_env$data_trial,
#     subset = train,
#     scale = TRUE,
#     validation = "CV"
#   )
# # biplot(PLSR, which = "loadings")
# par(mfrow=c(1,2))
# plot(RMSEP(PCR), legendpos = "topright")
# plot(RMSEP(PLSR), legendpos = "topright")
# 
# test_env$PLSR_MIN_RMSEP_COMP <- get_optimal_num_of_components(PLSR)
# plsr.pred <-
#   predict(PLSR, test_env$x[test,], ncomp = test_env$PLSR_MIN_RMSEP_COMP)
# get_MSE(plsr.pred, test_env$y[test])
# # END DEMO ----

# @var simulation_option_list
# variables related to the simulation itself: parameters that vary between runs like N, p, runs, ets
# @var dgp_option_list
simulation <-
  function(dgp_option_list = list(error_term_constant = 4,
                                  BETAS_INCREASE_FACTOR = 9),
           simulation_option_list = list(N = 200,
                                         p = 15,
                                         runs = 100,
                                         ncomp = NULL)) {
    start_time <- Sys.time()
    
    set.seed(13456)
   
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
      # strong factors are 1/10 of data set
      get_percentage_of_number(round(p * 0.1), p),
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
        inflate_beta <- FALSE
        
        if(j == 1){
          inflate_beta <- TRUE  
        }
        
        # Draw data ----
        dgp_data <-
          dgp(
            N = N,
            variable_count = p,
            mu = mu,
            case = j,
            inflate_beta = inflate_beta,
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
        
        
        # Use passed number of components or determine the optimal one
        if (is.null(simulation_option_list$ncomp)) {
          PCR_ncomp <- get_optimal_num_of_components(PCR)
        } else {
          PCR_ncomp <- simulation_option_list$ncomp
        }
        
        # TODO: Track ncomp
        PCR_ncomp_tracker[i, j] <- PCR_ncomp
        
        PCR_predict <- predict(PCR, x[test,], ncomp = PCR_ncomp)
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
        
        # par(mfrow=c(1,2))
        # 
        # plot(RMSEP(PCR), legendpos = "topright", main = paste("PCR, Case: ", j))
        # plot(RMSEP(PLSR), legendpos = "topright", main = paste("PLSR, Case: ", j))
        
        # Use passed number of components or determine the optimal one
        if (is.null(simulation_option_list$ncomp)) {
          PLSR_ncomp <- get_optimal_num_of_components(PCR)
        } else {
          PLSR_ncomp <- simulation_option_list$ncomp
        }
        
        # TODO: Track ncomp
        PLSR_ncomp_tracker[i, j] <- PLSR_ncomp
        
        PLSR_predict <- predict(PLSR, x[test,], ncomp = PLSR_ncomp)
        # Save MSE
        MSE_PLSR[i, j] <- get_MSE(PLSR_predict, y[test])
        # ----
      }
    }
    
    end_time <- Sys.time()
    print(paste("Function finished in", end_time - start_time))
    
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
# test_env$dgp_options_default <-
#   list(error_term_constant = 4,
#        BETAS_INCREASE_FACTOR = 2)
# test_env$simulation_options_default <-
#   list(N = 200, p = 100, runs = 20, ncomp = 6)
# 
# test_env$simulation_result0 <-
#   simulation(simulation_option_list = test_env$simulation_options_default)
# test_env$MSE_stats0 <- save_mean_mse_and_return_stats(test_env$simulation_result0)
# 
# 
# plot_MSE_comparison_boxplot(test_env$simulation_result0, test_env$MSE_stats0)
# 
# # DO NOT RUN, DO NOT CLEAR
# # Simulation 1 ----
# # [Simulate more variables]
# test_env$simulation_options_1 <- test_env$simulation_options_default
# test_env$simulation_options_1$runs <- 200
# test_env$simulation_options_1$N <- 192
# test_env$simulation_options_1$p <- 108
# test_env$simulation_options_default$ncomp <- 6
# 
# test_env$simulation_results1 <-
#   simulation(simulation_option_list = test_env$simulation_options_1)
#                             test_env$MSE_stats1 <-
#   save_mean_mse_and_return_stats(test_env$simulation_results1)
# # Get mean components used by each method
# # PCR: 15
# # PLSR: 1
# test_env$ncomp_mean1 <-
#   get_mean_number_of_components_by_method(
#     list(
#       PCR_ncomp = test_env$simulation_results1$PCR_ncomp,
#       PLSR_ncomp = test_env$simulation_results1$PLSR_ncomp
#     )
#   )
# 
# plot_MSE_comparison_boxplot(test_env$simulation_results1, test_env$MSE_stats1)
# 
#  # Simulation 2: Testing 100 runs with 6 components
# test_env$simulation_options_2 <- test_env$simulation_options_1
# test_env$simulation_options_2$ncomp <- 6
# test_env$simulation_options_2$runs <- 100
# 
# 
# test_env$simulation_results2 <-
#   simulation(simulation_option_list = test_env$simulation_options_2)
# 
# test_env$MSE_stats2 <-
#   save_mean_mse_and_return_stats(test_env$simulation_results2)
# 
# test_env$ncomp_mean2 <-
#   get_mean_number_of_components_by_method(
#     list(
#       PCR_ncomp = test_env$simulation_results2$PCR_ncomp,
#       PLSR_ncomp = test_env$simulation_results2$PLSR_ncomp
#     )
#   )
# 
# plot_MSE_comparison_boxplot(test_env$simulation_results2, test_env$MSE_stats2)
# 
# # Simulation 3: Simulation 2 + error_constant decreased to 1
# test_env$dgp_options_3 <- test_env$dgp_options_default
# test_env$dgp_options_3$error_term_constant <- 1
# 
# test_env$simulation_results3 <-
#   simulation(
#     simulation_option_list = test_env$simulation_options_2,
#     dgp_option_list = test_env$dgp_options_3
#   )
# 
# test_env$MSE_stats3 <-
#   save_mean_mse_and_return_stats(test_env$simulation_results3)
# 
# test_env$ncomp_mean3 <-
#   get_mean_number_of_components_by_method(
#     list(
#       PCR_ncomp = test_env$simulation_results3$PCR_ncomp,
#       PLSR_ncomp = test_env$simulation_results3$PLSR_ncomp
#     )
#   )
# 
# plot_MSE_comparison_boxplot(test_env$simulation_results3, test_env$MSE_stats3)
