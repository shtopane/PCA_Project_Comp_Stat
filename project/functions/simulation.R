# @var simulation_option_list
# variables related to the simulation itself: parameters that vary between runs like N, p, runs, ets
# @var dgp_option_list
simulation <-
  function(dgp_option_list = list(error_term_constant = 4,
                                  beta_factor = 9),
           simulation_option_list = list(N = 200,
                                         p = 15,
                                         runs = 100,
                                         ncomp = NULL,
                                         should_return_data = FALSE)) {
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
    
    if(simulation_option_list$should_return_data == TRUE){
      # Store data draws
      data_container <- list()
    }
    
    
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
            beta_factor = dgp_option_list$beta_factor,
            X_covariance_cluster_proportion = correlated_variables_with_response[j]
          )
        # ----
        
        if(simulation_option_list$should_return_data == TRUE){
          # Store data in order to return it
          data_container[[j]] <- dgp_data
        }
       
        
        # Separate X's from Y's
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
        
        # Track ncomp
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
        
        # Use passed number of components or determine the optimal one
        if (is.null(simulation_option_list$ncomp)) {
          PLSR_ncomp <- get_optimal_num_of_components(PCR)
        } else {
          PLSR_ncomp <- simulation_option_list$ncomp
        }
        
        # Track ncomp
        PLSR_ncomp_tracker[i, j] <- PLSR_ncomp
        
        PLSR_predict <- predict(PLSR, x[test,], ncomp = PLSR_ncomp)
        # Save MSE
        MSE_PLSR[i, j] <- get_MSE(PLSR_predict, y[test])
        # ----
      }
    }
    
    end_time <- Sys.time()
    print(paste("Function finished in", end_time - start_time))
    
    result <-  list(
      "MSE_PCR" = MSE_PCR,
      "MSE_PLSR" = MSE_PLSR,
      "PCR_ncomp" = PCR_ncomp_tracker,
      "PLSR_ncomp" = PLSR_ncomp_tracker
    )
    
    if(simulation_option_list$should_return_data == TRUE){
      result$data <- data_container
    }
    
    return(result)
    
  }
