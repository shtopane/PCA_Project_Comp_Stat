run_simulation1 <- function(simulation_option_default, dgp_option_default){
  simulation1 <- new.env()
  
  simulation1$result <-
    simulation(dgp_option_list = dgp_option_default,
               simulation_option_list = simulation_option_default)
  # Compute mean statistics from simulation
  simulation1$MSE_stats <-
    save_mean_mse_and_return_stats(simulation1$result)
  
  save(simulation1, file = "simulation_results_Rdata/simulation1.RData")
}

run_simulation2 <- function(simulation_option_default, dgp_option_default){
  simulation2 <- new.env()
  
  simulation2$sim_option <- simulation_option_default
  simulation2$sim_option$N <- possible_N[2]
  
  simulation2$result <-
    simulation(dgp_option_list = dgp_option_default,
               simulation_option_list = simulation2$sim_option)
  
  simulation2$MSE_stats <-
    save_mean_mse_and_return_stats(simulation2$result)
  
  simulation2$result_cleared <- simulation2$result
  # Remove outlier
  simulation2$result_cleared$MSE_PLSR <- simulation2$result_cleared$MSE_PLSR[-79, ]
  simulation2$result_cleared$MSE_PCR <- simulation2$result_cleared$MSE_PCR[-79, ]
  
  save(simulation2, file = "simulation_results_Rdata/simulation2.RData")
}

run_simulation3 <- function(simulation_option_default, dgp_option_default){
  simulation3 <- new.env()
  
  simulation3$sim_option <- simulation_option_default
  simulation3$sim_option$N <- possible_N[3] # 400
  
  simulation3$result <-
    simulation(dgp_option_list = dgp_option_default,
               simulation_option_list = simulation3$sim_option)
  
  simulation3$MSE_stats <-
    save_mean_mse_and_return_stats(simulation3$result)
  
  save(simulation3, file = "simulation_results_Rdata/simulation3.RData")
}

run_simulation4 <- function(simulation_option_default, dgp_option_default){
  simulation4 <- new.env()
  
  simulation4$sim_option <- simulation_option_default
  simulation4$sim_option$N <- possible_N[2] # 200
  simulation4$sim_option$p <- 100
  
  simulation4$result <-
    simulation(dgp_option_list = dgp_option_default,
               simulation_option_list = simulation4$sim_option)
  
  simulation4$MSE_stats <-
    save_mean_mse_and_return_stats(simulation4$result)
  
  save(simulation4, file = "simulation_results_Rdata/simulation4.RData")
}

run_simulation5 <- function(simulation_option_default, dgp_option_default){
  simulation5 <- new.env()
  
  simulation5$sim_option <- simulation_option_default
  simulation5$sim_option$N <- possible_N[2] # 200
  simulation5$sim_option$p <- 100
  simulation5$sim_option$ncomp <- 6
  
  simulation5$result <-
    simulation(dgp_option_list = dgp_option_default,
               simulation_option_list = simulation5$sim_option)
  
  simulation5$MSE_stats <-
    save_mean_mse_and_return_stats(simulation5$result)
  
  save(simulation5, file = "simulation_results_Rdata/simulation5.RData")
}

run_simulation6 <- function(simulation_option_default, dgp_option_default){
  simulation6 <- new.env()
  
  simulation6$sim_option <- simulation_option_default 
  simulation6$sim_option$N <- 200
  simulation6$sim_option$p <- 100
  simulation6$sim_option$ncomp <- 3
  
  simulation6$result <- simulation(dgp_option_list = dgp_option_default, simulation_option_list = simulation6$sim_option)
  
  simulation6$MSE_stats <- save_mean_mse_and_return_stats(simulation6$result)
  
  save(simulation6, file = "simulation_results_Rdata/simulation6.RData")
}

run_simulation7 <- function(simulation_option_default, dgp_option_default){
  simulation7 <- new.env()
  
  simulation7$sim_option <- simulation_option_default 
  simulation7$sim_option$N <- possible_N[2]
  simulation7$sim_option$p <- 100
  simulation7$sim_option$ncomp <- 1
  
  simulation7$result <- simulation(dgp_option_list = dgp_option_default, simulation_option_list = simulation7$sim_option)
  
  simulation7$MSE_stats <- save_mean_mse_and_return_stats(simulation7$result)
  
  save(simulation7, file = "simulation_results_Rdata/simulation7.RData")
}

run_simulation8 <- function(simulation_option_default, dgp_option_default) {
  simulation8 <- new.env()
  
  simulation8$sim_option <- simulation_option_default
  simulation8$sim_option$N <- 192
  simulation8$sim_option$p <- 108
  simulation8$sim_option$runs <- 1000
  simulation8$sim_option$ncomp <- 6
  
  simulation8$result <-
    simulation(dgp_option_list = dgp_option_default,
               simulation_option_list = simulation8$sim_option)
  
  simulation8$MSE_stats <-
    save_mean_mse_and_return_stats(simulation8$result)
  
  save(simulation8, file = "simulation_results_Rdata/simulation8.RData")
}

run_simulation9 <- function(simulation_option_default, dgp_option_default){
  simulation9 <- new.env()
  
  simulation9$sim_option <- simulation_option_default 
  simulation9$sim_option$N <- 192
  simulation9$sim_option$p <- 108
  
  simulation9$sim_option$ncomp <- 6
  
  simulation9$result <- simulation(dgp_option_list = dgp_option_default, simulation_option_list = simulation9$sim_option)
  
  simulation9$MSE_stats <- save_mean_mse_and_return_stats(simulation9$result)
  
  save(simulation9, file = "simulation_results_Rdata/simulation9.RData")
}

run_simulation10 <- function(simulation_option_default, dgp_option_default){
  simulation10 <- new.env()
  
  simulation10$sim_option <- simulation_option_default 
  simulation10$sim_option$N <- 192
  simulation10$sim_option$p <- 108
  
  simulation10$sim_option$ncomp <- 3
  
  simulation10$result <- simulation(dgp_option_list = dgp_option_default, simulation_option_list = simulation10$sim_option)
  
  simulation10$MSE_stats <- save_mean_mse_and_return_stats(simulation10$result)
  
  save(simulation10, file = "simulation_results_Rdata/simulation10.RData")
  .plot_simulation_result_save_image(simulation10$result, simulation10$MSE_stats, "simulation10")
}

run_simulation11 <- function(simulation_option_default, dgp_option_default){
  simulation11 <- new.env()
  
  simulation11$sim_option <- simulation_option_default 
  simulation11$sim_option$N <- 192
  simulation11$sim_option$p <- 108
  
  simulation11$sim_option$ncomp <- 1
  
  simulation11$result <- simulation(dgp_option_list = dgp_option_default, simulation_option_list = simulation11$sim_option)
  
  simulation11$MSE_stats <- save_mean_mse_and_return_stats(simulation11$result)
  
  save(simulation11, file = "simulation_results_Rdata/simulation11.RData")
}


# Plot results and save the plot as pdf[Not included in the project]
.plot_simulation_result_save_image <- function(result, MSE_stats, file_name){
  file_path <- paste("images/simulation_results/", file_name, ".pdf", sep = "")
  pdf(file = file_path)
  par(mfrow = c(3, 2))
  plot_MSE_comparison_boxplot(result , MSE_stats)
  dev.off()
}