run_simulation8 <- function(simulation_option_default, dgp_option_default){
  sim_option <- simulation_option_default 
  sim_option$N <- 192
  sim_option$p <- 108
  sim_option$runs <- 1000
  sim_option$ncomp <- 6
  
  # result <- simulation(dgp_option_list = dgp_option_default, simulation_option_list = sim_option)
  
  save(simulation8, file = "simulation_results_Rdata/simulation8.RData")
  
  MSE_stats <- save_mean_mse_and_return_stats(result)
  
  pdf(file = "images/simulation_results/simulation8.pdf")
  par(mfrow = c(3, 2))
  plot_MSE_comparison_boxplot(result , MSE_stats)
  dev.off()
}