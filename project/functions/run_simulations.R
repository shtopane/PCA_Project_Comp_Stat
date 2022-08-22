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
  
  save(simulation8, file = "simulation_results_Rdata/simulation8.RData")
  
  simulation8$MSE_stats <-
    save_mean_mse_and_return_stats(simulation8$result)
  
  pdf(file = "images/simulation_results/simulation8.pdf")
  par(mfrow = c(3, 2))
  plot_MSE_comparison_boxplot(simulation8$result , simulation8$MSE_stats)
  dev.off()
}