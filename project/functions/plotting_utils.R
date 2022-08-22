source("functions/constants.R")

plot_MSE_comparison_boxplot <- function(MSE_list, MSE_stats_list){
  length <- ncol(MSE_list$MSE_PCR)
  
  for(i in 1:length){
    boxplot(
      MSE_list$MSE_PCR[, i],
      MSE_list$MSE_PLSR[, i],
      names = c("PCR", "PLSR"),
      main = paste("MSE for PCR and PLS", "\n", simulation_factor_loadings_description[i]),
      cex.main=0.9,
      sub = paste(
        "(Change in % MSE b/n PCR and PLSR): ",
        MSE_stats_list$MSE_percentage_change[i],
        "%"
      ),
      ylab = "MSE",
      col = "lightblue",
      border = "black"
    )
  }
  
}