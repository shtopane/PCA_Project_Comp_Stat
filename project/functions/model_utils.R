get_MSE <- function(predicted, actual) {
  return(mean((predicted - actual) ^ 2))
}

get_optimal_num_of_components <- function(PCR_object) {
  PCR_object_RMSEP <- RMSEP(PCR_object)$val[1, ,]
  PCR_object_min_comp <- which.min(PCR_object_RMSEP) - 1
  
  # Prediction with 0 components not allowed
  if (PCR_object_min_comp == 0) {
    PCR_object_min_comp <- 1
  }
  
  return(PCR_object_min_comp)
}

get_percantage_change_MSE <- function(first_mean, second_mean){
  result <- ((first_mean - second_mean)/first_mean) * 100
  return(result)
}

save_mean_mse_and_return_stats <- function(MSE_list) {
  length <- ncol(MSE_list$MSE_PCR)
  MSE_PCR_mean <- rep(0, length)
  MSE_PLSR_mean <- rep(0, length)
  MSE_percentage_change <- rep(0, length)
  
  for (i in 1:length) {
    MSE_PCR_mean[i] <- mean(MSE_list$MSE_PCR[, i])
    MSE_PLSR_mean[i] <- mean(MSE_list$MSE_PLSR[, i])
    # Calculate percentage change between PCR and PLSR
    MSE_percentage_change[i] <-
      ((MSE_PCR_mean[i] - MSE_PLSR_mean[i]) / MSE_PCR_mean[i]) * 100
    # Round to 2 digits
    MSE_percentage_change[i] <- round(MSE_percentage_change[i], digits = 2)
  }
  
  return(
    list(
      "MSE_PCR_mean" = MSE_PCR_mean,
      "MSE_PLSR_mean" = MSE_PLSR_mean,
      "MSE_percentage_change" = MSE_percentage_change
    )
  )
  
}

get_mean_number_of_components_by_method <- function(ncomp_list){
  length <- ncol(ncomp_list$PCR_ncomp)
  mean_ncomp_PCR <- rep(0, length)
  mean_ncomp_PLSR <- rep(0, length)
  
  for(i in 1:length){
    mean_ncomp_PCR <- round(mean(ncomp_list$PCR_ncomp[,i]))
    mean_ncomp_PLSR <- round(mean(ncomp_list$PLSR_ncomp[,i]))
  }
  
  return(list(mean_ncomp_PCR, mean_ncomp_PLSR))
}