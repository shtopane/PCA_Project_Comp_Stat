test_env <- new.env()
library("MASS")

set.seed(199)

test_env$N <- 192
# Number of Variables
test_env$p <- 5
test_env$mu <- rnorm(test_env$p)

# test_env$cov_seq <- seq(from = 0.01, to = 1.00, len = test_env$p)
# test_env$variances <- rep(1, test_env$p) # runif(test_env$p, min = 5, max = 7)
# test_env$cc <- rep(0, (test_env$p * test_env$p))
#
# test_env$l <- 1
#
# for(i in 2:99){
#   if(test_env$l >= test_env$p){
#     test_env$l <- 1
#   }
#
#   test_env$cc[i] <- test_env$variances[test_env$l]
#   test_env$cc[i + 1] <- test_env$cov_seq[test_env$l]
#
#   test_env$l <- test_env$l + 1
# }
#
# test_env$cc[1] <- test_env$variances[1]
round(cov(test_env$subset), digits = 3)
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
test_env$betas <- c(0.1, -3.1, 0.2, -0.9, 0.04)
test_env$y <- test_env$X %*% test_env$betas + test_env$eps
test_env$data <- data.frame("Y" = test_env$y,
                            "X" = test_env$X)

test_env$pcr <-
  pcr(Y ~ .,
      data = test_env$data[1:(nrow(test_env$data) / 3) , ],
      scale = TRUE,
      validation = "CV")
summary(test_env$pcr)
biplot(test_env$pcr, which = "loadings")
plot(RMSEP(test_env$pcr), legendpos = "topright")


# test_env$sigma1 <- diag(test_env$sd) %*% diag(test_env$sd)
# test_env$series <-
#   mvrnorm(test_env$N, test_env$mu, Sigma = test_env$sigma1)
#
# test_env$series_ts <-
#   ts(test_env$series,
#      start = c(1959, 12),
#      frequency = 12)
# test_env$series_for_transformation <- NULL
# test_env$series_for_transformation$rawdata <- test_env$series_ts
#
# test_env$series_ts_transformed <-
#   get_transformed_time_series(ds = arg, enddate = c(1960, 8))
#
# tscode_weights <- rep(1, 6)
#
# for (i in 1:6) {
#   tscode_weights[i] <- length(filter_by_tscode(i)) / test_env$p
# }
#
# filter_by_tscode <- function(tscode) {
#   tscode_vec <- WORKING_DATA$tcodes[WORKING_DATA$tcodes == tscode]
#   return(tscode_vec)
# }



# test_env$AR_seq <- seq(from = -0.99, to = 0.99, by = 0.1)
# test_env$AR_vec <- matrix(ncol = length(test_env$AR_seq), nrow = 50)
#
# for(i in 1:length(test_env$AR_seq)){
#   test_env$AR_vec[, i] <- arima.sim(model = list(ar = test_env$AR_seq[i]), n = 50)
#   ts.plot(test_env$AR_vec[, i], ylab = paste("Variable ", i), main = paste("AR parameter: ", test_env$AR_seq[i]))
# }
