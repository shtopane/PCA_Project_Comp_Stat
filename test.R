# library("faux")
# 
# env <- new.env()
# 
# # TEST
# env$N <- 192
# env$p <- 108
# env$mu <- rnorm(env$p)
# env$sd <- sample.int(3, env$p, replace = TRUE) # rep(1, env$p)
# # correlation coefficients
# env$r <-  c(rep(0.8, 5), rep(0.1, 10))  # seq(from = 0.01, to = 0.89, len =env$p)
# 
# env$dat <- rnorm_multi(n = env$N, 
#                    mu = env$mu,
#                    sd = env$sd,
#                    r = env$correlations, 
#                    empirical = TRUE)
# corrplot::corrplot(cor(env$dat))
# 
# env$rho <- 0.84
# env$correlations <- exp(log(env$rho) * abs(matrix(rep(seq(1, env$p), env$p), env$p, env$p)
#                                    - t(matrix(rep(seq(1, env$p), env$p), env$p, env$p))))
