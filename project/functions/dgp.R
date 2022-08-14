# Variables needed:
# yt
# xt
# Lambda factor loadings
# ft(?) factors
# et = sqrt(c*N*et), # c <- (1 + r)*c_tilde
# betas - randomly determined

# Case I
# lambda i,j - iid N(0, 1)

# Case II
# lambda is either:
# 1. ~ N(0, 1) for 1 to N1, where N1 = N^k1, k1 = (0.25 075)(so for a fraction of the dataset)
# 2. 0 for observations N1 + 1,...,N

# Case III
# lambda = lambda_tilde/N^k2
# with: lambda_tilde ~ N(0,1) for i = 1,...,N, k2 = (0.25 0.75)
library("MASS")
dgp_env <- new.env()

# set.seed(12345)
# X0 <- MASS::mvrnorm(n=10, mu = c(0,0,0), Sigma = diag(3))

local({
  k1 <- c(0.25, 0.75)
  k2 <- c(0.25, 0.75)
  r <- c(1, 6)
  c_tilde <- c(1, 4, 9)
  times <- c(20, 30, 50, 100, 200, 400)
}, env = dgp_env)

dgp1 <- function(N = 500, r) {
  betas <- rnorm # vector (in paper is lambda)
  eps_normal <- rnorm(N) # vector
  u_t <- rnorm(N) # vector
  
  # We have to construct the lambdas now
  construct_lambda_by_case <- function(case_num = 1, r){
    case1_generation <- function(){
      lambdas <- mvrnorm(n=N, mu = c(0), Sigma = diag(1))
      return(lambdas)
    }
    
    return(switch(case_num,
           case1_generation()))
  }
  
  # Create matrix for lambdas
  lambdas <- construct_lambda_by_case(r = 1)
  
  # Generate error term for each c
  # TODO: Extract as funciton
  eps <- lapply(dgp_env$c_tilde, function(c_tilde) {
    c <- (1 + r) * c_tilde
    
    
    return(sqrt(c * N) * eps_normal)
  })

  return(lambdas)
}

eps <- dgp1(N = 500, r = dgp_env$r[1])

plot(1:500, eps)
dgp2 <- function(N = 500) {
  
}

dgp3 <- function(N = 500) {
  
}