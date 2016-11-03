set.seed(437298)
n <- 10000
burn_in <- 1000
n_draws <- 10000 + burn_in
b_true <- matrix(c(-6, 3, 3, 3), 4, 1)
X <- cbind(rep(1, n), matrix(rnorm(3 * n), n, 3))
u <- X[, 2]^2 * rnorm(n) # The first column of X is the constant
Ystar <- X %*% b_true + u
Y <- pmax(0, Ystar)

L <- function(theta) {
  mean(abs(Y - pmax(0, X %*% theta)))
}

theta_lower <- -10
theta_upper <- 10

# Vector of tuning parameters for random walk proposals
sigma_sq <- rep(4, length(b_true))

theta <- matrix(NA_real_, n_draws, length(b_true))

starting_value <- lm.fit(X, Y)$coefficients
current_value <- starting_value
accept_count <- 0
proposal_count <- 0

while(accept_count < n_draws){

  proposal <- current_value + rnorm(4, sd = sqrt(sigma_sq))
  proposal_count <- proposal_count + 1

  if(all((proposal > theta_lower) & (proposal < theta_upper))){
   rho <- min(exp(L(proposal) - L(current_value)), 1)
   U <- runif(1)
   if(U < rho){
     current_value <- proposal
     accept_count <- accept_count + 1
     theta[accept_count,] <- current_value
   }
  }

}

accept_count / proposal_count
draws <- theta[-(1:burn_in),]
