get_simdata <- function(n, beta_treat, error_distr = c("normal", "t3")){
  # set parameters
  beta0 = 1
  p_treat = 0.5
  sigma2 = 2
  v = 3
  
  x <- rbinom(n, size = 1, prob = p_treat)

  epsilon <- switch(
    error_distr,
    normal = rnorm(n, mean = 0, sd = sqrt(sigma2)),
    t3 = {
      u  <- rt(n, df = v)                 
      u * sqrt(sigma2 * (v - 2) / v)
    }
  )
  

  y <- beta0 + beta_treat * x + epsilon
  
  tibble::tibble(
    y = y,
    x = x
  )
}