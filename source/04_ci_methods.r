wald_ci <- function(fit, term = "x", level = 0.95){
  alpha <- 1 - level
  z <- qnorm(1 - alpha/2)
  
  beta_hat <- get_beta_hat(fit, term)
  se_hat   <- get_se_hat(fit, term)
  
  list(
    beta = beta_hat,
    se   = se_hat,
    ci   = c(lower = beta_hat - z * se_hat,
             upper = beta_hat + z * se_hat)
  )
}

boot_perc_ci <- function(simulated_data, B = 500, term = "x", level = 0.95){
  n <- nrow(simulated_data)
  alpha <- 1 - level
  
  beta_star <- numeric(B)
  
  for (b in seq_len(B)) {
    idx <- sample.int(n, size = n, replace = TRUE)
    dat_star <- simulated_data[idx, ]
    
    fit_star <- fit_model(dat_star)
    beta_star[b] <- get_beta_hat(fit_star, term)
  }
  
  ci <- quantile(
    beta_star,
    probs = c(alpha/2, 1 - alpha/2),
    na.rm = TRUE,
    names = FALSE
  )
  
  list(
    beta = mean(beta_star, na.rm = TRUE),     
    se   = sd(beta_star, na.rm = TRUE),       
    ci   = ci,
    beta_star = beta_star                      
  )
}

boot_t_ci <- function(simulated_data, B = 500, B_inner = 100,
                      term = "x", level = 0.95) {
  
  n <- nrow(simulated_data)
  alpha <- 1 - level
  
  # original fit (for beta_hat and model-based se_hat)
  fit_hat  <- fit_model(simulated_data)
  beta_hat <- get_beta_hat(fit_hat, term)
  se_hat   <- get_se_hat(fit_hat, term)
  
  t_star  <- numeric(B)
  se_star_vec <- numeric(B)   # NEW: store se_star from each outer bootstrap
  
  for (b in seq_len(B)) {
    
    # outer bootstrap
    idx_b <- sample.int(n, size = n, replace = TRUE)
    dat_star <- simulated_data[idx_b, , drop = FALSE]
    
    fit_star  <- fit_model(dat_star)
    beta_star <- get_beta_hat(fit_star, term)
    
    # inner bootstrap to estimate se_star
    n_star <- nrow(dat_star)
    beta_starstar <- numeric(B_inner)
    
    for (k in seq_len(B_inner)) {
      idx_k <- sample.int(n_star, size = n_star, replace = TRUE)
      dat_starstar <- dat_star[idx_k, , drop = FALSE]
      
      fit_starstar <- fit_model(dat_starstar)
      beta_starstar[k] <- get_beta_hat(fit_starstar, term)
    }
    
    se_star <- stats::sd(beta_starstar, na.rm = TRUE)
    se_star_vec[b] <- se_star
    
    if (is.finite(se_star) && se_star > 0) {
      t_star[b] <- (beta_star - beta_hat) / se_star
    } else {
      t_star[b] <- NA_real_
    }
  }
  
  # keep finite t*
  ok <- is.finite(t_star)
  t_star_ok <- t_star[ok]
  
  q <- stats::quantile(
    t_star_ok,
    probs = c(alpha/2, 1 - alpha/2),
    na.rm = TRUE,
    names = FALSE
  )
  
  ci <- c(
    lower = beta_hat - q[2] * se_hat,
    upper = beta_hat - q[1] * se_hat
  )
  
  list(
    beta = beta_hat,                          
    se   = stats::median(se_star_vec[ok], na.rm = TRUE),  
    ci   = ci,
    se_star = se_star_vec,                    
    t_star  = t_star                          
  )
}