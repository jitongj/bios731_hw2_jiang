compute_metrics <- function(results_df){
  
  results_df %>%
    dplyr::group_by(scenario, n, beta_true, error_distr, method) %>%
    dplyr::summarise(
      # true parameter (same within group)
      true_beta = dplyr::first(true_beta),
      
      # bias of beta_hat (method-specific because beta_hat is method-specific in your run_one_rep)
      mean_beta = mean(beta_hat, na.rm = TRUE),
      bias = mean_beta - true_beta,
      
      # distribution of se_hat: summary stats
      se_mean = mean(se_hat, na.rm = TRUE),
      se_sd   = sd(se_hat, na.rm = TRUE),
      se_q025 = unname(stats::quantile(se_hat, 0.025, na.rm = TRUE)),
      se_q50  = unname(stats::quantile(se_hat, 0.50,  na.rm = TRUE)),
      se_q975 = unname(stats::quantile(se_hat, 0.975, na.rm = TRUE)),
      
      # coverage for each method
      coverage = mean(cover, na.rm = TRUE),
      
      # MCSE for coverage estimate
      nrep = dplyr::n(),
      mcse = sqrt(coverage * (1 - coverage) / nrep),
      
      # computation time summary
      time_mean_sec = mean(time_sec, na.rm = TRUE),
      time_sd_sec   = sd(time_sec,   na.rm = TRUE),
      time_q50_sec  = unname(stats::quantile(time_sec, 0.50,  na.rm = TRUE)),
      time_q975_sec = unname(stats::quantile(time_sec, 0.975, na.rm = TRUE)),
      
      .groups = "drop"
    )
}
