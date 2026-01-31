out_dir <- here::here("data", "simulation_scenarios")

files <- list.files(out_dir, pattern = "\\.RData$", full.names = TRUE)

all_results <- purrr::map_dfr(files, function(f) {
  load(f)        # loads results_df
  results_df
})

metrics_df <- compute_metrics(all_results)


all_results <- all_results %>%
  mutate(method = factor(method, levels = c("wald", "perc", "bt")))

metrics_df <- metrics_df %>%
  mutate(method = factor(method, levels = c("wald", "perc", "bt")))


## Bias
p_bias <- metrics_df %>%
  ggplot(aes(x = method, y = bias)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  facet_grid(error_distr ~ n) +
  labs(
    title = "Bias of beta_hat across scenarios and methods",
    x = "Method",
    y = "Bias = mean(beta_hat) - beta_true",
    caption = "Each point is the Monte Carlo bias within a (n, error distribution, beta_true) scenario. Dashed line indicates zero bias."
  )

p_bias

## CI
p_cov <- metrics_df %>%
  ggplot(aes(x = method, y = coverage)) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  geom_point(size = 2) +
  facet_grid(error_distr ~ n) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Empirical coverage of nominal 95% confidence intervals",
    x = "Method",
    y = "Coverage probability",
    caption = "Coverage is the proportion of Monte Carlo replicates whose CI contains beta_true. Dashed line indicates nominal 0.95."
  )

p_cov


## distribution

p_se <- all_results %>%
  ggplot(aes(x = method, y = se_hat)) +
  geom_boxplot(outlier.alpha = 0.4) +
  facet_grid(error_distr ~ n + beta_true) +
  labs(
    title = "Distribution of standard error estimates across scenarios and methods",
    x = "Method",
    y = "Estimated SE of beta_hat",
    caption = "Boxplots show Monte Carlo distributions of method-specific SE estimates for each (n, beta_true, error distribution) scenario."
  )

p_se


## time
p_time <- all_results %>%
  ggplot(aes(x = method, y = time_sec)) +
  geom_boxplot(outlier.alpha = 0.4) +
  scale_y_log10() +
  facet_grid(error_distr ~ n + beta_true) +
  labs(
    title = "Computation time per replicate across scenarios and methods",
    x = "Method",
    y = "Elapsed time (seconds, log scale)",
    caption = "Times are per Monte Carlo replicate; bootstrap-t is slower due to nested resampling. Facets correspond to (n, beta_true, error distribution) scenarios."
  )

p_time
