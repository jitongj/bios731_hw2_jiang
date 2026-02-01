library(tidyverse)
library(broom)
library(future)
library(furrr)

set.seed(2026)

plan(multisession) 

# **********************
# 1. Load functions ----
# **********************
source(here::here("source", "01_simulate_data.R"))
source(here::here("source", "02_fit_model.R"))
source(here::here("source", "03_extract_estimates.R"))
source(here::here("source", "04_ci_methods.R"))

# **********************
# 2. Set simulation design ----
# **********************
nsim <- 475

n_vec <- c(10, 50, 500)
beta_true_vec <- c(0, 0.5, 2)
error_distr_vec <- c("normal", "t3")

B <- 200
B_inner <- 25


params_grid <- tidyr::expand_grid(
  n = n_vec,
  beta_true = beta_true_vec,
  error_distr = error_distr_vec
) %>%
  dplyr::mutate(
    scenario_id = paste0("sim_n", n, "_beta", beta_true, "_", error_distr)
  )

# **********************
# 3. One replication ----
# **********************
get_ci_vec <- function(ci_obj) {
  # Accept either a numeric vector c(lower=..., upper=...)
  # or a list with element $ci that is such a vector
  if (is.list(ci_obj) && !is.null(ci_obj$ci)) ci_obj <- ci_obj$ci
  as.numeric(ci_obj)[1:2]
}

run_one_rep <- function(params_row, seed,
                        B = B, B_inner = B_inner,
                        level = 0.95, term = "x") {
  
  set.seed(seed)
  
  simdata <- get_simdata(
    n = params_row$n,
    beta_treat = params_row$beta_true,
    error_distr = params_row$error_distr
  )
  
  fit_hat  <- fit_model(simdata)
  beta_hat <- get_beta_hat(fit_hat, term)
  se_hat   <- get_se_hat(fit_hat, term)
  
  # Wald
  t_wald <- system.time({
    wald_ci_obj <- wald_ci(fit_hat, term = term, level = level)
  })["elapsed"]
  wald_ci_vec <- get_ci_vec(wald_ci_obj)
  
  # Percentile
  t_perc <- system.time({
    perc_out <- boot_perc_ci(simdata, B = B, term = term, level = level)
  })["elapsed"]
  # perc_out is expected to be list(beta, se, ci) in your new setup
  perc_ci_vec <- get_ci_vec(perc_out)
  
  # Bootstrap-t
  t_bt <- system.time({
    bt_out <- boot_t_ci(simdata, B = B, B_inner = B_inner, term = term, level = level)
  })["elapsed"]
  bt_ci_vec <- get_ci_vec(bt_out)
  
  true_beta <- params_row$beta_true
  
  dplyr::bind_rows(
    tibble::tibble(
      method = "wald",
      beta_hat = beta_hat,
      se_hat   = se_hat,
      ci_l     = wald_ci_vec[1],
      ci_u     = wald_ci_vec[2],
      time_sec = as.numeric(t_wald)
    ),
    tibble::tibble(
      method = "perc",
      beta_hat = perc_out$beta,
      se_hat   = perc_out$se,
      ci_l     = perc_ci_vec[1],
      ci_u     = perc_ci_vec[2],
      time_sec = as.numeric(t_perc)
    ),
    tibble::tibble(
      method = "bt",
      beta_hat = bt_out$beta,
      se_hat   = bt_out$se,
      ci_l     = bt_ci_vec[1],
      ci_u     = bt_ci_vec[2],
      time_sec = as.numeric(t_bt)
    )
  ) %>%
    dplyr::mutate(
      true_beta = true_beta,
      cover = (ci_l <= true_beta & true_beta <= ci_u)
    )
}

# **********************
# 4. One scenario ----
# **********************
run_one_scenario <- function(params_row,
                             nsim = nsim,
                             B = B, B_inner = B_inner,
                             level = 0.95, term = "x") {
  
  seed_vec <- sample.int(1000000000L, size = nsim, replace = FALSE)
  
  # res <- purrr::map_dfr(
  #   seq_len(nsim),
  #   ~ run_one_rep(params_row, seed = seed_vec[.x],
  #                 B = B, B_inner = B_inner,
  #                 level = level, term = term)
  # )
  
  res <- furrr::future_map_dfr(
    seq_len(nsim),
    ~ run_one_rep(params_row,
                  seed = seed_vec[.x],
                  B = B, B_inner = B_inner,
                  level = level, term = term),
    .options = furrr::furrr_options(seed = TRUE)
  )
  
  res %>%
    mutate(
      n = params_row$n,
      beta_true = params_row$beta_true,
      error_distr = params_row$error_distr,
      scenario = params_row$scenario_id
    )
}

# **********************
# 5. Apply: run all scenarios + save ----
# **********************
out_dir <- here::here("data", "simulation_scenarios")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

purrr::pwalk(
  params_grid,
  function(n, beta_true, error_distr, scenario_id) {
    
    message("\n==============================")
    message("Running scenario: ")
    message("n = ", n, ", beta_true = ", beta_true, ", error_distr = ", error_distr)
    message("==============================")
    
    params_row <- list(
      n = n,
      beta_true = beta_true,
      error_distr = error_distr,
      scenario_id = scenario_id
    )
    
    results_df <- run_one_scenario(
      params_row,
      nsim = nsim,
      B = B, B_inner = B_inner
    )
    
    save(results_df,
         file = file.path(out_dir, paste0(scenario_id, ".RData")))
  }
)


# **********************
# 6. Combine and saveall data ----
# **********************
files <- list.files(out_dir, pattern = "\\.RData$", full.names = TRUE)

all_results <- purrr::map_dfr(files, function(f) {
  load(f)        
  results_df
})

save(all_results, file = here::here("data", "all_results.RData"))


