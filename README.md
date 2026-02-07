# BIOS 731 HW2: Simulation study (bootstrap CIs)

This repository implements a full-factorial simulation study for multiple linear regression and 95% confidence intervals using:
1) Wald CI  
2) Nonparametric bootstrap percentile CI  
3) Nonparametric bootstrap t CI  

It is organized so you can regenerate all results end-to-end by running a single script.

## Project structure

- `731_HW2.Rproj`  
  R Project file (open this first so relative paths work)

- `source/` (core functions)
  - `01_simulate_data.R`  
    Function to generate one simulated dataset for a given scenario (n, beta, error type).
  - `02_fit_model.R`  
    Model-fitting function (linear regression via `lm`).
  - `03_extract_estimates.R`  
    Functions to extract `beta_hat` and `se_hat` from fitted models.
  - `04_ci_methods.R`  
    CI construction methods:
      - Wald CI (model-based)
      - Bootstrap percentile CI
      - Bootstrap t CI (outer + inner bootstrap)
  - `05_compute_metric.R`  
    Performance metrics across Monte Carlo replicates:
      - bias of `beta_hat`
      - distribution of `se_hat`
      - coverage rate
      - MCSE for coverage
      - computation time

- `simulations/`
  - `run_simulation.R`  
    Master script that runs all scenarios, generates/saves scenario-level simulation outputs,
    and creates the combined results object.

- `data/` (generated outputs)
  - `simulation_scenarios/`  
    Saved results for each scenario (intermediate files).  
    Scenario naming format: `sim_nXX_betaXX_errorTYPE` (e.g., `sim_n50_beta0.5_t3`).
  - `all_results.RData`  
    Combined results across all scenarios and methods (produced by `run_simulation.R`).

- `analysis/`
  - `final_report.Rmd`  
    Loads `data/all_results.RData` and creates tables/plots + written answers.
  - `final_report.pdf`  
    Knitted output.

## How to reproduce (run order)

### Step 0: Open the project
1. Clone the repo
2. Open `731_HW2.Rproj`

### Step 1: Run the full simulation study
From the project root, run:

```r
source("simulations/run_simulation.R")
```

This script will:
	•	define the full factorial design (n, beta, error type)
	•	for each scenario:
	•	simulate data using functions in source/01_simulate_data.R
	•	fit the model using source/02_fit_model.R
	•	extract estimates using source/03_extract_estimates.R
	•	compute CIs using source/04_ci_methods.R
	•	compute metrics using source/05_compute_metric.R
	•	save per-scenario outputs into data/simulation_scenarios/
	•	save the combined results object as data/all_results.RData

## Step 2: Render the report

Knit:
	•	analysis/final_report.Rmd → analysis/final_report.pdf

The report reads data/all_results.RData and produces:
	•	Bias summary across scenarios/methods
	•	Coverage summary across scenarios/methods
	•	Distribution summaries for se_hat
	•	Computation time comparisons

Key simulation settings
	•	Full factorial factors:
	•	Sample size: n ∈ {10, 50, 500}
	•	True treatment effect: beta ∈ {0, 0.5, 2}
	•	Error distribution: Normal vs heavy-tailed (t with df = 3, scaled)
	•	Bootstrap parameters:
	•	Outer bootstrap resamples: B
	•	Inner bootstrap resamples: B_inner
	•	Confidence level: 95%

Notes on outputs
	•	Intermediate per-scenario files are saved in data/simulation_scenarios/
	•	The combined object data/all_results.RData is the single input for analysis/final_report.Rmd
