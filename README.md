# VIMC Cholera and Typhoid Project

**Author:** Jong-Hoon Kim  
**Date:** 2026-02-02

## Overview

This folder contains data and R code used for the Vaccine Impact Modeling Consortium (VIMC) project, focusing on **Cholera** and **Typhoid**. The code is designed to perform calculations, run simulations, and generate outputs (cases, deaths, DALYs, etc.) and plots required for VIMC submissions.

## File Structure and Naming Convention

The R Markdown files (`.Rmd`) in this directory are key to the workflow. Their naming convention is designed to clearly denote the specific simulation run, usually defined by the disease and the VIMC submission cycle/year.

*   **`[disease]_model_runs_[year].Rmd`**: These are the primary simulation scripts.
    *   **Examples**: `cholera_model_runs_2023.Rmd`, `typhoid_model_runs_2023.Rmd`.
    *   **Purpose**: To execute the model simulations for a specific disease and submission round (e.g., 2023, 2025). The code within these files orchestrates the parameter setup, cohort generation, and the main simulation loop.
*   **`[disease]_final.Rmd`**: These files represent finalized runs for 2025 analyses.
*   **`*_input_*.Rmd`**: These files (e.g., `cholera_model_inputs_2023.Rmd`) are generally used for pre-processing and preparing the input data before the main model runs.

### Additional Key Files

Beyond the main model run scripts, the following Rmd files play important roles in the project workflow:

#### Data Preparation and Inputs
*   **`parameter_set_prep.Rmd`**: Helper script to prepare and format parameter sets for stochastic predictions.
*   **`mean_age_infection.Rmd`**: Analysis script to review literatures and estimate the mean age of infection.

#### Advanced/Full Model Runs
*   **`[disease]_full_model_runs.Rmd`**: Scripts that contain more extensive or aggregated model executions, potentially covering multiple scenarios or years in a single batch.
*   **`typhoid_practice_runs.Rmd`**: A playground or testing script for experimenting with Typhoid model setups before full execution.

## Simulation Details

Based on an analysis of the files (specifically `cholera_model_runs_2023.Rmd` and `typhoid_model_runs_2023.Rmd`), the following details regarding the modeling approach were extracted:

### 1. Standardized Inputs from VIMC
The simulations rely on standardized inputs provided by VIMC.
*   **Demography**: The code utilizes VIMC-provided population and demographic data (e.g., `population_data`, `life_expectancy_data`).
*   **Vaccination Coverage**: Scenario-specific, VIMC-provided coverage inputs (e.g., No Vaccination, Campaign, Routine) are loaded from VIMC-provided files (often stored in `data` or `inst/extdata`).

### 2. Indirect Effects
The models account for the indirect effects of vaccination (herd immunity).
*   **Cholera**: The script `cholera_model_runs_2023.Rmd` explicitly sets `vacc_ind_effect <- TRUE` (Line 88) and passes this argument to the `simulate_cholera` function, ensuring indirect, population-level protection is included in the transmission dynamics.
*   **Typhoid**: The Typhoid modeling framework also incorporates calculations for indirect vaccine protection, as referenced by functions like `calc_indirect_vacc_protected` found within the code workflow.

## How to Run

To run the simulations, follow the workflow below. This example demonstrates how to run a Cholera simulation. The Typhoid simulation follows a similar pattern using `simulate_tf`.

```r
# 1. Load the package and data
devtools::load_all()

# 2. Define simulation settings
dis <- "Cholera"
nruns <- 200 # Number of stochastic runs
target_countries <- target_countries_cholera # or specify a vector of ISO3 codes
vacc_scenarios <- c("novacc", "campaign")

# 3. Create Parameters List
# Generate stochastic parameters for each country
params_list <- list()
for (k in seq_along(target_countries)) {
  cntry <- target_countries[[k]]
  params_list[[k]] <- set_params_cholera(
    disease = dis,
    nruns = nruns,
    country = cntry,
    parameter_data = parameter_data_cholera,
    incidence_rate_data = overall_incid_rate_cholera,
    case_fatality_ratio_data = cfr_data_cholera
  )
}
names(params_list) <- target_countries

# 4. Create Cohort List
# Generate population cohorts for each country
cohort_list <- lapply(target_countries, function(z) {
  setup_cohorts(country = z, year = "2000:2100", population_data = population_data)
})
names(cohort_list) <- target_countries

# 5. Run Simulation
# Iterate over vaccination scenarios
for (vacc_scenario in vacc_scenarios) {
  message(paste("Running scenario:", vacc_scenario))
  
  # Select appropriate coverage data (logic may vary based on scenario)
  # Here we assume a default list exists or is loaded
  vacc_cov_data <- NULL 
  if (vacc_scenario == "campaign") {
    vacc_cov_data <- vacc_cov_input_cholera[[1]] # Example selection
  }

  sim <- simulate_cholera(
    disease = dis,
    year = "2000:2100",
    country = target_countries,
    params_list = params_list,
    vacc_scenario = vacc_scenario,
    vacc_cov_data = vacc_cov_data,
    cohort = cohort_list,
    life_expectancy_data = life_expectancy_data,
    wash_prop = wash_prop,
    wash_risk_ratio = wash_risk_ratio_cholera,
    vacc_indirect_effect = TRUE,
    run_ids = 1:nruns
  )

  # 6. Save Outputs
  ttimestamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
  saveRDS(sim$stoch, paste0("outputs/stoch_output_list_", ttimestamp, "_", dis, "_", vacc_scenario, ".rds"))
  saveRDS(sim$central, paste0("outputs/central_output_list_", ttimestamp, "_", dis, "_", vacc_scenario, ".rds"))
}
```

### Key Functions
*   `set_params_[disease]`: Generates stochastic parameters.
*   `setup_cohorts`: specifices the population structure over time.
*   `simulate_[disease]`: The main wrapper that executes `vaccine_impact` functions across countries and run IDs.
