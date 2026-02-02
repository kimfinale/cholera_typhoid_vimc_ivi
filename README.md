# README: VIMC Cholera and Typhoid Project

**Author:** Jong-Hoon Kim  
**Date:** 2026-02-02

## Overview

This folder contains data and R code used for the Vaccine Impact Modeling Consortium (VIMC) project, focusing on **Cholera** and **Typhoid**. The code is designed to perform calculations, run simulations, and generate outputs (cases, deaths, DALYs, etc.) and plots required for VIMC submissions.

## File Structure and Naming Convention

The R Markdown files (`.Rmd`) in this directory are key to the workflow. Their naming convention is designed to clearly denote the specific simulation run, usually defined by the disease and the VIMC submission cycle/year.

*   **`[disease]_model_runs_[year].Rmd`**: These are the primary simulation scripts.
    *   **Examples**: `cholera_model_runs_2023.Rmd`, `typhoid_model_runs_2023.Rmd`.
    *   **Purpose**: To execute the model simulations for a specific disease and submission round (e.g., 2023, 2025). The code within these files orchestrates the parameter setup, cohort generation, and the main simulation loop.
*   **`[disease]_final.Rmd`**: These files likely represent finalized runs for previous analyses or specific consolidated reports.
*   **`*_input_*.Rmd`**: These files (e.g., `cholera_model_inputs_2023.Rmd`) are generally used for pre-processing and preparing the input data before the main model runs.

## Simulation Details

### 1. Standardized Inputs from VIMC
The simulations rely on standardized inputs provided by VIMC.
*   **Demography**: The code utilizes VIMC-provided population and demographic data (e.g., `population_data`, `life_expectancy_data`).
*   **Vaccination Coverage**: Scenario-specific coverage inputs (e.g., No Vaccination, Campaign, Routine) are loaded from VIMC-provided files (often stored in `data` or `inst/extdata`).

### 2. Indirect Effects
The models account for the indirect effects of vaccination (herd immunity).
*   **Cholera**: The script `cholera_model_runs_2023.Rmd` explicitly sets `vacc_ind_effect <- TRUE` (Line 88) and passes this argument to the `simulate_cholera` function, ensuring indirect, population-level protection is included in the transmission dynamics.
*   **Typhoid**: The Typhoid modeling framework also incorporates calculations for indirect vaccine protection, as referenced by functions like `calc_indirect_vacc_protected` found within the code workflow.

## How to Run

To reproduce a specific set of results:
1.  Open the relevant `Rmd` file (e.g., `cholera_model_runs_2023.Rmd`).
2.  Ensure all dependencies are loaded (the scripts typically use `devtools::load_all()` to load the package functions).
3.  Run the chunks sequentially to:
    *   Generate or load parameter sets.
    *   Define population cohorts.
    *   Execute the `simulate_[disease]` function.
    *   Aggregate and save the outputs to the `outputs/` folder.
