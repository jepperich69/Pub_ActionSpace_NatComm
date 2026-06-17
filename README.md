# The Diverging Action-Space: Evolving Rhythms and Changing Habits in Daily Mobility

**Author:** Jeppe Rich, Technical University of Denmark  
**Journal:** Nature Communications (under review)

---

## Overview

This repository contains the analysis code accompanying the manuscript. It reproduces all figures and tables from Step 2B onward using pre-computed kernel density estimation (KDE) artifacts derived from the Danish National Travel Survey (2007–2024).

Raw travel survey microdata are restricted and cannot be shared (Statistics Denmark data agreement). The KDE artifacts provided here are sufficient to reproduce all downstream analyses and figures reported in the paper, including the urban-restriction robustness check, the bootstrap uncertainty fan, and the bandwidth sensitivity analysis.

A fully executable capsule (identical code + data, no setup required) is also available on CodeOcean:  
[https://codeocean.com/signup/nature?token=1cc9ced764bd4c17b49656707d8d19ee](https://codeocean.com/signup/nature?token=1cc9ced764bd4c17b49656707d8d19ee)

---

## Repository structure

```
code/          R analysis scripts (Steps 2B–9)
data/kernels/  Pre-computed KDE artifacts for all scenarios (committed directly, no unzip step)
results/       Generated outputs (figures, tables, intermediate artifacts)
Codebase.Rproj RStudio project file
```

---

## Scenarios

| Scenario       | Description                                  |
|----------------|-----------------------------------------------|
| `baseline`     | All individuals, H_D = 5 km (reference)        |
| `sex1`         | Females                                        |
| `sex2`         | Males                                          |
| `city_10000`   | Residents of cities ≥ 10,000                   |
| `city_25000`   | Residents of cities ≥ 25,000                   |
| `city_50000`   | Residents of cities ≥ 50,000                   |
| `city_100000`  | Residents of cities ≥ 100,000                  |
| `bw3`          | Baseline population, H_D = 3 km bandwidth      |
| `bw7`          | Baseline population, H_D = 7 km bandwidth      |

`bw3` and `bw7` are used only by `step9_bandwidth_sensitivity.R` to test sensitivity to the KDE distance bandwidth; they are not run through `run_one_scenario.R`.

---

## How to run

### 1. (Optional) Validate inputs

```r
Rscript code/00_validate_inputs.R
```

### 2. Run a single scenario (Steps 2B–6)

```r
Rscript code/run_one_scenario.R baseline
```

### 3. Run all scenarios

```r
Rscript code/run_all.R
```

Outputs (PNG figures, CSV tables, intermediate `.rds` files) are written to `results/<scenario>/`.

### 4. Cross-scenario robustness and sensitivity analyses (Steps 7–9)

These run independently of Steps 2B–6, using the per-scenario outputs/kernels directly:

```r
Rscript code/step7_urban_robustness.R       # SI: settlement-type drift comparison -> results/urban_robustness/
Rscript code/step8_uncertainty.R            # Main Fig. 6a: bootstrap arrow-fan      -> results/uncertainty/
Rscript code/step9_bandwidth_sensitivity.R  # SI: bandwidth sensitivity (bw3/bw7)    -> results/bandwidth_sensitivity/
```

All commands are run from the project root (this directory).

---

## R dependencies

The pipeline uses base R plus the following packages:

```r
install.packages(c("data.table", "dplyr", "MASS", "ggplot2",
                   "tidyr", "purrr", "scales", "viridis"))
```

R version used: 4.3.x

---

## Key scripts

| Script                         | Purpose                                              |
|-------------------------------|------------------------------------------------------|
| `00_validate_inputs.R`        | Check all kernel inputs are present                  |
| `utils_io.R`                  | Shared I/O utilities and scenario labelling          |
| `run_one_scenario.R`          | Run full pipeline for one scenario                   |
| `run_all.R`                   | Run all scenarios in sequence                        |
| `step2_kernel_generation.R`   | KDE generation from microdata (documentation only)   |
| `step2b_visualization.R`      | KDE-based visualizations                             |
| `step2c_visualization_transpose.R` | Alternative KDE difference visualizations       |
| `step3_daytime.R`             | Temporal/daytime mobility analysis                   |
| `step4_integral.R`            | Action-space drift metrics                           |
| `step5_direction.R`           | Drift vector computation and visualization           |
| `step6_final_plots.R`         | Path complexity metrics and final summary figures    |
| `step7_urban_robustness.R`    | Settlement-type drift comparison across city-size scenarios (SI) |
| `step8_uncertainty.R`         | Bootstrap arrow-fan uncertainty plot (main Fig. 6a)   |
| `step9_bandwidth_sensitivity.R` | Drift comparison across bw3/baseline/bw7 KDE bandwidths (SI) |

---

## Note on output volume

The pipeline generates more figures and tables than are included in the manuscript. This is intentional — additional outputs support robustness checks and exploratory analysis.

---

## License

Code is released under the MIT License (see `LICENSE`). The Danish National Travel Survey microdata underlying the KDE artifacts are not covered by this license and remain subject to the Statistics Denmark data agreement.

---

## Contact

Prof. Jeppe Rich — rich@dtu.dk  
Department of Management Engineering, Technical University of Denmark
