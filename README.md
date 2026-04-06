# The Diverging Action-Space: Evolving Rhythms and Changing Habits in Daily Mobility

**Author:** Jeppe Rich, Technical University of Denmark  
**Journal:** Nature Communications (under review)

---

## Overview

This repository contains the analysis code accompanying the manuscript. It reproduces all figures and tables from Step 2B onward using pre-computed kernel density estimation (KDE) artifacts derived from the Danish National Travel Survey (2007–2024).

Raw travel survey microdata are restricted and cannot be shared (Statistics Denmark data agreement). The KDE artifacts provided here are sufficient to reproduce all downstream analyses and figures reported in the paper.

A fully executable capsule (identical code + data, no setup required) is also available on CodeOcean:  
[https://codeocean.com/signup/nature?token=1cc9ced764bd4c17b49656707d8d19ee](https://codeocean.com/signup/nature?token=1cc9ced764bd4c17b49656707d8d19ee)

---

## Repository structure

```
code/          R analysis scripts (Steps 2B–6)
data/
  kernels.zip  Pre-computed KDE artifacts for all scenarios (unzip before running)
results/       Generated outputs — created by the pipeline, not tracked in git
Codebase.Rproj RStudio project file
```

---

## Scenarios

| Scenario     | Description                        |
|--------------|------------------------------------|
| `baseline`   | All individuals                    |
| `sex1`       | Females                            |
| `sex2`       | Males                              |
| `city_10000` | Residents of cities ≥ 10,000       |
| `city_25000` | Residents of cities ≥ 25,000       |
| `city_50000` | Residents of cities ≥ 50,000       |
| `city_100000`| Residents of cities ≥ 100,000      |

---

## How to run

### 1. Unzip the kernel data

```bash
cd data
unzip kernels.zip
```

This creates `data/kernels/<scenario>/` with the required `.rds` files.

### 2. (Optional) Validate inputs

```r
Rscript code/00_validate_inputs.R
```

### 3. Run a single scenario

```r
Rscript code/run_one_scenario.R baseline
```

### 4. Run all scenarios

```r
Rscript code/run_all.R
```

Outputs (PNG figures, CSV tables, intermediate `.rds` files) are written to `results/<scenario>/`.

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

---

## Note on output volume

The pipeline generates more figures and tables than are included in the manuscript. This is intentional — additional outputs support robustness checks and exploratory analysis.

---

## Contact

Prof. Jeppe Rich — rich@dtu.dk  
Department of Management Engineering, Technical University of Denmark
