# The Diverging Action-Space: Evolving Rhythms and Changing Habits in Daily Mobility

**Author:** Jeppe Rich, Technical University of Denmark  
**Journal:** Nature Communications (under review)

---

## Overview

This repository contains the analysis code accompanying the manuscript. It reproduces all figures and tables from Step 2B onward using pre-computed kernel density estimation (KDE) artifacts derived from the Danish National Travel Survey (2007–2024).

Raw travel survey microdata are personal data and cannot be shared under the GDPR. The session grid shipped here (`grid_mv_step2.rds`) carries a masked surrogate in place of the survey's `SessionId`: the values are a rank-preserving relabelling 1..N within each scenario, they do not key back to the travel survey, and the mapping is not released. Resampling is by session count and sort order, so every reported number is unchanged. The grid ships only the six columns the pipeline reads (`SessionId`, `AgeGroup`, `SessionWeight`, `Year`, `r_rad_km`, `TimeMSM`); exact age, car ownership, urban flag, sex, cumulative distance and the active-window flag are written by the restricted kernel step and are not released. The KDE artifacts provided here are sufficient to reproduce all downstream analyses and figures reported in the paper, including the urban-restriction robustness check, the bootstrap uncertainty fan, and the bandwidth sensitivity analysis.

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
| `sex1`         | Males                                          |
| `sex2`         | Females                                        |
| `city_10000`   | Residents of cities ≥ 10,000                   |
| `city_25000`   | Residents of cities ≥ 25,000                   |
| `city_50000`   | Residents of cities ≥ 50,000                   |
| `city_100000`  | Residents of cities ≥ 100,000                  |
| `bw3`          | Baseline population, H_D = 3 km bandwidth      |
| `bw7`          | Baseline population, H_D = 7 km bandwidth      |
| `ht05`         | Baseline population, H_T = 0.5 h bandwidth     |
| `ht20`         | Baseline population, H_T = 2 h bandwidth       |

`bw3` and `bw7` are read only by `step9_bandwidth_sensitivity.R`, and `ht05` and `ht20` only by `step9b_temporal_bandwidth.R`. The four are not run through `run_one_scenario.R`.

---

## How to run

Everything is driven by one script. Run it from this directory:

```r
Rscript code/run_paper.R
```

It runs every step downstream of the shipped kernels and ends with a manifest
listing each figure and table in the manuscript, whether the file was produced,
which script owns it, and whether it matches the published version by checksum.

```r
Rscript code/run_paper.R --manifest-only   # check the manifest, run nothing
Rscript code/run_paper.R --with-restricted # also rebuild the kernels (needs the microdata)
Rscript code/run_paper.R --record          # stamp the current outputs as the reference
```

To verify rather than regenerate, send the output somewhere else and let the
manifest compare it against the manuscript copies:

```r
ACTIONSPACE_FIG_DIR=scratch/figures ACTIONSPACE_TABLE_DIR=scratch/tables \
  Rscript code/run_paper.R
```

Without that redirection the run overwrites the very files the reference points
at, and every comparison is a file against itself.

Individual steps can still be run on their own:

```r
Rscript code/00_validate_inputs.R           # check the kernel inputs are present
Rscript code/run_one_scenario.R baseline    # steps 2b-6 for one scenario
Rscript code/step7_urban_robustness.R       # SI Fig. S4, settlement-type drift
Rscript code/run_step8.R baseline           # Fig. 6a, bootstrap arrow fan
Rscript code/step9_bandwidth_sensitivity.R  # SI Fig. S6 and Table S6, spatial bandwidth
Rscript code/step9b_temporal_bandwidth.R    # SI Table S7, temporal bandwidth
Rscript code/step11_covid_departure.R       # SI Table S4, pandemic departures
Rscript code/build_si_tables.R              # SI Table S4-S8 bodies
```

---

## R dependencies

The pipeline uses base R plus the following packages:

```r
install.packages(c("data.table", "dplyr", "MASS", "ggplot2",
                   "tidyr", "purrr", "scales", "viridis"))
```

R version used: 4.5.2

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
| `step7_urban_robustness.R`    | Settlement-type drift comparison across city-size scenarios (SI Fig. S4) |
| `run_step8.R` / `step8_uncertainty_R2.R` | Session bootstrap; Fig. 6a and SI Fig. S5. The older `step8_uncertainty.R` is retired |
| `step9_bandwidth_sensitivity.R` | Spatial bandwidth sweep, SI Fig. S6 and Table S6      |
| `step9b_temporal_bandwidth.R` | Temporal bandwidth sweep, SI Table S7                 |
| `step10_collect_figures.R`    | Copy the manuscript figures into the manuscript directory |
| `step11_covid_departure.R`    | Pre-pandemic trend and departures, SI Table S4        |
| `build_si_tables.R`           | Generate SI Tables S4-S8 as LaTeX bodies the manuscript includes |
| `run_paper.R`                 | Run the whole pipeline and check the manifest         |

---

## Note on output volume

The pipeline generates more figures and tables than are included in the manuscript. This is intentional — additional outputs support robustness checks and exploratory analysis.

---

## License

Code is released under the MIT License (see `LICENSE`). The Danish National Travel Survey microdata underlying the KDE artifacts are not covered by this license and cannot be redistributed: they are personal data under the GDPR.

---

## Contact

Prof. Jeppe Rich — rich@dtu.dk  
Department of Management Engineering, Technical University of Denmark
