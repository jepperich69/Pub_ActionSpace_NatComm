CODE OCEAN CAPSULE README
=======================

Title:
The Diverging Action-Space: Evolving Rhythms and Changing Habits in Daily Mobility

This capsule reproduces all analyses and figures from Step 2B through Step 9 of the manuscript,
including the main-paper bootstrap uncertainty fan (Step 8) and the SI robustness/sensitivity
checks (Steps 7 and 9). Restricted raw microdata used in Step 2 are not shared. Instead, this
capsule provides derived kernel density estimation (KDE) artifacts that are sufficient to
reproduce all downstream results.

IMPORTANT NOTE FOR REVIEWERS
----------------------------
The pipeline generates a substantially larger number of figures and tables than those ultimately
included in the main paper and Supplementary Information. This is intentional and provided as a
service to reviewers and readers who wish to explore robustness checks, alternative visualizations,
and intermediate results.

Only a subset of outputs are referenced directly in the manuscript.

STRUCTURE
---------
data/kernels/<scenario>/   : Input KDE artifacts (RDS files)
code/                      : Analysis scripts
results/<scenario>/        : Generated figures, tables, and intermediate artifacts

SCENARIOS
---------
baseline      : All individuals, H_D = 5 km (reference)
sex1          : Females
sex2          : Males
city_10000    : Cities >= 10,000 inhabitants
city_25000    : Cities >= 25,000 inhabitants
city_50000    : Cities >= 50,000 inhabitants
city_100000   : Cities >= 100,000 inhabitants
bw3           : Baseline population, H_D = 3 km bandwidth (Step 9 only)
bw7           : Baseline population, H_D = 7 km bandwidth (Step 9 only)

KEY SCRIPTS
-----------
00_validate_inputs.R
  Verifies that all required kernel files exist for each scenario.

utils_io.R
  Central utilities for folder-agnostic I/O and scenario labeling.

run_one_scenario.R
  Runs the full pipeline (Step 2B–Step 6) for a single scenario.

run_all.R
  Runs the full pipeline for all scenarios in sequence.

step1_discriptive_stat.R
  Descriptive statistics (not executed in this capsule if raw data are required).

step2_kernel_generation.R
  Kernel generation from restricted microdata (included for documentation only).

step2b_visualization.R
  KDE-based visualizations using shared kernel artifacts.

step2c_visualization_transpose.R
  Alternative/transposed KDE difference visualizations.

step3_daytime.R
  Temporal/daytime mobility analysis.

step4_integral.R
  Integral/action-space metrics computation.

step5_direction.R
  Drift vector computation and visualization.

step6_final_plots.R
  Path complexity metrics and final summary figures.

step7_urban_robustness.R
  Settlement-type drift comparison across city-size scenarios (SI). Run standalone
  from the project root: Rscript code/step7_urban_robustness.R

step8_uncertainty.R
  Bootstrap arrow-fan uncertainty plot (main paper Fig. 6a). Run standalone
  from the project root: Rscript code/step8_uncertainty.R

step9_bandwidth_sensitivity.R
  Drift comparison across bw3/baseline/bw7 KDE distance bandwidths (SI). Run standalone
  from the project root: Rscript code/step9_bandwidth_sensitivity.R

REPRODUCIBILITY
---------------
All paths are relative and folder-agnostic. The pipeline is scenario-separated and deterministic.
Intermediate artifacts are written to disk to ensure modular reproducibility.

Corresponding author: Prof. Rich
