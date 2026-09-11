# code/run_one_scenario.R
source("code/utils_io.R")

args <- commandArgs(trailingOnly = TRUE)
scenario <- if (length(args) >= 1) args[[1]] else "baseline"

scenario_label <- get_scenario_label(scenario)

dirs <- get_dirs(scenario)
in_dir  <- dirs$in_dir
out_dir <- dirs$out_dir

cat("Scenario:", scenario, "\n")
cat("Input:   ", in_dir, "\n")
cat("Output:  ", out_dir, "\n\n")

# Run Steps (we'll wire these up one by one)
source("code/step2b_visualization.R")
source("code/step2c_visualization_transpose.R")
source("code/step3_daytime.R")
source("code/step4_integral.R")
source("code/step5_direction.R")
source("code/step6_final_plots.R")

