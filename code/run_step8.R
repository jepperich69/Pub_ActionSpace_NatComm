# Thin wrapper so run_paper.R can invoke step 8 for one scenario in a clean
# session. Figure 6a and both Supplementary Figure S5 panels come from the same
# script this way, which is what keeps their template and baseline identical.
args <- commandArgs(trailingOnly = TRUE)
scenario <- if (length(args) >= 1) args[[1]] else "baseline"
source("code/step8_uncertainty_R2.R")
