######################################################################
# STEP 2 – TEMPORAL BANDWIDTH SENSITIVITY RUN: H_T = 2 h
#
# Runs step2_kernel_generation.R with a wider temporal bandwidth,
# holding the spatial bandwidth at the baseline H_D = 5 km.
# Manual alternative to code/run_temporal_bandwidth.R, which runs both
# scenarios and stages the kernels for you. Prefer the driver.
#
# Output goes to Paper Results HT20/ in the project root; the 6 RDS files
# then have to be copied to code/data/kernels/ht20/ by hand.
#
# Requires the TU microdata Access database.
#
# Run from code/code/: Rscript step2_run_ht20.R
######################################################################

H_T        <- 2
output_dir <- "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Pub_ActionSpace_NatComm/Paper Results HT20"

source("step2_kernel_generation.R")
