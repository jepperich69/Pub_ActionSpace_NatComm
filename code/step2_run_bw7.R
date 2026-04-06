######################################################################
# STEP 2 – BANDWIDTH SENSITIVITY RUN: H_D = 7 km
#
# Runs step2_kernel_generation.R with a wider distance bandwidth.
# Output goes to Paper Results BW7/ in the project root.
# After running, copy the 6 RDS files to:
#   code/data/kernels/bw7/
#
# Run from project root: Rscript code/code/step2_run_bw7.R
######################################################################

H_D        <- 7
output_dir <- "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Pub_ActionSpace_NatComm/Paper Results BW7"

source("step2_kernel_generation.R")
