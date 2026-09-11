######################################################################
# STEP 2 - SCENARIO RUNNER
#
#   Rscript code/step2_run_scenario.R <scenario>
#
# Regenerates one scenario's kernels and copies the six RDS files into
# data/kernels/<scenario>/, which is what every downstream step reads.
#
# Why this exists
# ---------------
# The sex and city scenarios used to be produced by hand-editing
# step2_kernel_generation.R: uncomment a filter line, change output_dir,
# run, copy six files across, revert. Eight of the eleven kernel sets were
# built that way, so `run_paper.R --with-restricted` could not rebuild them
# and no record survived of which filters had been active for which run.
#
# That is also how the zero-trip sample came to carry
# `HomeAdrCitySize > 100000` while the trip-making sample did not: a city
# run reverted in one block and not the other. Scenario restrictions now
# live in CITY_MIN / SEX_ONLY, are read by both filters, and are set here.
#
# Each scenario must run in its OWN process. step2 guards H_T/H_D with
# `if (!exists(...))`, so sourcing it twice in one session silently reuses
# the first bandwidth and produces two identical "scenarios".
######################################################################

args     <- commandArgs(trailingOnly = TRUE)
scenario <- if (length(args) >= 1) args[[1]] else "baseline"

SPECS <- list(
  baseline    = list(),
  sex1        = list(SEX_ONLY = 1L),
  sex2        = list(SEX_ONLY = 2L),
  city_10000  = list(CITY_MIN = 10000),
  city_25000  = list(CITY_MIN = 25000),
  city_50000  = list(CITY_MIN = 50000),
  city_100000 = list(CITY_MIN = 100000),
  bw3         = list(H_D = 3),
  bw7         = list(H_D = 7),
  ht05        = list(H_T = 0.5),
  ht20        = list(H_T = 2)
)

if (!scenario %in% names(SPECS)) {
  stop("Unknown scenario: ", scenario,
       "\nKnown: ", paste(names(SPECS), collapse = ", "))
}

for (nm in names(SPECS[[scenario]])) assign(nm, SPECS[[scenario]][[nm]])

output_dir <- file.path(tempdir(), paste0("step2_", scenario))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n############ STEP 2 SCENARIO:", scenario, "############\n")
cat("Restrictions: ",
    if (length(SPECS[[scenario]]))
      paste(names(SPECS[[scenario]]), unlist(SPECS[[scenario]]),
            sep = " = ", collapse = ", ")
    else "none (full sample, default bandwidths)", "\n\n")

source("code/step2_kernel_generation.R")

KERNEL_FILES <- c("grid_mv_step2.rds",
                  "kde_baseline_by_cohort.rds",
                  "kde_yearbin_by_cohort.rds",
                  "kde_difference_by_cohort.rds",
                  "temporal_baseline_away.rds",
                  "temporal_yearbin_away.rds")

dest <- file.path("data", "kernels", scenario)
dir.create(dest, recursive = TRUE, showWarnings = FALSE)

src <- file.path(output_dir, KERNEL_FILES)
if (!all(file.exists(src))) {
  stop("step2 did not produce all six kernel files for '", scenario, "': ",
       paste(KERNEL_FILES[!file.exists(src)], collapse = ", "))
}

ok <- file.copy(src, file.path(dest, KERNEL_FILES), overwrite = TRUE)
if (!all(ok)) stop("Failed to copy kernels into ", dest)

cat("\nWrote", length(KERNEL_FILES), "kernel files to", dest, "\n")
