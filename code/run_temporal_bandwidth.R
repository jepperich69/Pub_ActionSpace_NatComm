######################################################################
# TEMPORAL BANDWIDTH SENSITIVITY - ONE-SHOT DRIVER
#
# Answers R2 comment 2.4: the published sensitivity varies the SPATIAL
# bandwidth only. This runs the temporal one end to end.
#
# RUN (from the code/ directory):
#   Rscript code/run_temporal_bandwidth.R
#   Rscript code/run_temporal_bandwidth.R --force   # re-run existing kernels
#
# Needs the TU microdata Access database, which step2 opens by absolute
# path. Everything downstream of the kernels is public.
#
# WHAT IT DOES
#   1. Re-estimates the KDE kernels twice, at H_T = 0.5 h and H_T = 2 h,
#      holding the spatial bandwidth at its baseline H_D = 5 km. Each run
#      is a separate R process, so no parameter leaks between them --
#      step2 guards H_T with if(!exists(...)), which would silently reuse
#      the first value if both ran in one session.
#   2. Copies the six RDS files of each run into data/kernels/<scenario>/.
#   3. Runs step9b_temporal_bandwidth.R, which reports drift on BOTH axes
#      across the three temporal bandwidths and writes the SI table.
#
# OUTPUTS
#   results/bandwidth_sensitivity/ht_drift_summary.csv
#   results/bandwidth_sensitivity/ht_drift_bars.png
#   results/bandwidth_sensitivity/table_ht_sensitivity.tex
#
# Runtime is dominated by step2, which reads the whole diary database
# twice. Expect this to be slow.
######################################################################

args  <- commandArgs(trailingOnly = TRUE)
FORCE <- "--force" %in% args

if (!dir.exists(file.path("code")) || !file.exists(file.path("code", "step2_kernel_generation.R"))) {
  stop("Run this from the code/ directory: Rscript code/run_temporal_bandwidth.R")
}

RSCRIPT   <- file.path(R.home("bin"), "Rscript")
CODE_ROOT <- normalizePath(getwd(), winslash = "/")
PROJ_ROOT <- normalizePath("..",     winslash = "/")

# The six files step2 writes that downstream steps consume.
KERNEL_FILES <- c(
  "kde_baseline_by_cohort.rds",
  "kde_yearbin_by_cohort.rds",
  "kde_difference_by_cohort.rds",
  "grid_mv_step2.rds",
  "temporal_baseline_away.rds",
  "temporal_yearbin_away.rds"
)

scenarios <- list(
  ht05 = list(H_T = 0.5, out = file.path(PROJ_ROOT, "Paper Results HT05")),
  ht20 = list(H_T = 2.0, out = file.path(PROJ_ROOT, "Paper Results HT20"))
)

cat("\n=== TEMPORAL BANDWIDTH SENSITIVITY ===\n")
cat("Code root: ", CODE_ROOT, "\n")
cat("Force re-run: ", FORCE, "\n\n")

# ===========================================================================
# 1) KERNELS
# ===========================================================================
run_step2 <- function(name, H_T, out_dir) {
  kernel_dir <- file.path("data", "kernels", name)
  target     <- file.path(kernel_dir, "kde_yearbin_by_cohort.rds")

  if (file.exists(target) && !FORCE) {
    cat("[", name, "] kernels already present, skipping. Use --force to redo.\n", sep = "")
    return(invisible(TRUE))
  }

  cat("\n--------------------------------------------------------------\n")
  cat(">> step2 kernels for ", name, " (H_T = ", H_T, " h, H_D = baseline)\n", sep = "")
  cat("--------------------------------------------------------------\n")

  # Separate process, so step2's if(!exists("H_T")) guard cannot reuse a
  # value left behind by the previous scenario. deparse() handles the
  # spaces and backslashes in the Windows paths.
  wrapper <- tempfile(fileext = ".R")
  writeLines(c(
    sprintf("setwd(%s)",      deparse(CODE_ROOT)),
    sprintf("H_T <- %s",      format(H_T)),
    sprintf("output_dir <- %s", deparse(out_dir)),
    "source('code/step2_kernel_generation.R')"
  ), wrapper)

  status <- system2(RSCRIPT, shQuote(wrapper))
  if (status != 0) stop("step2 failed for scenario '", name, "' (exit ", status, ")")

  # Stage the kernels where step9b expects them.
  if (!dir.exists(kernel_dir)) dir.create(kernel_dir, recursive = TRUE)
  src <- file.path(out_dir, KERNEL_FILES)
  missing <- KERNEL_FILES[!file.exists(src)]
  if (length(missing)) {
    stop("step2 did not produce: ", paste(missing, collapse = ", "),
         "\nLooked in: ", out_dir)
  }
  ok <- file.copy(src, file.path(kernel_dir, KERNEL_FILES), overwrite = TRUE)
  if (!all(ok)) stop("Could not stage kernels into ", kernel_dir)

  cat("  staged ", sum(ok), " kernel files -> ", kernel_dir, "\n", sep = "")
  invisible(TRUE)
}

for (nm in names(scenarios)) {
  run_step2(nm, scenarios[[nm]]$H_T, scenarios[[nm]]$out)
}

# ===========================================================================
# 2) SENSITIVITY
# ===========================================================================
cat("\n--------------------------------------------------------------\n")
cat(">> step9b - drift on both axes across temporal bandwidths\n")
cat("--------------------------------------------------------------\n")

status <- system2(RSCRIPT, shQuote(file.path("code", "step9b_temporal_bandwidth.R")))
if (status != 0) stop("step9b failed (exit ", status, ")")

# ===========================================================================
# 3) WHERE THINGS LANDED
# ===========================================================================
out_dir <- file.path("results", "bandwidth_sensitivity")
cat("\n=== DONE ===\n")
for (f in c("ht_drift_summary.csv", "ht_drift_bars.png", "table_ht_sensitivity.tex")) {
  p <- file.path(out_dir, f)
  cat(if (file.exists(p)) "  ok   " else "  MISSING ", p, "\n", sep = "")
}
cat("\nPaste table_ht_sensitivity.tex into NatComm_R2_supp.tex, then rewrite\n")
cat("response 2.4 against the numbers rather than the argument.\n")
