# code/00_validate_inputs.R

required_files <- c(
  "grid_mv_step2.rds",
  "kde_baseline_by_cohort.rds",
  "kde_difference_by_cohort.rds",
  "kde_yearbin_by_cohort.rds",
  "temporal_baseline_away.rds",
  "temporal_yearbin_away.rds"
)

kernels_root <- file.path("data", "kernels")
if (!dir.exists(kernels_root)) stop("Missing folder: ", kernels_root)

scenarios <- list.dirs(kernels_root, full.names = FALSE, recursive = FALSE)
if (length(scenarios) == 0) stop("No scenario folders found in: ", kernels_root)

cat("Found scenarios:\n")
print(scenarios)

ok <- TRUE
for (s in scenarios) {
  sdir <- file.path(kernels_root, s)
  missing <- required_files[!file.exists(file.path(sdir, required_files))]
  if (length(missing) > 0) {
    ok <- FALSE
    cat("\nScenario:", s, "is MISSING:\n")
    print(missing)
  } else {
    cat("\nScenario:", s, "OK (all required files present)\n")
  }
}

if (!ok) stop("\nValidation failed: some scenarios missing required files.\n")
cat("\nAll scenarios validated successfully.\n")
