######################################################################
# STEP 10 - COLLECT MANUSCRIPT FIGURES
#
# PURPOSE
# -------
# Exports the figures the manuscript includes from the scenario result
# folders into the manuscript figure directory, under the filenames the
# LaTeX source expects.
#
# WHY THIS EXISTS
# ---------------
# Most pipeline steps write only into results/<scenario>/. Only step 1,
# step 4 (R2), step 5b and step 8 (R2) ever exported to the manuscript
# figure directory. Every other manuscript figure had been copied there
# by hand, which meant a clean run of the suite reproduced the numbers
# but not the figures: eleven of sixteen came back missing. This step
# closes that gap, so run_paper.R can verify every figure against the
# recorded manuscript checksums.
#
# All sources are baseline-scenario outputs except where noted. Scenario
# variants of the same filenames exist under results/<scenario>/ and are
# deliberately not exported.
#
# RUN
# ---
# Rscript code/step10_collect_figures.R
######################################################################

source("code/utils_io.R")

cat("\n=== STEP 10: COLLECT MANUSCRIPT FIGURES ===\n\n")

figures <- list(
  list(item = "Figure 2",   src = "results/baseline/kde_YEARBIN_comparison_FIXED.png"),
  list(item = "Figure 3",   src = "results/baseline/kde_DIFFERENCE_by_cohort_SHARP.png"),
  list(item = "Figure 4",   src = "results/baseline/temporal_profiles_all_ages.png"),
  list(item = "Figure 6b",  src = "results/baseline/action_space_trajectory_TRIPWEIGHTED.png"),
  list(item = "Figure S1",  src = "results/baseline/window_shifts.png"),
  list(item = "Figure S2",  src = "results/baseline/mean_time_shift.png"),
  list(item = "Figure S3",  src = "results/baseline/mean_distance_trend_TRIPWEIGHTED.png"),
  list(item = "Figure S4",  src = "results/urban_robustness/urban_drift_comparison.png"),
  list(item = "Figure S6",  src = "results/bandwidth_sensitivity/bw_drift_bars.png")
)

fig_dir <- get_manuscript_fig_dir()
missing <- character(0)

for (f in figures) {
  if (!file.exists(f$src)) {
    missing <- c(missing, f$src)
    cat("  x", f$item, "- missing input:", f$src, "\n")
    next
  }

  dest <- file.path(fig_dir, basename(f$src))
  if (!file.copy(f$src, dest, overwrite = TRUE)) stop("Failed to write ", dest)

  cat("  ✓", f$item, "->", basename(dest), "\n")
}

cat("\nCollected", length(figures) - length(missing), "of", length(figures),
    "figures into", fig_dir, "\n")

# Generated LaTeX tables that the manuscript \input directly. Supplementary
# Table S4 is the only one wired up this way so far; the other three are still
# hand-formatted in the LaTeX source and are reported UNOWNED by run_paper.R.
tables <- list(
  list(item = "Table S4", src = "results/baseline/table_s4_covid_departure.tex")
)

tab_dir <- file.path(dirname(fig_dir), "tables")
if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE)

for (t in tables) {
  if (!file.exists(t$src)) {
    cat("  x", t$item, "- missing input:", t$src, "\n")
    next
  }
  if (!file.copy(t$src, file.path(tab_dir, basename(t$src)), overwrite = TRUE))
    stop("Failed to write ", t$item)
  cat("  ✓", t$item, "->", basename(t$src), "\n")
}

if (length(missing)) {
  cat("\nMissing inputs. Run the earlier steps first:\n")
  for (m in missing) cat("  -", m, "\n")
  cat("\n")
}

cat("=== STEP 10 COMPLETE ===\n\n")
