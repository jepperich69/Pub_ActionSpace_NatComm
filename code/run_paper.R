######################################################################
# run_paper.R - reproduce every figure and table in the manuscript
#
# RUN (from the code/ directory)
# ------------------------------
#   Rscript code/run_paper.R                 # public stage only (default)
#   Rscript code/run_paper.R --with-restricted
#   Rscript code/run_paper.R --manifest-only # check, run nothing
#
# TWO STAGES
# ----------
# RESTRICTED  Steps that read the Danish National Travel Survey microdata
#             directly from a local Access database. That database is not
#             redistributable, so these steps cannot run from a public
#             clone. They own Supplementary Table S1, Supplementary
#             Figure S7, and the KDE kernels themselves.
#
# PUBLIC      Everything downstream of the kernels shipped in data/kernels/.
#             This is what a reader can reproduce.
#
# The run ends with a MANIFEST listing every figure and table the
# manuscript includes, whether the file is present, and which script owns
# it. Assets with no owning script are reported as UNOWNED: those are
# real gaps in the reproduction suite, not incidental omissions.
#
# Each step runs in its own R session (via Rscript) so that variables
# such as `scenario` cannot leak between steps.
######################################################################

source("code/utils_io.R")

args            <- commandArgs(trailingOnly = TRUE)
with_restricted <- "--with-restricted" %in% args
manifest_only   <- "--manifest-only"   %in% args

SCENARIOS <- c("baseline", "sex1", "sex2",
               "city_10000", "city_25000", "city_50000", "city_100000")

# ============================================================================
# STEP RUNNER
# ============================================================================

failures <- character(0)

run_step <- function(script, args = character(0), label = script) {
  cat("\n--------------------------------------------------------------\n")
  cat(">>", label, "\n")
  cat("--------------------------------------------------------------\n")

  status <- system2("Rscript", c(file.path("code", script), args))

  if (status != 0) {
    failures <<- c(failures, label)
    cat("\n  !! FAILED:", label, "(exit", status, ")\n")
  }
  invisible(status)
}

# ============================================================================
# RESTRICTED STAGE
# ============================================================================

if (manifest_only) {
  cat("\n[manifest-only: no steps will be run]\n")
} else if (with_restricted) {
  cat("\n############ RESTRICTED STAGE (requires TU microdata) ############\n")
  run_step("step1_discriptive_stat.R", label = "step1 - descriptives, Table S1, Figure S7")
  run_step("step2_kernel_generation.R", label = "step2 - KDE kernel generation")
  run_step("step2_run_bw3.R", label = "step2 - kernels, bandwidth 3 km")
  run_step("step2_run_bw7.R", label = "step2 - kernels, bandwidth 7 km")
} else {
  cat("\n############ RESTRICTED STAGE: SKIPPED ############\n")
  cat("Steps 1 and 2 read the TU microdata Access database, which is not\n")
  cat("redistributable. Pass --with-restricted to run them.\n")
  cat("Supplementary Table S1 and Supplementary Figure S7 come from step 1;\n")
  cat("the kernels in data/kernels/ come from step 2 and are shipped.\n")
}

# ============================================================================
# PUBLIC STAGE
# ============================================================================

if (!manifest_only) {
  cat("\n############ PUBLIC STAGE (from data/kernels/) ############\n")

  for (s in SCENARIOS) {
    run_step("run_one_scenario.R", s,
             label = paste0("steps 2b-6 - scenario: ", s, " (", get_scenario_label(s), ")"))
  }

  run_step("step5b_sex_drift.R",         label = "step5b - Supplementary Figure S5 panels")
  run_step("step4_integral_R2.R",        label = "step4 (R2) - Figure 5")
  run_step("step7_urban_robustness.R",   label = "step7 - Supplementary Figure S4")
  run_step("step8_uncertainty_R2.R",     label = "step8 (R2) - Figure 6a, Supplementary Figure S8")
  run_step("step9_bandwidth_sensitivity.R", label = "step9 - Supplementary Figure S6, Table S3")
  run_step("check_drift_periods.R",      label = "check - drift period comparison table")
}

# ============================================================================
# MANIFEST
# ============================================================================

fig_dir <- get_manuscript_fig_dir()

# owner = NA marks an asset no script in this repository produces.
manifest <- rbind(
  # ---- main text figures ----
  data.frame(item = "Figure 1",  file = file.path(fig_dir, "activity_graph_v2_small.png"),           owner = NA),
  data.frame(item = "Figure 2",  file = file.path(fig_dir, "kde_YEARBIN_comparison_FIXED.png"),      owner = "step2b_visualization.R"),
  data.frame(item = "Figure 3",  file = file.path(fig_dir, "kde_DIFFERENCE_by_cohort_SHARP.png"),    owner = "step2c_visualization_transpose.R"),
  data.frame(item = "Figure 4",  file = file.path(fig_dir, "temporal_profiles_all_ages.png"),        owner = "step3_daytime.R"),
  data.frame(item = "Figure 5",  file = file.path(fig_dir, "Figure_R2_5.png"),                       owner = "step4_integral_R2.R"),
  data.frame(item = "Figure 6a", file = file.path(fig_dir, "Figure_R2_6a.png"),                      owner = "step8_uncertainty_R2.R"),
  data.frame(item = "Figure 6b", file = file.path(fig_dir, "action_space_trajectory_TRIPWEIGHTED.png"), owner = "step4_integral.R"),

  # ---- supplementary figures ----
  data.frame(item = "Figure S1", file = file.path(fig_dir, "window_shifts.png"),                     owner = "step3_daytime.R"),
  data.frame(item = "Figure S2", file = file.path(fig_dir, "mean_time_shift.png"),                   owner = "step3_daytime.R"),
  data.frame(item = "Figure S3", file = file.path(fig_dir, "mean_distance_trend_TRIPWEIGHTED.png"),  owner = "step4_integral.R"),
  data.frame(item = "Figure S4", file = file.path(fig_dir, "urban_drift_comparison.png"),            owner = "step7_urban_robustness.R"),
  data.frame(item = "Figure S5a", file = file.path(fig_dir, "drift_vectors_males.png"),              owner = "step5b_sex_drift.R"),
  data.frame(item = "Figure S5b", file = file.path(fig_dir, "drift_vectors_females.png"),            owner = "step5b_sex_drift.R"),
  data.frame(item = "Figure S6", file = file.path(fig_dir, "bw_drift_bars.png"),                     owner = "step9_bandwidth_sensitivity.R"),
  data.frame(item = "Figure S7", file = file.path(fig_dir, "Figure_R2_S7.png"),                      owner = "step1_discriptive_stat.R"),
  data.frame(item = "Figure S8", file = file.path(fig_dir, "Figure_R2_S8.png"),                      owner = "step8_uncertainty_R2.R"),

  # ---- tables ----
  data.frame(item = "Table 1",   file = "results/baseline/table_main1_descriptive.tex",              owner = NA),
  data.frame(item = "Table S1",  file = "results/baseline/table_s1_descriptive.tex",                 owner = "step1_discriptive_stat.R"),
  data.frame(item = "Table S2",  file = "results/baseline/table_path_complexity.tex",                owner = "step6_final_plots.R"),
  data.frame(item = "Table S3",  file = "results/bandwidth_sensitivity/table_bandwidth_drift.tex",   owner = NA)
)

manifest$present <- file.exists(manifest$file)
manifest$status  <- ifelse(!is.na(manifest$owner) & manifest$present, "OK",
                    ifelse(is.na(manifest$owner),                     "UNOWNED",
                                                                      "MISSING"))

cat("\n\n==============================================================\n")
cat("MANIFEST - manuscript figures and tables\n")
cat("==============================================================\n\n")

for (i in seq_len(nrow(manifest))) {
  cat(sprintf("  %-11s %-8s %-34s %s\n",
              manifest$item[i],
              manifest$status[i],
              basename(manifest$file[i]),
              if (is.na(manifest$owner[i])) "(no script produces this)" else manifest$owner[i]))
}

n_ok      <- sum(manifest$status == "OK")
n_unowned <- sum(manifest$status == "UNOWNED")
n_missing <- sum(manifest$status == "MISSING")

cat("\n  ", n_ok, "reproducible /", nrow(manifest), "assets\n")

if (n_unowned > 0) {
  cat("\n  ", n_unowned, "UNOWNED - no script in this repository generates these:\n")
  for (f in manifest$item[manifest$status == "UNOWNED"]) cat("      -", f, "\n")
  cat("   Figure 1 is a hand-drawn conceptual schematic and is expected here.\n")
  cat("   The tables are hand-formatted in the LaTeX source and are a real gap.\n")
}

if (n_missing > 0) {
  cat("\n  ", n_missing, "MISSING - a script owns these but the file is absent:\n")
  for (f in manifest$item[manifest$status == "MISSING"]) cat("      -", f, "\n")
  cat("   Figure S7 and Table S1 are expected to be missing unless the\n")
  cat("   restricted stage was run.\n")
}

if (length(failures) > 0) {
  cat("\n  ", length(failures), "STEP(S) FAILED:\n")
  for (f in failures) cat("      -", f, "\n")
}

write.csv(manifest, file.path("results", "manuscript_manifest.csv"), row.names = FALSE)
cat("\n  Manifest written to results/manuscript_manifest.csv\n\n")

if (length(failures) > 0) quit(status = 1)
