######################################################################
# STEP 5b - SEX-STRATIFIED DRIFT FIGURES (Supplementary Figure S5)
#
# PURPOSE
# -------
# Exports the male and female drift-vector panels that make up
# Supplementary Figure S5 under the filenames the manuscript includes.
#
# Until now these two files existed only as hand-made copies: someone
# duplicated results/sex1/drift_vectors_MINIMAL_COMBINED.png and
# results/sex2/drift_vectors_MINIMAL_COMBINED.png and renamed them by
# hand, with the sex2 copy landing as "drift_vectors_female.png"
# (singular) in results/ but "drift_vectors_females.png" (plural) in the
# manuscript. No script produced either name, so Supplementary Figure S5
# could not be regenerated from the repository. This script closes that
# gap and fixes the name mismatch.
#
# INPUTS
# ------
# results/sex1/drift_vectors_MINIMAL_COMBINED.png   (step 5, scenario sex1)
# results/sex2/drift_vectors_MINIMAL_COMBINED.png   (step 5, scenario sex2)
#
# OUTPUTS
# -------
# results/sex1/drift_vectors_males.png
# results/sex2/drift_vectors_females.png
# <manuscript figure dir>/drift_vectors_males.png
# <manuscript figure dir>/drift_vectors_females.png
#
# RUN
# ---
# Rscript code/step5b_sex_drift.R
#
# Requires step 5 to have run for the sex1 and sex2 scenarios first:
#   Rscript code/run_one_scenario.R sex1
#   Rscript code/run_one_scenario.R sex2
######################################################################

source("code/utils_io.R")

cat("\n=== STEP 5b: SEX-STRATIFIED DRIFT FIGURES (S5) ===\n\n")

SOURCE_PLOT <- "drift_vectors_MINIMAL_COMBINED.png"

panels <- list(
  list(scenario = "sex1", out = "drift_vectors_males.png"),
  list(scenario = "sex2", out = "drift_vectors_females.png")
)

fig_dir <- get_manuscript_fig_dir()
missing <- character(0)

for (p in panels) {
  src <- file.path("results", p$scenario, SOURCE_PLOT)

  if (!file.exists(src)) {
    missing <- c(missing, src)
    cat("  x missing input:", src, "\n")
    next
  }

  scenario_copy <- file.path("results", p$scenario, p$out)
  manuscript_copy <- file.path(fig_dir, p$out)

  ok <- file.copy(src, scenario_copy, overwrite = TRUE) &&
        file.copy(src, manuscript_copy, overwrite = TRUE)

  if (!ok) stop("Failed to write ", p$out)

  cat("  ✓", get_scenario_label(p$scenario), "->", p$out, "\n")
}

if (length(missing)) {
  stop("Step 5 has not been run for: ",
       paste(vapply(strsplit(missing, "/"), `[`, character(1), 2),
             collapse = ", "),
       "\nRun: Rscript code/run_one_scenario.R <scenario>")
}

cat("\n=== STEP 5b COMPLETE ===\n\n")
