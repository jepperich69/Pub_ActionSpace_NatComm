######################################################################
# BUILD SUPPLEMENTARY TABLE BODIES
#
# PURPOSE
# -------
# Writes the tabular body of every supplementary table whose numbers come
# out of the pipeline, so the SI can \input them instead of carrying a
# hand-typed copy.
#
# WHY THIS EXISTS
# ---------------
# The manifest in run_paper.R compares figures pixel for pixel but never
# looked inside a table. Supplementary Tables S6 and S7 were pre-fixed-weight
# in every cell for a full revision cycle, with the 56-65 sign inverted in
# both bodies and in both captions, while the pipeline reported success.
# A table the manuscript \inputs cannot drift away from the CSV it came from.
#
# Captions and table notes stay in the SI. Only the tabular goes here.
#
# INPUTS
#   results/baseline/path_complexity_metrics.csv          -> Table S5
#   results/bandwidth_sensitivity/bw_drift_summary.csv    -> Table S6
#   results/bandwidth_sensitivity/ht_drift_summary.csv    -> Table S7
#   results/uncertainty/baseline/bootstrap_summary.csv    -> Table S8
#   results/uncertainty/baseline/bootstrap_timing_summary.csv
#   results/baseline/table_s4_covid_departure.tex         -> Table S4 (copied)
#
# OUTPUTS
#   results/tables/table_s{4,5,6,7,8}_*.tex, and the same files copied into
#   the manuscript table directory (Overleaf_source/tables/ by default;
#   override with ACTIONSPACE_TABLE_DIR).
#
# RUN
#   Rscript code/build_si_tables.R
######################################################################

suppressPackageStartupMessages({
  library(data.table)
})

source("code/utils_io.R")

cat("\n=== BUILD SUPPLEMENTARY TABLE BODIES ===\n\n")

AGE_LEVELS <- c("10-17", "18-30", "31-55", "56-65", "66+")
AGE_TEX    <- c("10--17", "18--30", "31--55", "56--65", "66+")
FINAL_PERIOD <- "2022-2024"

out_dir <- file.path("results", "tables")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
tab_dir <- get_manuscript_table_dir()

# Signed value with a LaTeX math wrapper, e.g. $+0.306$.
num <- function(x, digits) {
  paste0(ifelse(x >= 0, "+", "-"), formatC(abs(x), format = "f", digits = digits))
}

signed <- function(x, digits) sprintf("$%s$", num(x, digits))

write_table <- function(lines, file) {
  path <- file.path(out_dir, file)
  writeLines(lines, path)
  dest <- file.path(tab_dir, file)
  if (!file.copy(path, dest, overwrite = TRUE)) stop("Failed to write ", dest)
  cat("  ok", file, "->", dest, "\n")
}

ordered_rows <- function(dt) {
  dt[match(AGE_LEVELS, dt$AgeGroup), ]
}

# ---------------------------------------------------------------------------
# Supplementary Table S4: pandemic departures. Written by step11; copied here
# so the manuscript table directory has a single producer.
# ---------------------------------------------------------------------------
s4_src <- file.path("results", "baseline", "table_s4_covid_departure.tex")
if (file.exists(s4_src)) {
  file.copy(s4_src, file.path(out_dir, "table_s4_covid_departure.tex"), overwrite = TRUE)
  dest <- file.path(tab_dir, "table_s4_covid_departure.tex")
  if (!file.copy(s4_src, dest, overwrite = TRUE)) stop("Failed to write ", dest)
  cat("  ok table_s4_covid_departure.tex ->", dest, "\n")
} else {
  cat("  x table_s4_covid_departure.tex - missing input:", s4_src, "\n")
}

# ---------------------------------------------------------------------------
# Supplementary Table S5: path complexity
# ---------------------------------------------------------------------------
pc <- ordered_rows(fread(file.path("results", "baseline", "path_complexity_metrics.csv")))
lines <- c(
  "\\begin{tabular}{lccccc}",
  "\\toprule",
  "\\textbf{Age group} &",
  "\\textbf{Displacement} &",
  "\\textbf{Path length} &",
  "\\textbf{Tortuosity} &",
  "\\textbf{R} &",
  "\\textbf{Circular SD} \\\\",
  " & (km) & (km) & (ratio) & & (degrees) \\\\",
  "\\midrule",
  sprintf("%s & %.3f & %.3f & %.2f & %.3f & %.1f \\\\",
          AGE_TEX, pc$endpoint_displacement, pc$path_length,
          pc$tortuosity, pc$R, pc$circular_sd),
  "\\bottomrule",
  "\\end{tabular}"
)
write_table(lines, "table_s5_path_complexity.tex")

# ---------------------------------------------------------------------------
# Supplementary Table S6: spatial bandwidth sweep, final-period drift
# ---------------------------------------------------------------------------
bw <- fread(file.path("results", "bandwidth_sensitivity", "bw_drift_summary.csv"))
bw <- bw[Period == FINAL_PERIOD]
wide <- dcast(bw, AgeGroup ~ bandwidth, value.var = "drift")
wide <- ordered_rows(wide)
stopifnot(all(c("bw3", "baseline", "bw7") %in% names(wide)))
lines <- c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "\\textbf{Age group} & \\textbf{$h_r = 3$\\,km} & \\textbf{$h_r = 5$\\,km (baseline)} & \\textbf{$h_r = 7$\\,km} \\\\",
  "\\midrule",
  sprintf("%s & %s & %s & %s \\\\", AGE_TEX,
          signed(wide$bw3, 3), signed(wide$baseline, 3), signed(wide$bw7, 3)),
  "\\bottomrule",
  "\\end{tabular}"
)
write_table(lines, "table_s6_bw_sensitivity.tex")

# ---------------------------------------------------------------------------
# Supplementary Table S7: temporal bandwidth sweep, final-period drift
# ---------------------------------------------------------------------------
ht <- fread(file.path("results", "bandwidth_sensitivity", "ht_drift_summary.csv"))
ht <- ht[Period == FINAL_PERIOD]
d_wide <- ordered_rows(dcast(ht, AgeGroup ~ bandwidth, value.var = "drift_distance"))
f_wide <- ordered_rows(dcast(ht, AgeGroup ~ bandwidth, value.var = "drift_fraction"))
stopifnot(all(c("ht05", "baseline", "ht20") %in% names(d_wide)))
lines <- c(
  "\\begin{tabular}{lccccccc}",
  "\\toprule",
  " & \\multicolumn{3}{c}{\\textbf{Distance when away (km)}} & & \\multicolumn{3}{c}{\\textbf{Time away (pp)}} \\\\",
  "\\cmidrule(lr){2-4}\\cmidrule(lr){6-8}",
  "\\textbf{Age group} & $h_t=0.5$ & $h_t=1$ & $h_t=2$ & & $h_t=0.5$ & $h_t=1$ & $h_t=2$ \\\\",
  "\\midrule",
  sprintf("%s & %s & %s & %s & & %s & %s & %s \\\\", AGE_TEX,
          signed(d_wide$ht05, 3), signed(d_wide$baseline, 3), signed(d_wide$ht20, 3),
          signed(f_wide$ht05 * 100, 5), signed(f_wide$baseline * 100, 5),
          signed(f_wide$ht20 * 100, 5)),
  "\\bottomrule",
  "\\end{tabular}"
)
write_table(lines, "table_s7_ht_sensitivity.tex")

# ---------------------------------------------------------------------------
# Supplementary Table S8: marginal quantities with bootstrap intervals
# ---------------------------------------------------------------------------
bs <- ordered_rows(fread(file.path("results", "uncertainty", "baseline", "bootstrap_summary.csv")))
tm <- ordered_rows(fread(file.path("results", "uncertainty", "baseline", "bootstrap_timing_summary.csv")))
lines <- c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  " & \\textbf{Distance when away} & \\textbf{Fraction of day away} & \\textbf{Mean timing when away} \\\\",
  "\\textbf{Age group} & (\\% change) & (\\% change) & (hours) \\\\",
  "\\midrule",
  sprintf("%s & %s $[%s, %s]$ & %s $[%s, %s]$ & %s $[%s, %s]$ \\\\", AGE_TEX,
          signed(bs$kde_pct, 2), num(bs$boot_lo, 2), num(bs$boot_hi, 2),
          signed(bs$kde_away_pct, 2), num(bs$away_lo, 2), num(bs$away_hi, 2),
          signed(tm$hour_shift, 3), num(tm$hour_shift_lo, 3), num(tm$hour_shift_hi, 3)),
  "\\bottomrule",
  "\\end{tabular}"
)
write_table(lines, "table_s8_marginals.tex")

cat("\nSupplementary table bodies written to", out_dir, "and", tab_dir, "\n")
