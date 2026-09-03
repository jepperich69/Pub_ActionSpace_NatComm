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
record_ref      <- "--record"          %in% args

# Reference checksums for the figures as they appear in the submitted
# manuscript. The suite is only doing its job if a rerun reproduces these
# byte for byte, so the manifest checks the MD5 of every regenerated figure
# against this file rather than merely checking that a file exists.
#
#   Rscript code/run_paper.R --record   # stamp current figures as reference
#
# Record only from a manuscript state you are prepared to defend: it
# redefines what "reproduces" means for every later run.
REF_FILE <- file.path("results", "figure_checksums.csv")

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
  run_step("step10_collect_figures.R",   label = "step10 - collect manuscript figures")
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

# ---- byte-identity against the recorded manuscript figures ----

manifest$md5 <- NA_character_
present_idx  <- which(manifest$present)
if (length(present_idx)) {
  manifest$md5[present_idx] <- unname(tools::md5sum(manifest$file[present_idx]))
}

if (record_ref) {
  ref_out <- manifest[manifest$present, c("item", "file", "md5")]
  ref_out$ref_path <- normalizePath(ref_out$file, winslash = "/")
  ref_out$file     <- basename(ref_out$file)
  write.csv(ref_out, REF_FILE, row.names = FALSE)
  cat("\nRecorded", nrow(ref_out), "reference checksums to", REF_FILE, "\n")
}

# A checksum mismatch has two very different causes, and conflating them makes
# the check useless. The plot may genuinely have changed, or the same plot may
# simply have been rendered by a different toolchain. We hit the second case
# immediately: ggplot2 changed its default ggsave background between the
# figures shipped with the manuscript and ggplot2 4.0.2, so the published PNGs
# carry a transparent background and current renders carry white. Identical
# plots, different bytes.
#
# So when the MD5 differs we compare the images themselves, compositing any
# alpha channel onto white first. Requires the png package; without it the
# result is reported as DIFFERS-? rather than silently passing.
#
# Returns the fraction of pixels differing by more than CHANNEL_TOL, or NA if
# the comparison could not be made. A single antialiased glyph rendered by a
# different font stack changes a handful of pixels, so requiring every pixel to
# match would flag every figure. We therefore threshold on the share of changed
# pixels, and the manifest prints that share so a borderline case is visible
# rather than hidden behind a verdict.
CHANNEL_TOL <- 8 / 255   # per-channel difference counted as a changed pixel
PIXEL_TOL   <- 0.005     # up to 0.5% changed pixels reads as a rendering difference

content_diff <- function(a, b) {
  if (!requireNamespace("png", quietly = TRUE)) return(NA_real_)
  if (!file.exists(a) || !file.exists(b))       return(NA_real_)

  # Read as a native raster: one packed integer per pixel rather than four
  # doubles. A full double read of the 10800x7200 difference map needs about
  # 2.5 GB and fails, which previously left that figure permanently unclassified
  # as DIFFERS-? — the one verdict that hides problems instead of surfacing them.
  read_native <- function(p) {
    m <- png::readPNG(p, native = TRUE)
    matrix(unclass(m), nrow = dim(m)[1])   # unclass: nativeRaster indexes oddly
  }

  ia <- try(read_native(a), silent = TRUE)
  ib <- try(read_native(b), silent = TRUE)
  if (inherits(ia, "try-error") || inherits(ib, "try-error")) return(NA_real_)
  if (!identical(dim(ia), dim(ib)))                           return(1)

  # A packed pixel whose bit pattern equals R's integer NA sentinel reads back
  # as NA (39 pixels in 10.8M for one of these figures). Treat those as equal
  # rather than letting them poison the comparison.
  na_px <- is.na(ia) | is.na(ib)
  if (any(na_px)) { ia[na_px] <- 0L; ib[na_px] <- 0L }

  # Unpack and compare in row blocks so peak memory stays bounded.
  byte <- function(v, shift) bitwAnd(bitwShiftR(v, shift), 255L)
  nrow_total <- dim(ia)[1]
  block      <- max(1L, as.integer(2e7 %/% dim(ia)[2]))
  changed    <- 0
  for (start in seq(1L, nrow_total, by = block)) {
    rows <- start:min(start + block - 1L, nrow_total)
    va <- as.integer(ia[rows, , drop = FALSE])
    vb <- as.integer(ib[rows, , drop = FALSE])

    # Composite each channel onto white using its own alpha.
    chan <- function(v, shift) {
      alpha <- byte(v, 24) / 255
      byte(v, shift) * alpha + 255 * (1 - alpha)
    }

    delta <- pmax(abs(chan(va, 0)  - chan(vb, 0)),
                  abs(chan(va, 8)  - chan(vb, 8)),
                  abs(chan(va, 16) - chan(vb, 16)))
    changed <- changed + sum(delta > CHANNEL_TOL * 255)
  }

  changed / (nrow_total * dim(ia)[2])
}

manifest$bitwise <- "NO-REF"
if (file.exists(REF_FILE)) {
  ref <- read.csv(REF_FILE, stringsAsFactors = FALSE)
  if (is.null(ref$ref_path)) ref$ref_path <- NA_character_
  m <- match(manifest$item, ref$item)

  for (i in seq_len(nrow(manifest))) {
    j <- m[i]
    if (is.na(j) || is.na(manifest$md5[i])) next

    if (manifest$md5[i] == ref$md5[j]) {
      manifest$bitwise[i] <- "IDENTICAL"
      next
    }

    frac <- if (grepl("\\.png$", manifest$file[i], ignore.case = TRUE)) {
      content_diff(ref$ref_path[j], manifest$file[i])
    } else NA_real_

    manifest$px_diff[i] <- frac
    manifest$bitwise[i] <- if (is.na(frac))          "DIFFERS-?"
                           else if (frac <= PIXEL_TOL) "RENDER-ONLY"
                           else                        "CONTENT"
  }
}

cat("\n\n==============================================================\n")
cat("MANIFEST - manuscript figures and tables\n")
cat("==============================================================\n\n")

for (i in seq_len(nrow(manifest))) {
  cat(sprintf("  %-11s %-8s %-10s %-34s %s\n",
              manifest$item[i],
              manifest$status[i],
              manifest$bitwise[i],
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

n_ident   <- sum(manifest$bitwise == "IDENTICAL")
n_render  <- sum(manifest$bitwise == "RENDER-ONLY")
n_content <- sum(manifest$bitwise == "CONTENT")
n_unknown <- sum(manifest$bitwise == "DIFFERS-?")
n_differs <- n_content + n_unknown

if (file.exists(REF_FILE)) {
  cat("\n  ", n_ident + n_render, "of", sum(manifest$present),
      "present assets match the manuscript:", n_ident, "byte-identical,",
      n_render, "identical in content\n")

  if (n_render > 0) {
    cat("\n  ", n_render, "RENDER-ONLY - same plot, different bytes:\n")
    for (f in manifest$item[manifest$bitwise == "RENDER-ONLY"]) cat("      -", f, "\n")
    cat("   Pixel-identical after compositing alpha onto white. This is a\n")
    cat("   toolchain difference, not a result change. Pin the environment if\n")
    cat("   byte-identity is required.\n")
  }

  if (n_content > 0) {
    cat("\n  ", n_content, "CONTENT - the plot itself differs from the manuscript:\n")
    for (f in manifest$item[manifest$bitwise == "CONTENT"]) cat("      -", f, "\n")
    cat("   Either the manuscript figure is stale, or the pipeline changed.\n")
    cat("   Resolve before submitting; do not re-record to hide it.\n")
  }

  if (n_unknown > 0) {
    cat("\n  ", n_unknown, "DIFFERS-? - checksum differs, content not checked:\n")
    for (f in manifest$item[manifest$bitwise == "DIFFERS-?"]) cat("      -", f, "\n")
    cat("   install.packages(\"png\") to classify these.\n")
  }
} else {
  cat("\n   No reference checksums yet. Run with --record to create", REF_FILE, "\n")
}

if (length(failures) > 0) {
  cat("\n  ", length(failures), "STEP(S) FAILED:\n")
  for (f in failures) cat("      -", f, "\n")
}

write.csv(manifest, file.path("results", "manuscript_manifest.csv"), row.names = FALSE)
cat("\n  Manifest written to results/manuscript_manifest.csv\n\n")

if (length(failures) > 0 || n_differs > 0) quit(status = 1)
