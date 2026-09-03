# code/utils_io.R

get_scenario_label <- function(scenario) {
  labels <- c(
    baseline     = "All individuals",
    sex1         = "Males",
    sex2         = "Females",
    city_10000   = "Cities ≥ 10,000",
    city_25000   = "Cities ≥ 25,000",
    city_50000   = "Cities ≥ 50,000",
    city_100000  = "Cities ≥ 100,000",
    bw3          = "Bandwidth H_D = 3 km",
    bw7          = "Bandwidth H_D = 7 km"
  )
  
  if (!scenario %in% names(labels)) {
    warning("Unknown scenario: ", scenario, " — using raw name.")
    return(scenario)
  }
  
  labels[[scenario]]
}


get_dirs <- function(scenario) {
  in_dir  <- file.path("data", "kernels", scenario)
  out_dir <- file.path("results", scenario)
  if (!dir.exists(in_dir)) stop("Scenario input folder not found: ", in_dir)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  list(in_dir = in_dir, out_dir = out_dir)
}


# ---------------------------------------------------------------------------
# Drift baseline
# ---------------------------------------------------------------------------
# The drift vectors in Figure 6a and Supplementary Figures S4 and S5 measure an
# age group's final position against its baseline position, which is the
# average of its per-period positions.
#
# This exclusion applies to the DRIFT VECTORS ONLY. Figure 5 (the deviation
# heatmap), Figure 6b (the trajectory panel) and the path-complexity statistics
# in Supplementary Table S2 keep the full six-period average, because there the
# pandemic period is part of what is being displayed rather than part of the
# reference: you want to see the 2019-2021 deviation, not define it away.
# Applying the exclusion to step 4's baseline row propagates it to all of them
# and moves the path-complexity angles substantially.
#
# The 2019-2021 bin spans the COVID-19 restrictions. Including it in that
# average pulls the reference point toward a three-year anomaly and understates
# the drift, so it is excluded. The 2022-2024 endpoint is deliberately kept:
# removing the pandemic from the reference does not make the comparison
# pandemic-free, and Supplementary Table S4 is what addresses persistence.
#
# Every script that forms this average must use the same definition. Four did
# so independently before this was factored out, which is how the baseline came
# to be labelled "Baseline (2007-2009)" while actually being a six-period mean.
BASELINE_EXCLUDE <- c("2019-2021")

# DO NOT RENAME without regenerating Supplementary Table S2.
#
# This label is misleading: the row it names is the average of the per-period
# positions, not the 2007-2009 period. It is kept anyway because step 5 orders
# the sequential vector chain by parsing years out of the period label, so the
# embedded "2007-2009" is what places the baseline at the start of the
# trajectory. Renaming it to "Baseline (period average)" leaves nothing to
# parse, the baseline sorts to the end instead, and the path-complexity angles
# in Supplementary Table S2 change substantially (18-30 mean resultant length
# 0.583 -> 0.195).
#
# Two things are worth separating here. The label is cosmetic. The ordering
# behaviour behind it is not: it inserts a six-period average into the timeline
# at the position where the 2007-2009 period belongs, so the first segment of
# every path runs from an average rather than from the first observation. That
# is a question about Supplementary Table S2, not about the drift vectors, and
# is left alone here.
BASELINE_LABEL   <- "Baseline (2007-2009)"

# TRUE for period labels that contribute to the baseline average. Period labels
# reach us with either a hyphen or an en-dash, so key on the digits.
in_baseline <- function(period) {
  d <- gsub("[^0-9]", "", period)
  !(paste0(substr(d, 1, 4), "-", substr(d, 5, 8)) %in% BASELINE_EXCLUDE)
}


# Where scripts write the figure files the manuscript includes.
#
# Resolution order:
#   1. ACTIONSPACE_FIG_DIR, if set
#   2. ../Overleaf_source/figures, when this checkout sits beside the
#      manuscript source (the author's working copy)
#   3. results/figures_manuscript, otherwise
#
# Step 3 is what a fresh clone gets. Scripts previously hardcoded an absolute
# path into the author's OneDrive, so every manuscript figure failed to export
# on any other machine.
get_manuscript_fig_dir <- function() {
  fig_dir <- Sys.getenv("ACTIONSPACE_FIG_DIR", unset = "")

  if (!nzchar(fig_dir)) {
    overleaf <- file.path("..", "Overleaf_source", "figures")
    fig_dir  <- if (dir.exists(overleaf)) {
      overleaf
    } else {
      file.path("results", "figures_manuscript")
    }
  }

  if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
  fig_dir
}
