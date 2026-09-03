######################################################################
# check_drift_periods.R
#
# Verification utility (not a pipeline step).
#
# Recomputes the drift components (dx = fraction of day away, in
# percentage points; dy = mean distance when away, in km) that underlie
# main-text Figure 6a and Supplementary Figure S8, under the three
# constructions that appear in the R2 revision:
#
#   endpoint_pre : 2007-2009 -> 2016-2018
#                  Endpoint-to-endpoint over the pre-pandemic window.
#                  This is the comparison Reviewer 2 asked for in
#                  comment 2.5.
#
#   s8_window    : 2016-2018 relative to the mean of 2007-2018
#                  The construction step8_uncertainty_R2.R uses for
#                  Supplementary Figure S8.
#
#   fig6a_full   : 2022-2024 relative to the mean of 2007-2024
#                  The construction step8_uncertainty_R2.R uses for
#                  main-text Figure 6a.
#
# The window constructions measure an endpoint against the average of a
# window that contains it, which damps any monotone trend; the endpoint
# construction does not. The three columns are reported side by side so
# the difference is explicit.
#
# Point estimates only. Uncertainty comes from the session-level
# bootstrap in step8_uncertainty_R2.R.
#
# Usage (from the code/ directory):
#   Rscript code/check_drift_periods.R
#
# Input:  results/<scenario>/action_space_metrics_TRIPWEIGHTED.csv
# Output: results/<scenario>/drift_periods_comparison.csv
######################################################################

suppressPackageStartupMessages({
  library(dplyr)
})

source("code/utils_io.R")

SCENARIO   <- if (exists("scenario")) scenario else "baseline"
AGE_LEVELS <- c("10-17", "18-30", "31-55", "56-65", "66+")

PERIODS     <- c("2007-2009", "2010-2012", "2013-2015",
                 "2016-2018", "2019-2021", "2022-2024")
PRE_PERIODS <- PERIODS[1:4]

dirs    <- get_dirs(SCENARIO)
OUT_DIR <- dirs$out_dir

metrics_path <- file.path(OUT_DIR, "action_space_metrics_TRIPWEIGHTED.csv")
if (!file.exists(metrics_path)) {
  stop("Metrics file not found: ", metrics_path,
       "\nRun step4 for this scenario first.")
}

cat("\n=== DRIFT PERIOD COMPARISON (", SCENARIO, ") ===\n\n", sep = "")

# The Period column mixes hyphens and en-dashes depending on which script
# wrote it, so key on the two four-digit years rather than the raw string.
metrics <- read.csv(metrics_path, stringsAsFactors = FALSE) %>%
  filter(!grepl("^Baseline", Period)) %>%
  mutate(
    Period = vapply(
      regmatches(Period, gregexpr("[0-9]{4}", Period)),
      function(y) paste(y[1], y[2], sep = "-"),
      character(1)
    ),
    x = fraction_away * 100,      # percentage points
    y = mean_distance_active      # km
  ) %>%
  select(AgeGroup, Period, x, y)

missing <- setdiff(
  paste(rep(AGE_LEVELS, each = length(PERIODS)), PERIODS),
  paste(metrics$AgeGroup, metrics$Period)
)
if (length(missing)) stop("Missing age-period cells: ",
                          paste(missing, collapse = ", "))

get_cell <- function(age, period) {
  r <- metrics[metrics$AgeGroup == age & metrics$Period == period, ]
  c(x = r$x, y = r$y)
}

win_mean <- function(age, periods) {
  r <- metrics[metrics$AgeGroup == age & metrics$Period %in% periods, ]
  c(x = mean(r$x), y = mean(r$y))
}

out <- do.call(rbind, lapply(AGE_LEVELS, function(a) {
  p_start <- get_cell(a, "2007-2009")
  p_pre   <- get_cell(a, "2016-2018")
  p_end   <- get_cell(a, "2022-2024")
  b_pre   <- win_mean(a, PRE_PERIODS)
  b_full  <- win_mean(a, PERIODS)

  data.frame(
    AgeGroup          = a,
    endpoint_pre_dx   = p_pre["x"] - p_start["x"],
    endpoint_pre_dy   = p_pre["y"] - p_start["y"],
    s8_window_dx      = p_pre["x"] - b_pre["x"],
    s8_window_dy      = p_pre["y"] - b_pre["y"],
    fig6a_full_dx     = p_end["x"] - b_full["x"],
    fig6a_full_dy     = p_end["y"] - b_full["y"],
    row.names         = NULL,
    stringsAsFactors  = FALSE
  )
}))

num_cols <- setdiff(names(out), "AgeGroup")
out[num_cols] <- lapply(out[num_cols], function(v) round(v, 3))

print(out, row.names = FALSE)

cat("\ndx = change in fraction of day away from home (percentage points)",
    "\ndy = change in mean distance when away (km)\n")

csv_path <- file.path(OUT_DIR, "drift_periods_comparison.csv")
write.csv(out, csv_path, row.names = FALSE)
cat("\nWritten: ", csv_path, "\n\n", sep = "")
