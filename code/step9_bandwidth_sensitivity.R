######################################################################
# STEP 9 – BANDWIDTH SENSITIVITY ANALYSIS
#
# Compares drift vectors across three KDE distance bandwidths:
#   bw3 (H_D = 3 km), baseline (H_D = 5 km), bw7 (H_D = 7 km)
#
# Approach: re-derives the integral metrics from kde_yearbin_by_cohort.rds
#   directly (same computation as Step 4), so Steps 3–5 do not need to be
#   re-run for bw3 and bw7.
#
# Inputs:  code/data/kernels/bw3/kde_yearbin_by_cohort.rds
#          code/data/kernels/baseline/kde_yearbin_by_cohort.rds
#          code/data/kernels/bw7/kde_yearbin_by_cohort.rds
# Outputs: results/bandwidth_sensitivity/bw_trajectory.png
#          results/bandwidth_sensitivity/bw_drift_comparison.png
#          results/bandwidth_sensitivity/bw_drift_summary.csv
######################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

cat("\n=== STEP 9: Bandwidth Sensitivity Analysis ===\n\n")

# ===========================================================================
# PATHS
# ===========================================================================
root_dir   <- "."   # must be run from code/ directory
kernel_dir <- file.path(root_dir, "data", "kernels")
out_dir    <- file.path(root_dir, "results", "bandwidth_sensitivity")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

bandwidths <- list(
  bw3      = list(label = "H_D = 3 km", H_D = 3, path = file.path(kernel_dir, "bw3")),
  baseline = list(label = "H_D = 5 km (baseline)", H_D = 5, path = file.path(kernel_dir, "baseline")),
  bw7      = list(label = "H_D = 7 km", H_D = 7, path = file.path(kernel_dir, "bw7"))
)

# ===========================================================================
# HELPER: integral metrics from a KDE density data frame (same as Step 4)
# ===========================================================================
HOME_CUTOFF <- 0.02   # km — must match Step 2
dt_val <- 24 / 96     # hours per time bin
dd_val <- 60 / 96     # km per distance bin

compute_tripweighted_metrics <- function(df_density) {
  fraction_away_val <- sum(df_density$density[df_density$d > HOME_CUTOFF]) * dt_val * dd_val

  time_profiles <- df_density %>%
    group_by(t) %>%
    summarise(
      mean_d_active = {
        active  <- density[d > HOME_CUTOFF]
        d_activ <- d[d > HOME_CUTOFF]
        if (sum(active) > 0) weighted.mean(d_activ, w = active, na.rm = TRUE) else 0
      },
      .groups = "drop"
    )

  time_profiles %>%
    summarise(
      fraction_away        = fraction_away_val,
      mean_distance_active = mean(mean_d_active[mean_d_active > 0], na.rm = TRUE),
      .groups = "drop"
    )
}

# ===========================================================================
# LOAD AND COMPUTE FOR EACH BANDWIDTH
# ===========================================================================

all_metrics <- lapply(names(bandwidths), function(bw_name) {
  bw    <- bandwidths[[bw_name]]
  rds   <- file.path(bw$path, "kde_yearbin_by_cohort.rds")

  if (!file.exists(rds)) {
    stop("Missing kernel file for '", bw_name, "': ", rds,
         "\nRun step2_run_", bw_name, ".R then copy RDS files to ", bw$path)
  }

  cat("Loading", bw$label, "...\n")
  dat <- readRDS(rds)
  cat("  Rows:", nrow(dat), "\n")

  metrics <- dat %>%
    group_by(AgeGroup, YearBin) %>%
    group_modify(~ compute_tripweighted_metrics(.x)) %>%
    rename(Period = YearBin) %>%
    mutate(bandwidth = bw_name, bw_label = bw$label, H_D = bw$H_D)

  metrics
})

metrics_all <- bind_rows(all_metrics) %>% ungroup()

# Enforce consistent AgeGroup ordering
age_levels <- c("10-17", "18-30", "31-55", "56-65", "66+")
metrics_all <- metrics_all %>%
  mutate(AgeGroup = factor(AgeGroup, levels = age_levels))

# Normalize period dash encoding
normalize_dash <- function(x) gsub("[\u2012\u2013\u2014\u2212\u00e2\u0080\u0093]", "-", x, perl = TRUE)
metrics_all <- metrics_all %>%
  mutate(Period = normalize_dash(Period))

# Parse end year for ordering
extract_end_year <- function(x) as.numeric(sub(".*?(\\d{4})[^\\d]*$", "\\1", x))
metrics_all <- metrics_all %>%
  mutate(end_year = extract_end_year(Period)) %>%
  arrange(AgeGroup, bandwidth, end_year)

cat("\nMetrics computed. Periods found:", paste(sort(unique(metrics_all$Period)), collapse = ", "), "\n\n")

# ===========================================================================
# BASELINE: arithmetic mean over all periods (same as Step 4)
# ===========================================================================
baseline_metrics <- metrics_all %>%
  group_by(AgeGroup, bandwidth, bw_label, H_D) %>%
  summarise(
    baseline_mda = mean(mean_distance_active, na.rm = TRUE),
    .groups = "drop"
  )

# ===========================================================================
# COMPUTE DRIFT = period value - baseline
# ===========================================================================
drift_metrics <- metrics_all %>%
  left_join(baseline_metrics, by = c("AgeGroup", "bandwidth", "bw_label", "H_D")) %>%
  mutate(drift = mean_distance_active - baseline_mda)

# Summary: drift for the most recent period only
last_period_end <- max(metrics_all$end_year, na.rm = TRUE)
drift_final <- drift_metrics %>%
  filter(end_year == last_period_end)

cat("Drift in final period (", last_period_end - 2, "–", last_period_end, "):\n", sep = "")
print(drift_final %>% select(AgeGroup, bw_label, mean_distance_active, baseline_mda, drift))
cat("\n")

# ===========================================================================
# FIGURE 1: mean_distance_active trajectory over time by bandwidth
# ===========================================================================

bw_colours <- c(
  "bw3"      = "#E15759",
  "baseline" = "#4E79A7",
  "bw7"      = "#F28E2B"
)
bw_labels <- setNames(
  sapply(names(bandwidths), function(b) bandwidths[[b]]$label),
  names(bandwidths)
)

# Period labels: use numeric start year — avoids dash-encoding differences across RDS files
yr_start <- function(x) as.integer(regmatches(x, regexpr("\\d{4}", x)))
metrics_all <- metrics_all %>%
  mutate(yr_start = vapply(Period, yr_start, integer(1)))

period_levels <- metrics_all %>%
  distinct(yr_start, end_year) %>%
  arrange(end_year) %>%
  pull(yr_start) %>%
  as.character()

metrics_plot <- metrics_all %>%
  mutate(
    period_label = factor(as.character(yr_start), levels = period_levels),
    bandwidth    = factor(bandwidth, levels = names(bw_colours))
  )

p1 <- ggplot(metrics_plot,
             aes(x = period_label, y = mean_distance_active,
                 colour = bandwidth, group = bandwidth)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ AgeGroup, nrow = 1, scales = "free_y") +
  scale_colour_manual(values = bw_colours, labels = bw_labels, name = "Bandwidth") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(
    title    = "Action-space mean distance (active periods) by bandwidth",
    subtitle = "Three KDE distance bandwidths: H_D = 3, 5, 7 km",
    x        = "Period start year",
    y        = "Mean distance when away (km)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(out_dir, "bw_trajectory.png"), p1,
       width = 14, height = 5, dpi = 300)
cat("Saved: bw_trajectory.png\n")

# ===========================================================================
# FIGURE 2: drift in each year-bin, all bandwidths, faceted by age group
# ===========================================================================

drift_plot <- drift_metrics %>%
  mutate(
    yr_start     = vapply(Period, yr_start, integer(1)),
    period_label = factor(as.character(yr_start), levels = period_levels),
    bandwidth    = factor(bandwidth, levels = names(bw_colours))
  )

p2 <- ggplot(drift_plot,
             aes(x = period_label, y = drift,
                 colour = bandwidth, group = bandwidth)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ AgeGroup, nrow = 1) +
  scale_colour_manual(values = bw_colours, labels = bw_labels, name = "Bandwidth") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(
    title    = "Drift from mean baseline by bandwidth",
    subtitle = "Drift = period mean_distance_active − arithmetic mean across periods",
    x        = "Period start year",
    y        = "Drift in mean away-distance (km)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(out_dir, "bw_drift_comparison.png"), p2,
       width = 14, height = 5, dpi = 300)
cat("Saved: bw_drift_comparison.png\n")

# ===========================================================================
# FIGURE 3: final-period drift bars – side-by-side by bandwidth
# ===========================================================================

p3 <- ggplot(drift_final,
             aes(x = AgeGroup, y = drift, fill = bandwidth)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
  scale_fill_manual(values = bw_colours, labels = bw_labels, name = "Bandwidth") +
  labs(
    title    = paste0("Final-period drift by age group and bandwidth (", last_period_end - 2,
                      "\u20132022\u20132024)"),
    subtitle = "Drift = mean_distance_active(final period) − arithmetic mean baseline",
    x        = "Age group",
    y        = "Drift (km)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(out_dir, "bw_drift_bars.png"), p3,
       width = 9, height = 5, dpi = 300)
cat("Saved: bw_drift_bars.png\n")

# ===========================================================================
# CSV EXPORT
# ===========================================================================
drift_summary <- drift_metrics %>%
  select(AgeGroup, Period, end_year, bandwidth, H_D, bw_label,
         mean_distance_active, baseline_mda, drift)

write.csv(drift_summary, file.path(out_dir, "bw_drift_summary.csv"), row.names = FALSE)
cat("Saved: bw_drift_summary.csv\n")

# ===========================================================================
# CONSOLE SUMMARY
# ===========================================================================
cat("\n=== DRIFT SUMMARY (final period) ===\n")
drift_final_wide <- drift_final %>%
  select(AgeGroup, bandwidth, drift) %>%
  pivot_wider(names_from = bandwidth, values_from = drift)
print(drift_final_wide)

cat("\n=== STEP 9 COMPLETE ===\n")
cat("Outputs in: ", out_dir, "\n")
