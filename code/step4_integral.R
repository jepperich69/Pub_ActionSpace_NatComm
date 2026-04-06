######################################################################
# STEP 4 – ACTION-SPACE INTEGRAL METRICS (TRIP-WEIGHTED)
# REVISED VERSION: Uses arithmetic mean of periods as baseline
# 
# KEY CHANGE: Instead of using the pooled KDE baseline from Step 2,
# we now compute baseline as the arithmetic mean of the six 3-year
# period values for each age group. This makes the baseline directly
# interpretable from the visible data points.
######################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(scales)

cat("\n=== STEP 4 REVISED: ARITHMETIC MEAN BASELINE ===\n\n")

# ============================================================================
# PATHS
# ============================================================================

# =======================
# I/O: folder-agnostic
# =======================
if (!exists("in_dir") || !exists("out_dir")) {
  source("code/utils_io.R")
  scenario <- if (exists("scenario")) scenario else "baseline"
  dirs <- get_dirs(scenario)
  in_dir  <- dirs$in_dir
  out_dir <- dirs$out_dir
}

input_dir  <- in_dir
output_dir <- out_dir

cat("Loading from: ", input_dir, "\n")
cat("Saving to:    ", output_dir, "\n\n")

# ============================================================================
# CRITICAL: HOME CUTOFF MUST MATCH STEP 2
# ============================================================================
HOME_CUTOFF <- 0.02  # km - matches Step 2's HOME_ZERO_CUTOFF

cat("Using HOME_CUTOFF = ", HOME_CUTOFF, " km (must match Step 2)\n\n")

# ============================================================================
# LOAD KDE DENSITIES FROM STEP 2
# ============================================================================

cat("Loading KDE densities from Step 2...\n")

# NOTE: We now ONLY need the year-bin densities, not the pooled baseline
yearbin_densities_did <- readRDS(file.path(input_dir, "kde_yearbin_by_cohort.rds"))

cat("  ✓ Loaded year-bin KDE (", nrow(yearbin_densities_did), "rows)\n\n")

# ============================================================================
# COMPUTE TRIP-WEIGHTED METRICS
# ============================================================================

cat("Computing trip-weighted action-space metrics...\n\n")

compute_tripweighted_metrics <- function(df_density) {
  # df_density should have columns: d, t, density
  # NOTE: When using group_modify, grouping columns are NOT in df_density
  
  dt_val <- 24 / 96  # 0.25 hours
  dd_val <- 60 / 96  # ~0.625 km
  
  # Compute fraction_away by direct integration over (t,d) space
  fraction_away_val <- sum(df_density$density[df_density$d > HOME_CUTOFF]) * dt_val * dd_val
  
  # For each TIME point, compute the mean distance (spatial average)
  time_profiles <- df_density %>%
    group_by(t) %>%
    summarise(
      # Mean distance at this time
      mean_d = weighted.mean(d, w = density, na.rm = TRUE),
      
      # Mean distance when away (d > HOME_CUTOFF)
      mean_d_active = {
        active <- density[d > HOME_CUTOFF]
        d_active <- d[d > HOME_CUTOFF]
        if(sum(active) > 0) {
          weighted.mean(d_active, w = active, na.rm = TRUE)
        } else {
          0
        }
      },
      
      .groups = "drop"
    )
  
  # Compute summary metrics
  time_profiles %>%
    summarise(
      fraction_away = fraction_away_val,
      mean_distance = mean(mean_d),
      mean_distance_active = mean(mean_d_active[mean_d_active > 0], na.rm = TRUE),
      volume = sum(mean_d) * dt_val,  # km·hours per day
      dist_p90 = quantile(mean_d_active[mean_d_active > 0], 0.90, na.rm = TRUE),
      var_distance = var(mean_d),
      .groups = "drop"
    )
}

# ============================================================================
# APPLY TO YEAR-BINS (ALL PERIODS)
# ============================================================================

cat("Computing year-bin metrics for all periods...\n")

yearbin_metrics <- yearbin_densities_did %>%
  group_by(AgeGroup, YearBin) %>%
  group_modify(~ compute_tripweighted_metrics(.x)) %>%
  rename(Period = YearBin)

cat("  ✓ Year-bin metrics (", nrow(yearbin_metrics), " rows)\n")
print(head(yearbin_metrics, 12))
cat("\n")

# ============================================================================
# NEW APPROACH: COMPUTE BASELINE AS ARITHMETIC MEAN OF PERIODS
# ============================================================================

cat("Computing baseline as arithmetic mean across periods...\n")

baseline_metrics_mean <- yearbin_metrics %>%
  group_by(AgeGroup) %>%
  summarise(
    fraction_away = mean(fraction_away),
    mean_distance = mean(mean_distance),
    mean_distance_active = mean(mean_distance_active),
    volume = mean(volume),
    dist_p90 = mean(dist_p90),
    var_distance = mean(var_distance),
    .groups = "drop"
  ) %>%
  mutate(Period = "Baseline (2007-2009)")

cat("  ✓ Baseline metrics computed from period averages:\n")
print(baseline_metrics_mean)
cat("\n")

cat("COMPARISON: This baseline is now the arithmetic mean of the six period values,\n")
cat("            NOT the pooled KDE baseline from Step 2.\n\n")

# ============================================================================
# COMPUTE CHANGES FROM NEW BASELINE
# ============================================================================

cat("Computing changes from arithmetic mean baseline...\n")

metric_changes <- yearbin_metrics %>%
  left_join(
    baseline_metrics_mean %>% 
      dplyr::select(-Period) %>%
      rename_with(~ paste0(., "_baseline"), -AgeGroup),
    by = "AgeGroup"
  ) %>%
  mutate(
    change_fraction_away = fraction_away - fraction_away_baseline,
    change_mean_distance_active = mean_distance_active - mean_distance_active_baseline,
    change_volume = volume - volume_baseline,
    pct_change_volume = 100 * (volume - volume_baseline) / volume_baseline,
    pct_change_distance = 100 * (mean_distance_active - mean_distance_active_baseline) / mean_distance_active_baseline
  )

cat("  ✓ Changes computed (", nrow(metric_changes), " rows)\n")
cat("\nSample of percentage changes:\n")
print(metric_changes %>% 
        dplyr::select(AgeGroup, Period, mean_distance_active, 
                      mean_distance_active_baseline, pct_change_distance) %>%
        arrange(AgeGroup, Period))
cat("\n")

# Verify that changes sum to approximately zero for each age group
change_sums <- metric_changes %>%
  group_by(AgeGroup) %>%
  summarise(
    sum_pct_change = sum(pct_change_distance),
    sum_absolute_change = sum(change_mean_distance_active),
    .groups = "drop"
  )

cat("VALIDATION: Sum of percentage changes by age group (should be ≈ 0):\n")
print(change_sums)
cat("\n")

# ============================================================================
# VISUALIZATIONS
# ============================================================================

cat("Creating updated plots...\n")

# Combine for plotting
all_metrics <- bind_rows(baseline_metrics_mean, yearbin_metrics)

# Plot 1: Mean distance trend (UPDATED)
p_distance_trend <- yearbin_metrics %>%
  ggplot(aes(x = Period, y = mean_distance_active, color = AgeGroup, group = AgeGroup)) +
  geom_hline(data = baseline_metrics_mean, 
             aes(yintercept = mean_distance_active, color = AgeGroup),
             linetype = "dashed", linewidth = 0.8) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_viridis_d(option = "D") +
  labs(title = "Mean Distance When Away Over Time",
       subtitle = "Trip-weighted metric. Dashed lines = arithmetic mean across periods",
       x = "Survey Period", 
       y = "Mean Distance When Away (km)",
       color = "Age Group") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

ggsave(file.path(output_dir, "mean_distance_trend_TRIPWEIGHTED.png"), 
       p_distance_trend, width = 10, height = 7, dpi = 300)
cat("  ✓ mean_distance_trend_TRIPWEIGHTED_REVISED.png\n")

# Plot 2: Trajectory plot (UPDATED)
p_trajectory <- yearbin_metrics %>%
  ggplot(aes(x = fraction_away, y = mean_distance_active, color = AgeGroup)) +
  geom_point(data = baseline_metrics_mean, size = 5, shape = 18) +
  geom_path(aes(group = AgeGroup), linewidth = 0.8, alpha = 0.5) +
  geom_point(aes(shape = Period), size = 3) +
  scale_color_viridis_d(option = "D") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "Action-Space Trajectory (Trip-Weighted)",
       subtitle = "Diamond = arithmetic mean baseline, path shows evolution",
       x = "Fraction of Day Away from Home", 
       y = "Mean Distance When Away (km)",
       color = "Age Group", shape = "Period") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

ggsave(file.path(output_dir, "action_space_trajectory_TRIPWEIGHTED.png"), 
       p_trajectory, width = 10, height = 8, dpi = 300)
cat("  ✓ action_space_trajectory_TRIPWEIGHTED.png\n")

# Plot 3: Decomposition (UPDATED)
p_decomposition <- metric_changes %>%
  dplyr::select(AgeGroup, Period, 
                `Time away` = change_fraction_away,
                `Distance when away` = change_mean_distance_active) %>%
  pivot_longer(cols = c(`Time away`, `Distance when away`),
               names_to = "Component", values_to = "Change") %>%
  ggplot(aes(x = Period, y = Change, fill = Component)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ AgeGroup, ncol = 2, scales = "free_y") +
  scale_fill_viridis_d(option = "C", begin = 0.3, end = 0.8) +
  labs(title = "Mobility Decomposition (Trip-Weighted)",
       subtitle = "Changes from arithmetic mean baseline",
       x = "Survey Period", y = "Change from Baseline",
       fill = "Component") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

ggsave(file.path(output_dir, "mobility_decomposition_TRIPWEIGHTED.png"), 
       p_decomposition, width = 12, height = 10, dpi = 300)
cat("  ✓ mobility_decomposition_TRIPWEIGHTED_REVISED.png\n")

# Plot 4: Heatmap (UPDATED - THIS IS THE KEY CHANGE)
p_heatmap <- metric_changes %>%
  ggplot(aes(x = Period, y = AgeGroup, fill = pct_change_distance)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%+.1f%%", pct_change_distance)), 
            size = 3.5, fontface = "bold") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, name = "% Change",
                       limits = c(-30, 30)) +
  labs(title = "% Change in Mean Distance When Away",
       subtitle = "Relative to period-averaged baseline (arithmetic mean). Blue = shorter, Red = longer",
       x = "Survey Period", y = "Age Group") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

ggsave(file.path(output_dir, "distance_change_heatmap_TRIPWEIGHTED.png"), 
       p_heatmap, width = 10, height = 7, dpi = 300)
cat("  ✓ distance_change_heatmap_TRIPWEIGHTED_REVISED.png\n\n")

# ============================================================================
# SUMMARY TABLE
# ============================================================================

cat("Creating summary table...\n")

summary_table <- yearbin_metrics %>%
  left_join(
    baseline_metrics_mean %>% 
      dplyr::select(AgeGroup, baseline_dist = mean_distance_active),
    by = "AgeGroup"
  ) %>%
  group_by(AgeGroup) %>%
  summarise(
    baseline_dist = first(baseline_dist),
    mean_dist = mean(mean_distance_active),
    min_dist = min(mean_distance_active),
    max_dist = max(mean_distance_active),
    range_dist = max_dist - min_dist,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

cat("\n=== Summary: Trip-weighted distance by age group ===\n")
print(summary_table)
cat("\n")

# ============================================================================
# SAVE DATA TABLES
# ============================================================================

cat("Saving data tables...\n")

write.csv(all_metrics, 
          file.path(output_dir, "action_space_metrics_TRIPWEIGHTED.csv"),
          row.names = FALSE)
cat("  ✓ action_space_metrics_TRIPWEIGHTED.csv\n")

write.csv(metric_changes,
          file.path(output_dir, "action_space_changes_TRIPWEIGHTED.csv"),
          row.names = FALSE)
cat("  ✓ action_space_changes_TRIPWEIGHTED_REVISED.csv\n\n")

# ============================================================================
# COMPARISON WITH ORIGINAL APPROACH
# ============================================================================

cat(rep("=", 70), "\n", sep="")
cat("KEY DIFFERENCES FROM ORIGINAL VERSION:\n")
cat(rep("=", 70), "\n\n", sep="")

cat("ORIGINAL APPROACH:\n")
cat("  • Baseline = pooled KDE on all 2007-2024 data\n")
cat("  • Dashed lines ≠ arithmetic mean of visible points\n")
cat("  • Heatmap values relative to pooled baseline\n\n")

cat("REVISED APPROACH:\n")
cat("  • Baseline = arithmetic mean of six period values\n")
cat("  • Dashed lines = arithmetic mean of visible points\n")
cat("  • Heatmap values relative to arithmetic mean\n")
cat("  • Changes now sum to ≈0 for each age group\n\n")

cat("VISUAL CONSISTENCY:\n")
cat("  • Line plot and heatmap now use SAME baseline definition\n")
cat("  • Dashed lines directly interpretable from data\n")
cat("  • Percentage changes balanced around zero\n\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat(rep("=", 70), "\n", sep="")
cat("STEP 4 REVISED COMPLETE - ARITHMETIC MEAN BASELINE\n")
cat(rep("=", 70), "\n\n", sep="")

cat("NEW plots created (with _REVISED suffix):\n")
cat("  1. mean_distance_trend_TRIPWEIGHTED_REVISED.png\n")
cat("  2. action_space_trajectory_TRIPWEIGHTED_REVISED.png\n")
cat("  3. mobility_decomposition_TRIPWEIGHTED_REVISED.png\n")
cat("  4. distance_change_heatmap_TRIPWEIGHTED_REVISED.png\n\n")

cat("NEW data tables (with _REVISED suffix):\n")
cat("  • action_space_metrics_TRIPWEIGHTED.csv\n")
cat("  • action_space_changes_TRIPWEIGHTED.csv\n\n")

cat("NEXT STEPS:\n")
cat("  1. Compare _REVISED.png files with originals\n")
cat("  2. Verify that heatmap values now make sense\n")
cat("  3. Check that dashed lines in trend plot equal arithmetic mean\n")
cat("  4. Update manuscript captions accordingly\n\n")

cat("All outputs saved to: ", output_dir, "\n\n")

cat(rep("=", 70), "\n", sep="")
######################################################################