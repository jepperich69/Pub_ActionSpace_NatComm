# Required libraries
suppressPackageStartupMessages({
  library(ggplot2)
  library(viridis)
  library(scales)
  library(dplyr)
  library(tidyr)
  library(MASS)   # for kde2d
  library(grid)   # for unit() in arrow()
})

# Resolve namespace conflicts explicitly
select <- dplyr::select
filter <- dplyr::filter

######################################################################
# STEP 5 - DRIFT VECTORS WITH COMBINED BASELINE (FIXED)
# Showing evolution from baseline (2007-2009) to most recent period
# FIX: Use mean_distance_active consistently for both KDE and arrows
######################################################################

cat("\n=== Creating drift vector visualization with COMBINED BASELINE ===\n\n")

# Required libraries
suppressPackageStartupMessages({
  library(ggplot2)
  library(viridis)
  library(scales)
  library(dplyr)
  library(tidyr)
  library(MASS)   # for kde2d
  library(grid)   # for unit() in arrow()
})

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

cat("Kernel inputs from: ", input_dir, "\n")
cat("Step outputs to:     ", output_dir, "\n\n")

# ============================================================================
# LOAD STEP 4 RESULTS (use TRIPWEIGHTED file you created in Step 4)
# ============================================================================
cat("\n=== LOADING STEP 4 RESULTS ===\n")

step4_file <- file.path(output_dir, "action_space_metrics_TRIPWEIGHTED.csv")
if (!file.exists(step4_file)) {
  stop("Error: 'action_space_metrics_TRIPWEIGHTED.csv' not found at: ", step4_file,
       "\nRun Step 4 first, or check the output_dir above.")
}

all_volume_metrics <- read.csv(step4_file, check.names = FALSE)


# After: all_volume_metrics <- read.csv(step4_file, check.names = FALSE)

# --- Helpers to normalize Period strings and parse years ---------------------
normalize_dash <- function(x) {
  # convert figure/minus/en/em dashes to ASCII hyphen
  gsub("[\u2012\u2013\u2014\u2212]", "-", x, perl = TRUE)
}
extract_end_year <- function(x) {
  x2 <- normalize_dash(x)
  as.numeric(sub(".*?(\\d{4})[^\\d]*$", "\\1", x2))
}

# Harmonize columns and normalize Period labels
all_volume_metrics <- all_volume_metrics %>%
  mutate(
    Period = normalize_dash(Period),
    Period = case_when(
      Period %in% c("Baseline", "Baseline (2007-2009)", "2007-2009") ~ "Baseline (2007-2009)",
      TRUE ~ Period
    )
  )

cat("âœ“ Loaded Step 4 results\n")
cat("Available periods: ", paste(unique(all_volume_metrics$Period), collapse = ", "), "\n\n")

# --- AUTO-SCALE TO KILOMETERS (guards against underscaled units) -------------
baseline_probe <- all_volume_metrics %>%
  filter(Period == "Baseline (2007-2009)")

if (median(baseline_probe$mean_distance_active, na.rm = TRUE) < 3) {
  scale_factor <- 10
  message(sprintf("âš  Detected underscaled distances. Multiplying by %g to convert to km.", scale_factor))
  all_volume_metrics <- all_volume_metrics %>%
    mutate(mean_distance_active = mean_distance_active * scale_factor)
}

# ============================================================================
# PREPARE DATA FOR DRIFT ANALYSIS
# ============================================================================
baseline_data <- all_volume_metrics %>%
  filter(Period == "Baseline (2007-2009)")

yearbin_data <- all_volume_metrics %>%
  filter(Period != "Baseline (2007-2009)")

if (nrow(baseline_data) == 0) {
  stop("No rows found for Period == 'Baseline (2007-2009)'. Check Step 4 output.")
}

# --- Periods and their order -----------------------
period_levels_chk <- yearbin_data %>%
  distinct(Period) %>%
  mutate(end_year = extract_end_year(Period)) %>%
  arrange(end_year)

if (any(is.na(period_levels_chk$end_year))) {
  stop("Some Period labels failed to parse: ",
       paste(period_levels_chk$Period[is.na(period_levels_chk$end_year)], collapse = ", "))
}

print(period_levels_chk)

cat("Baseline period: Baseline (2007-2009)\n")
cat("Year-bin periods (ordered): ", paste(period_levels_chk$Period, collapse = ", "), "\n\n")

# ============================================================================
# STEP 1: 2D KDE background using mean_distance_active
# ============================================================================
x_vals <- all_volume_metrics$fraction_away
y_vals <- all_volume_metrics$mean_distance_active

fraction_seq <- seq(min(x_vals, na.rm = TRUE) - 0.01,
                    max(x_vals, na.rm = TRUE) + 0.01, length.out = 100)
distance_seq <- seq(min(y_vals, na.rm = TRUE) - 0.5,
                    max(y_vals, na.rm = TRUE) + 0.5, length.out = 100)

kde_2d <- MASS::kde2d(
  x = x_vals,
  y = y_vals,
  n = 100,
  lims = c(range(fraction_seq), range(distance_seq))
)

kde_df <- expand.grid(
  fraction_away = kde_2d$x,
  mean_distance_active = kde_2d$y
) %>%
  mutate(density = as.vector(kde_2d$z))

# ============================================================================
# STEP 2: Build ONE centroid per (AgeGroup, Period) and drift vectors
# FIX: Use mean_distance_active consistently!
# ============================================================================

# 2.1 Ensure exactly 1 row per (AgeGroup, Period)
centroids <- all_volume_metrics %>%
  group_by(AgeGroup, Period) %>%
  summarise(
    fraction_away = mean(fraction_away, na.rm = TRUE),
    mean_distance_active = mean(mean_distance_active, na.rm = TRUE),  # FIXED: was mean_distance
    .groups = "drop"
  )

# Defensive check
dup_check <- centroids %>% count(AgeGroup, Period)
stopifnot(all(dup_check$n == 1))

# 2.2 Order periods and pick base/latest
period_levels <- centroids %>%
  filter(Period != "Baseline (2007-2009)") %>%
  distinct(Period) %>%
  mutate(end_year = extract_end_year(Period)) %>%
  arrange(end_year) %>%
  pull(Period)

most_recent_period <- tail(period_levels, 1L)

# 2.3 Compute baseâ†’latest endpoints using mean_distance_active
start_data <- centroids %>%
  filter(Period == "Baseline (2007-2009)") %>%
  transmute(AgeGroup, x_start = fraction_away, y_start = mean_distance_active)  # FIXED

end_data <- centroids %>%
  filter(Period == most_recent_period) %>%
  transmute(AgeGroup, x_end = fraction_away, y_end = mean_distance_active)  # FIXED

# 2.4 Join and compute drift vectors
drift_vectors <- start_data %>%
  inner_join(end_data, by = "AgeGroup") %>%
  mutate(
    dx = x_end - x_start,
    dy = y_end - y_start,
    magnitude = sqrt(dx^2 + dy^2),
    angle = atan2(dy, dx) * 180 / pi,
    pct_change_distance = 100 * dy / y_start,
    pct_change_time     = 100 * dx / x_start
  )

cat("\n=== DRIFT VECTOR CALCULATIONS ===\n")
print(drift_vectors)

# ============================================================================
# STEP 3: MAIN visualization (clean arrows with labels)
# ============================================================================
start_label <- "2007-09"
end_year <- gsub(".*-", "", most_recent_period)

p_drift_main <- ggplot() +
  geom_raster(data = kde_df,
              aes(x = fraction_away, y = mean_distance_active, fill = density),
              interpolate = TRUE, alpha = 0.8) +
  scale_fill_viridis_c(option = "inferno", trans = "sqrt", name = "Density") +
  geom_contour(data = kde_df,
               aes(x = fraction_away, y = mean_distance_active, z = density),
               color = "gray", alpha = 0.4, bins = 6, linewidth = 0.3) +
  geom_segment(data = drift_vectors,
               aes(x = x_start, y = y_start, 
                   xend = x_end, yend = y_end,
                   color = AgeGroup),
               arrow = arrow(type = "closed", length = unit(0.13, "in"), angle = 20),
               linewidth = 1, alpha = 0.95, lineend = "round") +
  geom_point(data = drift_vectors,
             aes(x = x_start, y = y_start, color = AgeGroup),
             size = 2, shape = 1, alpha = 0.7) +
  geom_point(data = drift_vectors,
             aes(x = x_end, y = y_end, color = AgeGroup),
             size = 2, shape = 16) +
  geom_text(data = drift_vectors,
            aes(x = x_end, y = y_end, label = AgeGroup),
            color = "gray50", fontface = "bold", size = 4.5,
            nudge_y = 0.25, show.legend = FALSE) +
  geom_text(data = drift_vectors,
            aes(x = x_start, y = y_start, label = start_label),
            color = "blue", size = 2.5, alpha = 0.7,
            nudge_y = -0.2) +
  scale_color_viridis_d(option = "D", name = "Age Group") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = sprintf("Action-Space Evolution: Baseline (%s) to (%s)", start_label, end_year),
       subtitle = "Arrows show direction and magnitude of change from combined baseline",
       x = "Fraction of Day Away from Home", 
       y = "Mean Distance When Away (km)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 11))

print(p_drift_main)

# ============================================================================
# STEP 4: MINIMAL version - Arrows only
# ============================================================================

p_drift_minimal <- ggplot() +
  geom_raster(data = kde_df,
              aes(x = fraction_away, y = mean_distance_active, fill = density),
              interpolate = TRUE, alpha = 0.8) +
  scale_fill_viridis_c(option = "inferno", trans = "sqrt", guide = "none") +
  geom_contour(data = kde_df,
               aes(x = fraction_away, y = mean_distance_active, z = density),
               color = "gray10", alpha = 0.3, bins = 5, linewidth = 0.3) +
  geom_segment(data = drift_vectors,
               aes(x = x_start, y = y_start,
                   xend = x_end, yend = y_end,
                   color = AgeGroup),
               arrow = arrow(type = "closed", length = unit(0.13, "in"), angle = 20),
               linewidth = 2, alpha = 0.95, lineend = "round") +
  geom_text(data = drift_vectors,
            aes(x = x_end, y = y_end, label = AgeGroup),
            color = "gray50", fontface = "bold", size = 5,
            nudge_y = 0.25, show.legend = FALSE) +
  scale_color_viridis_d(option = "D", name = "Age Group") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(
    title = paste0(
      "Long-term End-point Mobility Drift (2007 to 2024): ",
      scenario_label
    ),
    x = "Fraction of Day Away from Home",
    y = "Mean Distance When Away (km)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),  # Centered title
    
    # Legend at center bottom, inside plot
    legend.position = c(0.5, 0.08),
    legend.justification = c("center", "bottom"),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    
    legend.background = element_rect(fill = alpha("white", 0.85), 
                                     color = "gray30", linewidth = 0.5),
    legend.margin = margin(8, 12, 8, 12),
    
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(0.6, "cm"),
    
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  )

print(p_drift_minimal)

ggsave(file.path(output_dir, "drift_vectors_MINIMAL_COMBINED.png"), 
       p_drift_minimal, width = 11, height = 8, dpi = 300)
# ============================================================================
# STEP 5: Trajectories + drift
# ============================================================================

# Prepare trajectory data
df_traj <- all_volume_metrics %>%
  mutate(
    Period = normalize_dash(Period),
    Period = case_when(
      Period == "Baseline" ~ "Baseline (2007-2009)",
      TRUE ~ Period
    ),
    x = fraction_away,
    y = mean_distance_active  # FIXED: consistent variable
  )

# Get period ordering
baseline_row <- df_traj %>% filter(Period == "Baseline (2007-2009)") %>% slice(1)
baseline_numeric <- if (nrow(baseline_row) > 0) 2008 else NA

periods_ordered <- df_traj %>%
  mutate(
    Year_numeric = if_else(Period == "Baseline (2007-2009)", baseline_numeric,
                           extract_end_year(Period))
  ) %>%
  distinct(Period, Year_numeric) %>%
  arrange(Year_numeric)

df_traj$Period <- factor(df_traj$Period, levels = periods_ordered$Period)
df_traj$AgeGroup <- factor(df_traj$AgeGroup,
                           levels = c("10-17","18-30","31-55","56-65","66+"))

# Create full trajectory plot
p_trajectories <- ggplot() +
  geom_raster(data = kde_df,
              aes(x = fraction_away, y = mean_distance_active, fill = density),
              interpolate = TRUE, alpha = 0.7) +
  scale_fill_viridis_c(option = "inferno", trans = "sqrt", guide = "none") +
  geom_path(data = df_traj,
            aes(x = x, y = y, color = AgeGroup, group = AgeGroup),
            linewidth = 1.3, alpha = 0.8) +
  geom_point(data = df_traj %>% filter(Period == "Baseline (2007-2009)"),
             aes(x = x, y = y, color = AgeGroup),
             size = 4, shape = 23, fill = "gray", stroke = 1.5) +
  geom_point(data = df_traj %>% filter(Period != "Baseline (2007-2009)"),
             aes(x = x, y = y, color = AgeGroup, shape = Period),
             size = 3, alpha = 0.9) +
  geom_segment(data = drift_vectors,
               aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = AgeGroup),
               arrow = arrow(type = "closed", length = unit(0.1, "in")),
               linewidth = 1.2, alpha = 0.6, linetype = "dashed") +
  scale_color_viridis_d(option = "D", name = "Age Group") +
  scale_shape_manual(values = c(15, 17, 18, 19, 8, 4), name = "Period") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "Complete Mobility Trajectory with Drift Vectors",
       subtitle = "Diamond = baseline, path shows evolution, dashed arrow = overall drift",
       x = "Fraction of Day Away from Home",
       y = "Mean Distance When Away (km)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"))


# ============================================================================
# OPTIONAL: TRAJECTORY-AVERAGED DRIFT VECTORS
# Calculate drift as average of sequential period-to-period vectors
# ============================================================================
cat("\n=== CALCULATING TRAJECTORY-AVERAGED DRIFT VECTORS ===\n\n")

# Get all periods ordered by year
all_periods_ordered <- centroids %>%
  dplyr::distinct(Period) %>%
  dplyr::mutate(end_year = extract_end_year(Period)) %>%
  dplyr::arrange(end_year) %>%
  dplyr::pull(Period)

cat("All periods (ordered):", paste(all_periods_ordered, collapse = " -> "), "\n\n")

# Calculate sequential period-to-period vectors for each age group
sequential_vectors <- list()

for (i in 1:(length(all_periods_ordered) - 1)) {
  period_from <- all_periods_ordered[i]
  period_to <- all_periods_ordered[i + 1]
  
  cat(sprintf("Computing vector: %s -> %s\n", period_from, period_to))
  
  start_temp <- centroids %>%
    dplyr::filter(Period == period_from) %>%
    dplyr::transmute(AgeGroup, x_start = fraction_away, y_start = mean_distance_active)
  
  end_temp <- centroids %>%
    dplyr::filter(Period == period_to) %>%
    dplyr::transmute(AgeGroup, x_end = fraction_away, y_end = mean_distance_active)
  
  vectors_temp <- dplyr::inner_join(start_temp, end_temp, by = "AgeGroup") %>%
    dplyr::mutate(
      dx = x_end - x_start,
      dy = y_end - y_start,
      period_transition = paste(period_from, "->", period_to)
    )
  
  sequential_vectors[[i]] <- vectors_temp
}

# Combine all sequential vectors
all_sequential <- dplyr::bind_rows(sequential_vectors)

# Calculate average drift vector for each age group
trajectory_drift <- all_sequential %>%
  dplyr::group_by(AgeGroup) %>%
  dplyr::summarise(
    dx_avg = mean(dx, na.rm = TRUE),
    dy_avg = mean(dy, na.rm = TRUE),
    n_transitions = n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    magnitude = sqrt(dx_avg^2 + dy_avg^2),
    angle = atan2(dy_avg, dx_avg) * 180 / pi
  )

# Get baseline positions for arrow start points
baseline_positions <- centroids %>%
  dplyr::filter(Period == "Baseline (2007-2009)") %>%
  dplyr::transmute(AgeGroup, x_start = fraction_away, y_start = mean_distance_active)

# Create endpoint positions using averaged drift
# IMPORTANT: Explicitly select all columns to ensure angle and magnitude are preserved
trajectory_drift_vectors <- trajectory_drift %>%
  dplyr::inner_join(baseline_positions, by = "AgeGroup") %>%
  dplyr::mutate(
    x_end = x_start + dx_avg,
    y_end = y_start + dy_avg,
    # Calculate percentage changes
    pct_change_time = (dx_avg / x_start) * 100,
    pct_change_distance = (dy_avg / y_start) * 100
  ) %>%
  dplyr::select(AgeGroup, dx_avg, dy_avg, magnitude, angle, n_transitions,
                x_start, y_start, x_end, y_end, pct_change_time, pct_change_distance)

cat("\nTrajectory-averaged drift vectors:\n")
print(trajectory_drift_vectors %>% 
        dplyr::select(AgeGroup, dx_avg, dy_avg, magnitude, angle, n_transitions))

# ============================================================================
# PLOT: Trajectory-averaged drift visualization
# ============================================================================
cat("\nCreating trajectory-averaged drift visualization...\n")

start_label <- "2007-09"
end_year <- extract_end_year(most_recent_period)

p_drift_trajectory_avg <- ggplot() +
  # KDE background
  geom_raster(data = kde_df,
              aes(x = fraction_away, y = mean_distance_active, fill = density),
              interpolate = TRUE, alpha = 0.7) +
  scale_fill_viridis_c(option = "inferno", trans = "sqrt", guide = "none") +
  
  # Drift vectors (trajectory-averaged)
  geom_segment(data = trajectory_drift_vectors,
               aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = AgeGroup),
               arrow = arrow(type = "closed", length = unit(0.15, "in")),
               linewidth = 2, alpha = 0.95, lineend = "round") +
  
  # Age group labels at endpoints
  geom_text(data = trajectory_drift_vectors,
            aes(x = x_end, y = y_end, label = AgeGroup),
            color = "gray50", fontface = "bold", size = 5,
            nudge_y = 0.25, show.legend = FALSE) +
  
  scale_color_viridis_d(option = "D", name = "Age Group") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  
  labs(
    title = paste0(
      "Long-term Average Mobility Drift (2007 to 2024): ",
      scenario_label
    ),
    subtitle = NULL,  # Explicitly remove subtitle
    x = "Fraction of Day Away from Home",
    y = "Mean Distance When Away (km)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_blank(),  # Double-ensure subtitle is removed
    
    # Legend at center bottom, inside plot
    legend.position = c(0.5, 0.08),
    legend.justification = c("center", "bottom"),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    
    legend.background = element_rect(fill = alpha("white", 0.85), 
                                     color = "gray30", linewidth = 0.5),
    legend.margin = margin(8, 12, 8, 12),
    
    legend.key.width = unit(1.5, "cm"),
    legend.key.height = unit(0.6, "cm"),
    
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  )

print(p_drift_trajectory_avg)

ggsave(file.path(output_dir, "drift_vectors_TRAJECTORY_AVERAGED.png"), 
       p_drift_trajectory_avg, width = 11, height = 8, dpi = 300)

cat("✓ Trajectory-averaged drift plot saved\n")

# ============================================================================
# COMPARISON: Endpoint vs Trajectory-averaged
# ============================================================================
cat("\n=== COMPARISON: Endpoint vs Trajectory-Averaged Drift ===\n\n")

comparison <- drift_vectors %>%
  dplyr::select(AgeGroup, dx_endpoint = dx, dy_endpoint = dy, 
                mag_endpoint = magnitude, angle_endpoint = angle) %>%
  dplyr::inner_join(
    trajectory_drift_vectors %>%
      dplyr::select(AgeGroup, dx_trajectory = dx_avg, dy_trajectory = dy_avg,
                    mag_trajectory = magnitude, angle_trajectory = angle),
    by = "AgeGroup"
  ) %>%
  dplyr::mutate(
    dx_diff = dx_trajectory - dx_endpoint,
    dy_diff = dy_trajectory - dy_endpoint,
    mag_diff = mag_trajectory - mag_endpoint,
    angle_diff = angle_trajectory - angle_endpoint,
    dx_pct_diff = (dx_diff / abs(dx_endpoint)) * 100,
    dy_pct_diff = (dy_diff / abs(dy_endpoint)) * 100
  )

print(comparison)

cat("\nKey differences:\n")
for (i in 1:nrow(comparison)) {
  row <- comparison[i, ]
  cat(sprintf("  %s:\n", row$AgeGroup))
  cat(sprintf("    - Time direction: %+.1f%% difference\n", row$dx_pct_diff))
  cat(sprintf("    - Distance direction: %+.1f%% difference\n", row$dy_pct_diff))
  cat(sprintf("    - Magnitude: %.3f (endpoint) vs %.3f (trajectory-avg), diff = %+.3f\n",
              row$mag_endpoint, row$mag_trajectory, row$mag_diff))
}

# Save comparison data
write.csv(comparison,
          file.path(output_dir, "drift_comparison_endpoint_vs_trajectory.csv"),
          row.names = FALSE)

cat("\n✓ Comparison data saved\n")


# ============================================================================
# STEP 6: Interpretations and statistics
# ============================================================================
cat("\n=== DRIFT INTERPRETATIONS ===\n\n")

drift_interpretation <- drift_vectors %>%
  arrange(desc(pct_change_distance)) %>%
  mutate(
    distance_interpretation = case_when(
      pct_change_distance > 10 ~ paste0("EXPANDING: +", round(dy, 2), " km (+", 
                                        round(pct_change_distance, 1), "%)"),
      pct_change_distance < -10 ~ paste0("CONTRACTING: ", round(dy, 2), " km (", 
                                         round(pct_change_distance, 1), "%)"),
      TRUE ~ paste0("STABLE: ", round(dy, 2), " km (", 
                    ifelse(pct_change_distance >= 0, "+", ""),
                    round(pct_change_distance, 1), "%)")
    ),
    time_interpretation = case_when(
      abs(pct_change_time) < 1 ~ "similar time away",
      pct_change_time > 0 ~ paste0("+", round(pct_change_time, 1), "% more time away"),
      pct_change_time < 0 ~ paste0(round(pct_change_time, 1), "% less time away")
    ),
    overall_trend = case_when(
      pct_change_distance > 10 & pct_change_time > 1 ~ "Expanding both space & time",
      pct_change_distance > 10 & pct_change_time <= 1 ~ "Traveling further, similar time",
      pct_change_distance < -10 & pct_change_time < -1 ~ "Contracting both space & time",
      pct_change_distance < -10 & pct_change_time >= -1 ~ "Traveling less far, similar time",
      TRUE ~ "Relatively stable"
    )
  ) %>%
  dplyr::select(AgeGroup, distance_interpretation, time_interpretation, 
                overall_trend, magnitude) %>%
  arrange(AgeGroup)

print(drift_interpretation)

cat("\n=== SUMMARY STATISTICS ===\n\n")
summary_stats <- drift_vectors %>%
  summarise(
    avg_distance_change = mean(dy, na.rm = TRUE),
    avg_pct_distance_change = mean(pct_change_distance, na.rm = TRUE),
    avg_time_change = mean(dx, na.rm = TRUE),
    avg_pct_time_change = mean(pct_change_time, na.rm = TRUE),
    max_expansion = max(pct_change_distance, na.rm = TRUE),
    max_contraction = min(pct_change_distance, na.rm = TRUE),
    expanding_groups = sum(pct_change_distance > 10, na.rm = TRUE),
    contracting_groups = sum(pct_change_distance < -10, na.rm = TRUE),
    stable_groups = sum(abs(pct_change_distance) <= 10, na.rm = TRUE)
  )

cat("Overall trends from baseline (2007-2009) to", most_recent_period, ":\n")
cat("- Average distance change:", round(summary_stats$avg_distance_change, 2), "km\n")
cat("- Average % distance change:", round(summary_stats$avg_pct_distance_change, 1), "%\n")
cat("- Average time away change:", round(summary_stats$avg_pct_time_change, 1), "%\n")
cat("- Groups expanding (>10%):", summary_stats$expanding_groups, "\n")
cat("- Groups contracting (<-10%):", summary_stats$contracting_groups, "\n")
cat("- Groups stable (Â±10%):", summary_stats$stable_groups, "\n\n")

# ============================================================================
# STEP 7: Save all outputs
# ============================================================================
cat("\n=== SAVING OUTPUTS ===\n")

ggsave(file.path(output_dir, "drift_vectors_COMBINED_BASELINE.png"), 
       p_drift_main, width = 11, height = 8, dpi = 300)

ggsave(file.path(output_dir, "drift_vectors_MINIMAL_COMBINED.png"), 
       p_drift_minimal, width = 11, height = 8, dpi = 300)

ggsave(file.path(output_dir, "mobility_trajectories_FULL.png"), 
       p_trajectories, width = 12, height = 8, dpi = 300)

write.csv(drift_interpretation,
          file.path(output_dir, "drift_interpretation_COMBINED_BASELINE.csv"),
          row.names = FALSE)

write.csv(drift_vectors,
          file.path(output_dir, "drift_vectors_data_COMBINED_BASELINE.csv"),
          row.names = FALSE)

dev.off()
cat("\nâœ“ All drift analysis outputs saved!\n")
cat("  Files created:\n")
cat("  - drift_vectors_COMBINED_BASELINE.png (main visualization)\n")
cat("  - drift_vectors_MINIMAL_COMBINED.png (minimal version)\n")
cat("  - drift_vectors_TRAJECTORY_AVERAGED.png (trajectory-averaged)\n")
cat("  - mobility_trajectories_FULL.png (full time series)\n")
cat("  - drift_interpretation_COMBINED_BASELINE.csv (interpretation)\n")
cat("  - drift_vectors_data_COMBINED_BASELINE.csv (raw data)\n")
cat("  - drift_comparison_endpoint_vs_trajectory.csv (comparison)\n\n")

cat("=", rep("=", 60), "\n", sep="")
cat("STEP 5 COMPLETE: Drift vector analysis with combined baseline\n")
cat("                 including trajectory-averaged drift option\n")
cat("=", rep("=", 60), "\n\n", sep="")

saveRDS(all_sequential, file.path(output_dir, "all_sequential.rds"))
saveRDS(drift_vectors,  file.path(output_dir, "drift_vectors.rds"))
