######################################################################
# STEP 3 – TEMPORAL ANALYSIS (FIXED)
######################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(scales)

cat("\n=== STEP 3: TEMPORAL ANALYSIS ===\n\n")

# =======================
# I/O: folder-agnostic
# =======================
# Expect in_dir/out_dir from run_one_scenario.R
# Fallback allows running this script standalone.
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

# Load pre-computed temporal profiles
temporal_baseline_smooth <- readRDS(file.path(input_dir, "temporal_baseline_away.rds"))
temporal_yearbin_smooth  <- readRDS(file.path(input_dir, "temporal_yearbin_away.rds"))

dt <- 24 / 96

cat("  ✓ Loaded files\n")
cat("  Baseline:", nrow(temporal_baseline_smooth), "rows\n")
cat("  Year-bin:", nrow(temporal_yearbin_smooth), "rows\n\n")

######################################################################
# COMPUTE TEMPORAL STATISTICS - WITHOUT group_modify!
######################################################################

cat("Computing temporal statistics...\n")

# Baseline stats - do each age group manually
baseline_stats <- temporal_baseline_smooth %>%
  filter(!is.na(density_smooth)) %>%  # Remove NAs from filter edges
  group_by(AgeGroup) %>%
  summarise(
    #mean_hour = sum(t * density_smooth * dt) / sum(density_smooth * dt),
    mean_hour = {
      theta <- 2 * pi * t / 24  # Convert hours to radians
      weights <- density_smooth * dt
      sin_sum <- sum(sin(theta) * weights)
      cos_sum <- sum(cos(theta) * weights)
      mean_angle <- atan2(sin_sum, cos_sum)  # Result in radians [-pi, pi]
      mean_hour_circular <- (mean_angle * 24 / (2 * pi)) %% 24  # Convert back to hours [0, 24)
      mean_hour_circular
    },
    median_hour = {
      cum_dens <- cumsum(density_smooth * dt)
      cum_dens <- cum_dens / max(cum_dens)
      t[which.min(abs(cum_dens - 0.5))]
    },
    peak_hour = t[which.max(density_smooth)],
    morning_6_12 = sum(density_smooth[t >= 6 & t < 12] * dt),
    midday_12_18 = sum(density_smooth[t >= 12 & t < 18] * dt),
    evening_18_24 = sum(density_smooth[t >= 18 & t <= 24] * dt),
    total_away = sum(density_smooth * dt),
    sd_hour = sqrt(sum((t - mean_hour)^2 * density_smooth * dt) / sum(density_smooth * dt)),
    .groups = "drop"
  ) %>%
  mutate(Period = "Baseline")

cat("  ✓ Baseline statistics (", nrow(baseline_stats), " rows)\n")
print(baseline_stats)
cat("\n")

# Year-bin stats
yearbin_stats <- temporal_yearbin_smooth %>%
  filter(!is.na(density_smooth)) %>%  # Remove NAs
  group_by(AgeGroup, YearBin) %>%
  summarise(
    mean_hour = sum(t * density_smooth * dt) / sum(density_smooth * dt),
    median_hour = {
      cum_dens <- cumsum(density_smooth * dt)
      cum_dens <- cum_dens / max(cum_dens)
      t[which.min(abs(cum_dens - 0.5))]
    },
    peak_hour = t[which.max(density_smooth)],
    morning_6_12 = sum(density_smooth[t >= 6 & t < 12] * dt),
    midday_12_18 = sum(density_smooth[t >= 12 & t < 18] * dt),
    evening_18_24 = sum(density_smooth[t >= 18 & t <= 24] * dt),
    total_away = sum(density_smooth * dt),
    sd_hour = sqrt(sum((t - mean_hour)^2 * density_smooth * dt) / sum(density_smooth * dt)),
    .groups = "drop"
  ) %>%
  rename(Period = YearBin)

cat("  ✓ Year-bin statistics (", nrow(yearbin_stats), " rows)\n")
print(head(yearbin_stats, 10))
cat("\n")

# Combine
all_temporal_stats <- bind_rows(baseline_stats, yearbin_stats)

######################################################################
# COMPUTE SHIFTS
######################################################################

cat("Computing shifts from baseline...\n")

temporal_shifts <- yearbin_stats %>%
  left_join(
    baseline_stats %>% dplyr::select(-Period),
    by = "AgeGroup",
    suffix = c("", "_baseline")
  ) %>%
  mutate(
    shift_mean = mean_hour - mean_hour_baseline,
    shift_median = median_hour - median_hour_baseline,
    shift_peak = peak_hour - peak_hour_baseline,
    shift_morning = morning_6_12 - morning_6_12_baseline,
    shift_midday = midday_12_18 - midday_12_18_baseline,
    shift_evening = evening_18_24 - evening_18_24_baseline,
    shift_total_away = total_away - total_away_baseline,
    shift_sd = sd_hour - sd_hour_baseline
  ) %>%
  dplyr::select(AgeGroup, Period, starts_with("shift_"))

cat("  ✓ Shifts computed (", nrow(temporal_shifts), " rows)\n")
print(head(temporal_shifts, 10))
cat("\n")

######################################################################
# PLOTS
######################################################################

cat("Creating plots...\n")

# Plot 1: Temporal profiles
p_all_ages <- temporal_yearbin_smooth %>%
  filter(!is.na(density_smooth)) %>%
  ggplot(aes(x = t, y = density_smooth, color = YearBin)) +
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_line(data = temporal_baseline_smooth %>% filter(!is.na(density_smooth)), 
            aes(x = t, y = density_smooth),
            color = "black", linewidth = 1.5, linetype = "dashed") +
  facet_wrap(~ AgeGroup, ncol = 2, scales = "free_y") +
  scale_color_viridis_d(option = "D") +
  scale_x_continuous(breaks = seq(0, 24, 6)) +
  labs(
    title = "Temporal Activity Profiles by Age Group",
    subtitle = "When people are away from home (d > 0.5 km). Dashed black = baseline",
    x = "Hour of Day", 
    y = "Probability Density",
    color = "Survey Period"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(file.path(output_dir, "temporal_profiles_all_ages.png"),
       p_all_ages, width = 12, height = 10, dpi = 300)
cat("  ✓ temporal_profiles_all_ages.png\n")

# Plot 2: Mean shift
p_mean_shift <- temporal_shifts %>%
  ggplot(aes(x = Period, y = shift_mean, color = AgeGroup, group = AgeGroup)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_viridis_d(option = "D") +
  labs(
    title = "Shift in Mean Activity Time from Baseline",
    subtitle = "Positive = later in day, Negative = earlier in day",
    x = "Survey Period", y = "Shift (hours)", color = "Age Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

ggsave(file.path(output_dir, "mean_time_shift.png"), 
       p_mean_shift, width = 10, height = 6, dpi = 300)
cat("  ✓ mean_time_shift.png\n")

# Plot 3: Window shifts
p_window_shifts <- temporal_shifts %>%
  dplyr::select(AgeGroup, Period, shift_morning, shift_midday, shift_evening) %>%
  pivot_longer(cols = starts_with("shift_"), names_to = "Window", values_to = "Shift") %>%
  mutate(Window = recode(Window,
                         shift_morning = "Morning (6-12h)",
                         shift_midday  = "Midday (12-18h)",
                         shift_evening = "Evening (18-24h)")) %>%
  ggplot(aes(x = Period, y = Shift, fill = Window)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ AgeGroup, ncol = 2) +
  scale_fill_viridis_d(option = "C") +
  labs(title = "Change in Activity by Time Window",
       subtitle = "Positive = more activity than baseline, Negative = less",
       x = "Survey Period", y = "Change in Probability Mass", fill = "Time Window") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

ggsave(file.path(output_dir, "window_shifts.png"), 
       p_window_shifts, width = 12, height = 10, dpi = 300)
cat("  ✓ window_shifts.png\n")

# Plot 4: Heatmap
p_heatmap <- temporal_shifts %>%
  ggplot(aes(x = Period, y = AgeGroup, fill = shift_mean)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%+.2f", shift_mean)), size = 3.5, fontface = "bold") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, name = "Shift (h)") +
  labs(title = "Mean Activity Time Shift from Baseline",
       subtitle = "Blue = earlier, Red = later",
       x = "Survey Period", y = "Age Group") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())

ggsave(file.path(output_dir, "heatmap_time_shift.png"), 
       p_heatmap, width = 10, height = 7, dpi = 300)
cat("  ✓ heatmap_time_shift.png\n")

# Plot 5: Peak shift
p_peak_shift <- temporal_shifts %>%
  ggplot(aes(x = Period, y = shift_peak, color = AgeGroup, group = AgeGroup)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_viridis_d(option = "D") +
  labs(title = "Shift in Peak Activity Time from Baseline",
       x = "Survey Period", y = "Shift (hours)", color = "Age Group") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

ggsave(file.path(output_dir, "peak_time_shift.png"), 
       p_peak_shift, width = 10, height = 6, dpi = 300)
cat("  ✓ peak_time_shift.png\n\n")

######################################################################
# SAVE TABLES
######################################################################

cat("Saving data tables...\n")

write.csv(temporal_shifts, file.path(output_dir, "temporal_shifts.csv"), row.names = FALSE)
write.csv(all_temporal_stats, file.path(output_dir, "temporal_statistics.csv"), row.names = FALSE)

cat("  ✓ temporal_shifts.csv\n")
cat("  ✓ temporal_statistics.csv\n\n")

cat(rep("=", 70), "\n", sep="")
cat("STEP 3 COMPLETE!\n")
cat(rep("=", 70), "\n")