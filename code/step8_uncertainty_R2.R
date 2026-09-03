######################################################################
# STEP 8 – BOOTSTRAP ARROW-FAN UNCERTAINTY PLOT (R2 VERSION)
#
# PURPOSE
# -------
# Addresses reviewer requests for robustness and uncertainty by
# visualising the sampling stability of the drift vectors. Rather than
# a formal significance test, this produces a fan of drift arrows —
# one per bootstrap resample — showing how consistently each age
# group's arrow points in the same direction under resampling.
#
# KEY R2 ADDITION:
# Generates two figures:
# 1. Figure_R2_6a.png - Full-period drift stability (2022-2024 vs baseline)
# 2. Figure_R2_S8.png - Pre-pandemic drift stability (2016-2018 vs 2007-2018 baseline)
#
# INPUTS
# ------
# data/kernels/<SCENARIO>/grid_mv_step2.rds
# results/<SCENARIO>/action_space_metrics_TRIPWEIGHTED.csv  (KDE estimates)
#
# OUTPUTS  (written to results/uncertainty/ and Overleaf_source/figures/)
# -------
# Figure_R2_6a.png  - Main figure: full-period arrow fan
# Figure_R2_S8.png  - Supplementary figure: pre-pandemic arrow fan
# bootstrap_pct_change.png
# bootstrap_summary.csv
#
# RUN
# ---
# Rscript code/code/step8_uncertainty_R2.R
######################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(purrr)
})

source("code/utils_io.R")

cat("\n=== STEP 8: BOOTSTRAP ARROW-FAN UNCERTAINTY (R2 VERSION) ===\n\n")

# ============================================================================
# CONFIG
# ============================================================================

SCENARIO    <- if (exists("scenario")) scenario else "baseline"
B           <- 500
SEED        <- 42
HOME_CUTOFF <- 0.02   # km — must match step 2 / step 4
AGE_LEVELS  <- c("10-17", "18-30", "31-55", "56-65", "66+")

# Pre-pandemic comparison (Supplementary Figure S8).
# Endpoint-to-endpoint: the first bin against the last bin ending before the
# pandemic. This is the comparison Reviewer 2 asked for in comment 2.5.
# It is deliberately NOT an endpoint-versus-window-mean contrast: averaging a
# baseline over a window that contains the endpoint damps any monotone trend,
# which is what suppressed the 18-30 pre-pandemic signal in the first version
# of this figure.
PRE_START <- "2007-2009"
PRE_END   <- "2016-2018"

set.seed(SEED)

dirs    <- get_dirs(SCENARIO)
OUT_DIR <- file.path("results", "uncertainty", SCENARIO)

# Which manuscript figure this scenario's arrow fan becomes. Supplementary
# Figure S5 is produced here rather than by a separate script so that it uses
# the identical visual template to Figure 6a - same baseline, same bootstrap
# fan, same axes - since its caption tells the reader it relates to Figure 6a.
MANUSCRIPT_FIGURE <- c(
  baseline = "Figure_R2_6a.png",
  sex1     = "drift_vectors_males.png",
  sex2     = "drift_vectors_females.png"
)[[SCENARIO]]
if (is.null(MANUSCRIPT_FIGURE) || is.na(MANUSCRIPT_FIGURE)) MANUSCRIPT_FIGURE <- NA_character_
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

age_colours <- c(
  "10-17" = "#440154",
  "18-30" = "#3B528B",
  "31-55" = "#21908C",
  "56-65" = "#5DC963",
  "66+"   = "#FDE725"
)

cat("Scenario   :", SCENARIO, "\n")
cat("Replicates :", B, "\n\n")

# ============================================================================
# 1.  KDE-BASED POINT ESTIMATES  (from step 4)
# ============================================================================

cat("Loading step 4 KDE-based point estimates...\n")

metrics_path <- file.path(dirs$out_dir, "action_space_metrics_TRIPWEIGHTED.csv")
if (!file.exists(metrics_path))
  stop("Step 4 output not found: ", metrics_path)

metrics <- read.csv(metrics_path, check.names = FALSE) %>%
  mutate(
    Period   = gsub("[\u2012\u2013\u2014\u2212\u2015]", "-", Period),
    AgeGroup = factor(AgeGroup, levels = AGE_LEVELS)
  )

period_rows <- metrics %>% filter(!grepl("^Baseline", Period))

# A. Full Period (Headline) Point Estimates
kde_baseline <- period_rows %>%
  filter(in_baseline(Period)) %>%
  group_by(AgeGroup) %>%
  summarise(x_base = mean(fraction_away),
            y_base = mean(mean_distance_active), .groups = "drop")

latest_period <- sort(unique(period_rows$Period)) |> tail(1)

kde_endpoint <- period_rows %>%
  filter(Period == latest_period) %>%
  select(AgeGroup, x_end = fraction_away, y_end = mean_distance_active)

kde_drift <- kde_baseline %>%
  inner_join(kde_endpoint, by = "AgeGroup") %>%
  mutate(
    dx  = x_end - x_base,
    dy  = y_end - y_base,
    pct_change_distance = 100 * dy / y_base
  )

cat("  ✓ KDE drift vectors (full) loaded\n")

# B. Pre-Pandemic Point Estimates
pre_pandemic_periods <- c("2007-2009", "2010-2012", "2013-2015", "2016-2018")

kde_baseline_pre <- period_rows %>%
  filter(Period == PRE_START) %>%
  select(AgeGroup, x_base = fraction_away, y_base = mean_distance_active)

kde_endpoint_pre <- period_rows %>%
  filter(Period == PRE_END) %>%
  select(AgeGroup, x_end = fraction_away, y_end = mean_distance_active)

kde_drift_pre <- kde_baseline_pre %>%
  inner_join(kde_endpoint_pre, by = "AgeGroup") %>%
  mutate(
    dx  = x_end - x_base,
    dy  = y_end - y_base,
    pct_change_distance = 100 * dy / y_base
  )

cat("  ✓ KDE drift vectors (pre-pandemic) loaded\n\n")

# ============================================================================
# 2.  LOAD SESSION-LEVEL GRID
# ============================================================================

cat("Loading grid_mv_step2.rds...\n")
grid_path <- file.path(dirs$out_dir, "grid_mv_step2.rds")
if (!file.exists(grid_path))
  stop("grid_mv_step2.rds not found at: ", grid_path)

grid_mv <- readRDS(grid_path)
cat("  ✓ Loaded:", format(nrow(grid_mv), big.mark = ","), "rows,",
    format(n_distinct(grid_mv$SessionId), big.mark = ","), "sessions\n\n")

# ============================================================================
# 3.  COLLAPSE TO SESSION-LEVEL SUMMARIES
# ============================================================================

cat("Collapsing to session-level summaries...\n")

year_bin_map <- tibble(
  Year = 2007:2024,
  YearBin = case_when(
    Year %in% 2007:2009 ~ "2007-2009",
    Year %in% 2010:2012 ~ "2010-2012",
    Year %in% 2013:2015 ~ "2013-2015",
    Year %in% 2016:2018 ~ "2016-2018",
    Year %in% 2019:2021 ~ "2019-2021",
    Year %in% 2022:2024 ~ "2022-2024"
  )
)

session_summ <- grid_mv %>%
  left_join(year_bin_map, by = "Year") %>%
  filter(!is.na(YearBin), !is.na(AgeGroup)) %>%
  mutate(
    away         = !is.na(r_rad_km) & r_rad_km > HOME_CUTOFF,
    dist_if_away = if_else(away, r_rad_km, NA_real_)
  ) %>%
  group_by(SessionId, AgeGroup, YearBin, SessionWeight) %>%
  summarise(
    frac_away_i = mean(away,         na.rm = TRUE),
    mean_dist_i = mean(dist_if_away, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_dist_i = if_else(is.nan(mean_dist_i), NA_real_, mean_dist_i),
    AgeGroup    = factor(AgeGroup, levels = AGE_LEVELS)
  )

cat("  ✓", format(nrow(session_summ), big.mark = ","),
    "session-level rows across",
    n_distinct(session_summ$YearBin), "year-bins\n\n")

# Pre-split for fast bootstrap resampling
splits <- session_summ %>%
  group_by(AgeGroup, YearBin) %>%
  group_split()

# ============================================================================
# 4.  HELPERS
# ============================================================================

wm_safe <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  if (!any(keep)) return(NA_real_)
  sum(x[keep] * w[keep]) / sum(w[keep])
}

compute_drift_boot <- function(df, replicate_id, is_pre_pandemic = FALSE) {
  # Fraction away: all sessions contribute
  cell_frac <- df %>%
    group_by(AgeGroup, YearBin) %>%
    summarise(frac_away = wm_safe(frac_away_i, SessionWeight),
              .groups = "drop")

  # Mean distance: only sessions with away time
  cell_dist <- df %>%
    filter(!is.na(mean_dist_i)) %>%
    group_by(AgeGroup, YearBin) %>%
    summarise(mean_dist = wm_safe(mean_dist_i, SessionWeight),
              .groups = "drop")

  cell_means <- cell_frac %>%
    left_join(cell_dist, by = c("AgeGroup", "YearBin"))

  if (is_pre_pandemic) {
    # Pre-pandemic scenario
    pre_pandemic_bins <- c("2007-2009", "2010-2012", "2013-2015", "2016-2018")
    cell_means <- cell_means %>% filter(YearBin %in% pre_pandemic_bins)

    base <- cell_means %>%
      filter(YearBin == PRE_START) %>%
      select(AgeGroup, x_base = frac_away, y_base = mean_dist)

    ep <- cell_means %>%
      filter(YearBin == PRE_END) %>%
      select(AgeGroup, x_end = frac_away, y_end = mean_dist)
  } else {
    # Full scenario
    base <- cell_means %>%
      filter(in_baseline(YearBin)) %>%
      group_by(AgeGroup) %>%
      summarise(x_base = mean(frac_away, na.rm = TRUE),
                y_base = mean(mean_dist,  na.rm = TRUE), .groups = "drop")

    ep <- cell_means %>%
      filter(YearBin == max(YearBin)) %>%
      select(AgeGroup, x_end = frac_away, y_end = mean_dist)
  }

  base %>%
    inner_join(ep, by = "AgeGroup") %>%
    mutate(
      dx  = x_end - x_base,
      dy  = y_end - y_base,
      pct_change_distance = 100 * dy / y_base,
      replicate = replicate_id
    )
}

# ============================================================================
# 5.  RUN BOOTSTRAP (FULL SCENARIO)
# ============================================================================

cat("Running", B, "bootstrap replicates (Full Scenario)...\n")

boot_results <- map_dfr(seq_len(B), function(b) {
  boot_data <- map_dfr(splits, ~ .x[sample(nrow(.x), nrow(.x), replace = TRUE), ])
  compute_drift_boot(boot_data, b, is_pre_pandemic = FALSE)
})

cat("  ✓ Full Scenario Bootstrap complete\n\n")

# Recenter full bootstrap
boot_means <- boot_results %>%
  group_by(AgeGroup) %>%
  summarise(mean_dx = mean(dx, na.rm = TRUE),
            mean_dy = mean(dy, na.rm = TRUE),
            .groups = "drop")

boot_results <- boot_results %>%
  left_join(boot_means, by = "AgeGroup") %>%
  left_join(kde_drift %>% select(AgeGroup, kde_dx = dx, kde_dy = dy,
                                 kde_x_base = x_base, kde_y_base = y_base),
            by = "AgeGroup") %>%
  mutate(
    dx_centered = kde_dx + (dx - mean_dx),
    dy_centered = kde_dy + (dy - mean_dy)
  )

# ============================================================================
# 6.  RUN BOOTSTRAP (PRE-PANDEMIC SCENARIO)
# ============================================================================

cat("Running", B, "bootstrap replicates (Pre-Pandemic)...\n")

# Filter splits for pre-pandemic periods to speed up sampling
splits_pre <- splits %>%
  keep(~ first(.x$YearBin) %in% pre_pandemic_periods)

boot_results_pre <- map_dfr(seq_len(B), function(b) {
  boot_data <- map_dfr(splits_pre, ~ .x[sample(nrow(.x), nrow(.x), replace = TRUE), ])
  compute_drift_boot(boot_data, b, is_pre_pandemic = TRUE)
})

cat("  ✓ Pre-Pandemic Bootstrap complete\n\n")

# Recenter pre-pandemic bootstrap
boot_means_pre <- boot_results_pre %>%
  group_by(AgeGroup) %>%
  summarise(mean_dx = mean(dx, na.rm = TRUE),
            mean_dy = mean(dy, na.rm = TRUE),
            .groups = "drop")

boot_results_pre <- boot_results_pre %>%
  left_join(boot_means_pre, by = "AgeGroup") %>%
  left_join(kde_drift_pre %>% select(AgeGroup, kde_dx = dx, kde_dy = dy,
                                     kde_x_base = x_base, kde_y_base = y_base),
            by = "AgeGroup") %>%
  mutate(
    dx_centered = kde_dx + (dx - mean_dx),
    dy_centered = kde_dy + (dy - mean_dy)
  )

# ============================================================================
# 7.  OUTPUT SUMMARIES
# ============================================================================

boot_summary <- boot_results %>%
  group_by(AgeGroup) %>%
  summarise(
    boot_median_pct = median(pct_change_distance, na.rm = TRUE),
    boot_lo         = quantile(pct_change_distance, 0.025, na.rm = TRUE),
    boot_hi         = quantile(pct_change_distance, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

out_summary <- kde_drift %>%
  select(AgeGroup, kde_pct = pct_change_distance) %>%
  left_join(boot_summary, by = "AgeGroup") %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

write.csv(out_summary, file.path(OUT_DIR, "bootstrap_summary.csv"), row.names = FALSE)
cat("  ✓ bootstrap_summary.csv saved\n")

# Pre-pandemic point estimates and bootstrap intervals.
# Written out because the response letter quotes these numbers; without them
# nothing in results/ lets a reader check Supplementary Figure S8.
pre_point <- kde_drift_pre %>%
  select(AgeGroup, x_base, y_base, x_end, y_end, dx, dy, pct_change_distance)

pre_ci <- boot_results_pre %>%
  group_by(AgeGroup) %>%
  summarise(
    dx_lo = quantile(dx_centered, 0.025, na.rm = TRUE),
    dx_hi = quantile(dx_centered, 0.975, na.rm = TRUE),
    dy_lo = quantile(dy_centered, 0.025, na.rm = TRUE),
    dy_hi = quantile(dy_centered, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

pre_summary <- pre_point %>%
  left_join(pre_ci, by = "AgeGroup") %>%
  mutate(
    dx_significant = (dx_lo > 0) | (dx_hi < 0),
    dy_significant = (dy_lo > 0) | (dy_hi < 0)
  )

write.csv(pre_summary, file.path(OUT_DIR, "prepandemic_drift_summary.csv"),
          row.names = FALSE)
cat("  ✓ prepandemic_drift_summary.csv saved\n")

write.csv(boot_results_pre %>%
            select(replicate, AgeGroup, dx = dx_centered, dy = dy_centered),
          file.path(OUT_DIR, "prepandemic_drift_replicates.csv"), row.names = FALSE)
cat("  ✓ prepandemic_drift_replicates.csv saved\n")

cat("\n--- Pre-pandemic drift (", PRE_START, " to ", PRE_END, ") ---\n", sep = "")
print(as.data.frame(
  pre_summary %>%
    select(AgeGroup, dx, dx_lo, dx_hi, dx_significant,
           dy, dy_lo, dy_hi, dy_significant) %>%
    mutate(across(where(is.numeric), ~ round(., 4)))
), row.names = FALSE)
cat("\n")

# ============================================================================
# 8.  FIGURE 6A: BOOTSTRAP ARROW FAN (FULL PERIOD)
# ============================================================================

cat("Creating Figure_R2_6a.png...\n")

p_fan_full <- ggplot() +
  geom_segment(
    data = boot_results %>%
      mutate(
        AgeGroup  = factor(AgeGroup, levels = AGE_LEVELS),
        fan_xend  = kde_x_base + dx_centered,
        fan_yend  = kde_y_base + dy_centered
      ) %>%
      filter(!is.na(fan_xend), !is.na(fan_yend)),
    aes(x = kde_x_base, y = kde_y_base,
        xend = fan_xend, yend = fan_yend,
        color = AgeGroup),
    alpha = 0.04, linewidth = 0.45,
    arrow = arrow(type = "open", length = unit(0.04, "in"), angle = 25)
  ) +
  geom_point(
    data = kde_drift,
    aes(x = x_base, y = y_base, color = AgeGroup),
    shape = 18, size = 6
  ) +
  geom_segment(
    data = kde_drift,
    aes(x = x_base, y = y_base,
        xend = x_end, yend = y_end,
        color = AgeGroup),
    linewidth = 1.8,
    arrow = arrow(type = "closed", length = unit(0.14, "in"), angle = 20),
    lineend = "round"
  ) +
  geom_text(
    data = kde_drift,
    aes(x = x_end, y = y_end, label = AgeGroup, color = AgeGroup),
    size = 5.0, fontface = "bold", nudge_y = 0.20, show.legend = FALSE
  ) +
  scale_color_manual(values = age_colours, name = "Age group") +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  labs(
    title    = if (SCENARIO == "baseline")
                 "Drift vector stability: 500 bootstrap resamples (2022-2024 vs baseline)"
               else
                 paste0("Drift vector stability (2022-2024 vs baseline): ",
                        get_scenario_label(SCENARIO)),
    subtitle = paste0(
      "Faint arrows: session-level bootstrap resamples (n = ", B, "). ",
      "Thick arrows: KDE-based point estimates. Diamond = 2007-2024 baseline."
    ),
    x = "Fraction of day away from home",
    y = "Mean distance when away (km)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "right",
    plot.title      = element_text(face = "bold", size = 18),
    plot.subtitle   = element_text(size = 11, color = "gray40"),
    panel.border    = element_rect(color = "gray80", fill = NA, linewidth = 0.4)
  )

ggsave(file.path(OUT_DIR, "drift_fan_full.png"), p_fan_full, width = 10, height = 7.5, dpi = 300)
if (!is.na(MANUSCRIPT_FIGURE)) {
  ggsave(file.path(get_manuscript_fig_dir(), MANUSCRIPT_FIGURE),
         p_fan_full, width = 10, height = 7.5, dpi = 300)
  cat("  exported as", MANUSCRIPT_FIGURE, "
")
}
cat("  ✓ Figure_R2_6a.png saved to Overleaf and results\n")

# ============================================================================
# 9.  FIGURE S8: BOOTSTRAP ARROW FAN (PRE-PANDEMIC)
# ============================================================================

cat("Creating Figure_R2_S8.png...\n")

p_fan_pre <- ggplot() +
  geom_segment(
    data = boot_results_pre %>%
      mutate(
        AgeGroup  = factor(AgeGroup, levels = AGE_LEVELS),
        fan_xend  = kde_x_base + dx_centered,
        fan_yend  = kde_y_base + dy_centered
      ) %>%
      filter(!is.na(fan_xend), !is.na(fan_yend)),
    aes(x = kde_x_base, y = kde_y_base,
        xend = fan_xend, yend = fan_yend,
        color = AgeGroup),
    alpha = 0.04, linewidth = 0.45,
    arrow = arrow(type = "open", length = unit(0.04, "in"), angle = 25)
  ) +
  geom_point(
    data = kde_drift_pre,
    aes(x = x_base, y = y_base, color = AgeGroup),
    shape = 18, size = 6
  ) +
  geom_segment(
    data = kde_drift_pre,
    aes(x = x_base, y = y_base,
        xend = x_end, yend = y_end,
        color = AgeGroup),
    linewidth = 1.8,
    arrow = arrow(type = "closed", length = unit(0.14, "in"), angle = 20),
    lineend = "round"
  ) +
  geom_text(
    data = kde_drift_pre,
    aes(x = x_end, y = y_end, label = AgeGroup, color = AgeGroup),
    size = 5.0, fontface = "bold", nudge_y = 0.20, show.legend = FALSE
  ) +
  scale_color_manual(values = age_colours, name = "Age group") +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  labs(
    title    = paste0("Pre-pandemic drift vectors (", PRE_START, " to ", PRE_END, ")"),
    subtitle = paste0(
      "Faint arrows: session-level bootstrap resamples (n = ", B, "). ",
      "Thick arrows: KDE-based point estimates. Diamond = ", PRE_START, " origin."
    ),
    x = "Fraction of day away from home",
    y = "Mean distance when away (km)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "right",
    plot.title      = element_text(face = "bold", size = 18),
    plot.subtitle   = element_text(size = 11, color = "gray40"),
    panel.border    = element_rect(color = "gray80", fill = NA, linewidth = 0.4)
  )

# Dropped from the supplement in favour of Supplementary Table S4; retained
# here because the estimates remain a useful cross-check.
ggsave(file.path(OUT_DIR, "drift_fan_prepandemic.png"), p_fan_pre,
       width = 10, height = 7.5, dpi = 300)
cat("  ✓ Figure_R2_S8.png saved to Overleaf and results\n")

# ============================================================================
# 10. DIAGNOSTIC PLOT: % CHANGE DISTRIBUTION
# ============================================================================

cat("Creating diagnostic % change distribution plot...\n")

p_dist <- boot_results %>%
  mutate(
    AgeGroup = factor(AgeGroup, levels = AGE_LEVELS),
    pct_centered = 100 * dy_centered / kde_y_base
  ) %>%
  ggplot(aes(x = AgeGroup, y = pct_centered, fill = AgeGroup)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
  geom_violin(alpha = 0.35, color = NA, trim = TRUE) +
  geom_boxplot(width = 0.15, outlier.shape = NA, color = "gray30", fill = "white", alpha = 0.8) +
  geom_point(
    data = kde_drift,
    aes(x = AgeGroup, y = pct_change_distance),
    shape = 4, size = 5, stroke = 1.5, color = "black",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = age_colours, guide = "none") +
  scale_y_continuous(
    labels = function(x) paste0(ifelse(x >= 0, "+", ""), round(x, 0), "%")
  ) +
  labs(
    title    = "Bootstrap distribution of % change in mean distance (2022-2024 vs baseline)",
    subtitle = paste0(
      "Violin + box: distribution across ", B, " bootstrap resamples. ",
      "Cross (x): KDE point estimate."
    ),
    x = "Age group",
    y = "% change in mean distance when away"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title    = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 11, color = "gray40")
  )

ggsave(file.path(OUT_DIR, "bootstrap_pct_change.png"), p_dist, width = 9, height = 6, dpi = 300)
cat("  ✓ bootstrap_pct_change.png saved\n\n")

cat("=== STEP 8 COMPLETE: Bootstrap Arrow-Fan Uncertainty (R2 Version) ===\n\n")
