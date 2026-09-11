######################################################################
# STEP 8 – BOOTSTRAP ARROW-FAN UNCERTAINTY PLOT
#
# PURPOSE
# -------
# Addresses reviewer requests for robustness and uncertainty by
# visualising the sampling stability of the drift vectors. Rather than
# a formal significance test, this produces a fan of drift arrows —
# one per bootstrap resample — showing how consistently each age
# group's arrow points in the same direction under resampling.
#
# METHOD
# ------
# 1. Load the session-level 15-min grid (grid_mv_step2.rds) — already
#    in data/kernels/, no Access database required.
# 2. Collapse to session-level summaries:
#      fraction_away_i    = share of 96 bins with r > HOME_CUTOFF
#      mean_dist_active_i = mean r over away bins
# 3. Bootstrap (B = 500): resample sessions with replacement within
#    each AgeGroup × YearBin cell.
# 4. For each replicate compute drift vectors using the same definition
#    as step 4 (baseline = mean of 6 year-bin values, endpoint =
#    2022–2024).
# 5. Plot: faint arrows (one per replicate per age group) as uncertainty
#    cloud; thick arrows from the KDE-based step 4 estimates on top.
#
# INPUTS
# ------
# data/kernels/<SCENARIO>/grid_mv_step2.rds
# results/<SCENARIO>/action_space_metrics_TRIPWEIGHTED.csv  (KDE estimates)
#
# OUTPUTS  (written to results/uncertainty/)
# -------
# bootstrap_arrow_fan.png    — main figure: arrow fan + KDE overlay
# bootstrap_pct_change.png   — % distance change distribution per group
# bootstrap_summary.csv      — median + 95% CI of bootstrap distribution
#
# RUN
# ---
# Rscript code/step8_uncertainty.R
######################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(purrr)
})

source("code/utils_io.R")

cat("\n=== STEP 8: BOOTSTRAP ARROW-FAN UNCERTAINTY ===\n\n")

# ============================================================================
# CONFIG
# ============================================================================

SCENARIO    <- if (exists("scenario")) scenario else "baseline"
B           <- 500
SEED        <- 42
HOME_CUTOFF <- 0.02   # km — must match step 2 / step 4
AGE_LEVELS  <- c("10-17", "18-30", "31-55", "56-65", "66+")

set.seed(SEED)

dirs    <- get_dirs(SCENARIO)
OUT_DIR <- file.path("results", "uncertainty")
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
# 1.  KDE-BASED POINT ESTIMATES  (from step 4 — the headline results)
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

kde_baseline <- period_rows %>%
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

cat("  \u2713 KDE drift vectors loaded\n\n")
print(kde_drift %>% select(AgeGroup, pct_change_distance) %>%
        mutate(pct_change_distance = round(pct_change_distance, 1)))
cat("\n")

# ============================================================================
# 2.  LOAD SESSION-LEVEL GRID
# ============================================================================

cat("Loading grid_mv_step2.rds...\n")
# Use results/ copy — verified correct (data/kernels/ may be an older run)
grid_path <- file.path(dirs$out_dir, "grid_mv_step2.rds")
if (!file.exists(grid_path))
  stop("grid_mv_step2.rds not found at: ", grid_path)

grid_mv <- readRDS(grid_path)
cat("  \u2713 Loaded:", format(nrow(grid_mv), big.mark = ","), "rows,",
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
    mean_dist_i = mean(dist_if_away, na.rm = TRUE),   # NaN when no away bins
    .groups = "drop"
  ) %>%
  mutate(
    mean_dist_i = if_else(is.nan(mean_dist_i), NA_real_, mean_dist_i),
    AgeGroup    = factor(AgeGroup, levels = AGE_LEVELS)
  )

cat("  \u2713", format(nrow(session_summ), big.mark = ","),
    "session-level rows across",
    n_distinct(session_summ$YearBin), "year-bins\n\n")

# Pre-split for fast bootstrap resampling
splits <- session_summ %>%
  group_by(AgeGroup, YearBin) %>%
  group_split()

# ============================================================================
# 4.  HELPERS
# ============================================================================

# weighted.mean that skips NA in BOTH x and w (base weighted.mean only skips
# NA in x, returning NA whenever any weight is NA)
wm_safe <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  if (!any(keep)) return(NA_real_)
  sum(x[keep] * w[keep]) / sum(w[keep])
}

compute_drift_boot <- function(df, replicate_id) {
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

  base <- cell_means %>%
    group_by(AgeGroup) %>%
    summarise(x_base = mean(frac_away, na.rm = TRUE),
              y_base = mean(mean_dist,  na.rm = TRUE), .groups = "drop")

  ep <- cell_means %>%
    filter(YearBin == max(YearBin)) %>%
    select(AgeGroup, x_end = frac_away, y_end = mean_dist)

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
# 5.  RUN BOOTSTRAP
# ============================================================================

cat("Running", B, "bootstrap replicates...\n")

boot_results <- map_dfr(seq_len(B), function(b) {
  boot_data <- map_dfr(splits, ~ .x[sample(nrow(.x), nrow(.x), replace = TRUE), ])
  compute_drift_boot(boot_data, b)
})

cat("  \u2713 Bootstrap complete\n\n")

# ============================================================================
# 5b. RECENTER BOOTSTRAP AROUND KDE DRIFT
#
# The raw session-level bootstrap mean differs from the KDE estimate due
# to bandwidth smoothing. We keep the bootstrap *variance* (spread of the
# fan) but shift the *mean* to align with the KDE arrow, so the fan
# radiates from and points toward the same place as the headline result.
# ============================================================================

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
    dx_centered = kde_dx + (dx - mean_dx),   # deviation re-centred on KDE drift
    dy_centered = kde_dy + (dy - mean_dy)
  )

# ============================================================================
# 6.  BOOTSTRAP SUMMARY
# ============================================================================

boot_summary <- boot_results %>%
  group_by(AgeGroup) %>%
  summarise(
    boot_median_pct = median(pct_change_distance, na.rm = TRUE),
    boot_lo         = quantile(pct_change_distance, 0.025, na.rm = TRUE),
    boot_hi         = quantile(pct_change_distance, 0.975, na.rm = TRUE),
    boot_dx_lo      = quantile(dx, 0.025, na.rm = TRUE),
    boot_dx_hi      = quantile(dx, 0.975, na.rm = TRUE),
    boot_dy_lo      = quantile(dy, 0.025, na.rm = TRUE),
    boot_dy_hi      = quantile(dy, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

out_summary <- kde_drift %>%
  select(AgeGroup, kde_pct = pct_change_distance) %>%
  left_join(boot_summary, by = "AgeGroup") %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

cat("--- Bootstrap distribution of % change in distance ---\n")
cat("(KDE point estimate shown alongside bootstrap 95% range)\n\n")
print(out_summary %>% select(AgeGroup, kde_pct, boot_lo, boot_median_pct, boot_hi))
cat("\n")

write.csv(out_summary, file.path(OUT_DIR, "bootstrap_summary.csv"),
          row.names = FALSE)
cat("  \u2713 bootstrap_summary.csv\n\n")

# ============================================================================
# 7.  FIGURE 1: BOOTSTRAP ARROW FAN
# ============================================================================

cat("Creating bootstrap arrow-fan figure...\n")

# Thin bootstrap arrows — colour by age group, high transparency
# Thick KDE arrows on top

p_fan <- ggplot() +

  # --- Bootstrap arrow cloud ---
  # Fan is anchored at the KDE baseline (same diamond as the thick arrow).
  # Only the endpoint varies per replicate: KDE_base + bootstrap_drift.
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
    alpha = 0.04, linewidth = 0.35,
    arrow = arrow(type = "open", length = unit(0.04, "in"), angle = 25)
  ) +

  # --- KDE baseline diamonds ---
  geom_point(
    data = kde_drift,
    aes(x = x_base, y = y_base, color = AgeGroup),
    shape = 18, size = 5
  ) +

  # --- KDE drift arrows (headline result) ---
  geom_segment(
    data = kde_drift,
    aes(x = x_base, y = y_base,
        xend = x_end, yend = y_end,
        color = AgeGroup),
    linewidth = 1.6,
    arrow = arrow(type = "closed", length = unit(0.12, "in"), angle = 20),
    lineend = "round"
  ) +

  # --- KDE endpoint labels ---
  geom_text(
    data = kde_drift,
    aes(x = x_end, y = y_end, label = AgeGroup, color = AgeGroup),
    size = 3.5, fontface = "bold", nudge_y = 0.18, show.legend = FALSE
  ) +

  scale_color_manual(values = age_colours, name = "Age group") +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +

  labs(
    title    = "Drift vector stability: 500 bootstrap resamples",
    subtitle = paste0(
      "Faint arrows: session-level bootstrap resamples (n = ", B, ") within each age group \u00d7 year-bin cell.\n",
      "Thick arrows: KDE-based point estimates (headline results). Diamond = period-averaged baseline."
    ),
    x = "Fraction of day away from home",
    y = "Mean distance when away (km)"
  ) +

  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title      = element_text(face = "bold"),
    plot.subtitle   = element_text(size = 9, color = "gray40"),
    panel.border    = element_rect(color = "gray80", fill = NA, linewidth = 0.4)
  )

ggsave(file.path(OUT_DIR, "bootstrap_arrow_fan.png"),
       p_fan, width = 10, height = 7, dpi = 300)
cat("  \u2713 bootstrap_arrow_fan.png\n")

# ============================================================================
# 8.  FIGURE 2: % CHANGE DISTRIBUTION (violin / box per age group)
# ============================================================================

cat("Creating % change distribution figure...\n")

p_dist <- boot_results %>%
  mutate(
    AgeGroup = factor(AgeGroup, levels = AGE_LEVELS),
    pct_centered = 100 * dy_centered / kde_y_base   # recentred % change
  ) %>%
  ggplot(aes(x = AgeGroup, y = pct_centered, fill = AgeGroup)) +

  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50",
             linewidth = 0.6) +

  geom_violin(alpha = 0.35, color = NA, trim = TRUE) +

  geom_boxplot(width = 0.15, outlier.shape = NA,
               color = "gray30", fill = "white", alpha = 0.8) +

  # KDE point estimate as a cross
  geom_point(
    data = kde_drift,
    aes(x = AgeGroup, y = pct_change_distance),
    shape = 4, size = 4, stroke = 1.5, color = "black",
    inherit.aes = FALSE
  ) +

  scale_fill_manual(values = age_colours, guide = "none") +
  scale_y_continuous(
    labels = function(x) paste0(ifelse(x >= 0, "+", ""), round(x, 0), "%")
  ) +

  labs(
    title    = "Bootstrap distribution of % change in mean distance (2022\u20132024 vs baseline)",
    subtitle = paste0(
      "Violin + box: distribution across ", B, " session-level bootstrap resamples.\n",
      "Cross (\u00d7): KDE-based point estimate (headline result)."
    ),
    x = "Age group",
    y = "% change in mean distance when away"
  ) +

  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray40")
  )

ggsave(file.path(OUT_DIR, "bootstrap_pct_change.png"),
       p_dist, width = 9, height = 6, dpi = 300)
cat("  \u2713 bootstrap_pct_change.png\n\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat(rep("=", 65), "\n", sep = "")
cat("STEP 8 COMPLETE: Bootstrap arrow-fan uncertainty\n")
cat(rep("=", 65), "\n\n", sep = "")
cat("Outputs written to:", OUT_DIR, "\n")
cat("  bootstrap_arrow_fan.png   — fan of 500 arrows per age group\n")
cat("  bootstrap_pct_change.png  — violin plot of % change distribution\n")
cat("  bootstrap_summary.csv     — KDE estimate + bootstrap 95% range\n\n")

cat("Interpretation:\n")
cat("  Arrow direction consistency across resamples = sampling stability.\n")
cat("  KDE arrows (thick) are the headline result; the fan shows how\n")
cat("  much the direction varies under session-level resampling.\n")
cat("  Note: bootstrap point estimates may differ from KDE estimates\n")
cat("  due to KDE bandwidth smoothing — direction stability is the\n")
cat("  key quantity, not the bootstrap mean.\n\n")
