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
source("code/bootstrap_kde.R")
source("code/eq7_weighting_lib.R")

# Bandwidths for the bootstrap KDE refit; must match step2.
if (!exists("BOOT_H_T")) BOOT_H_T <- 1
if (!exists("BOOT_H_D")) BOOT_H_D <- 5

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
# Read the grid from the kernel folder, which is where step2 writes it and
# what 00_validate_inputs.R checks. This read used dirs$out_dir (results/),
# contradicting the header above: after step2 was rerun, the kernels updated
# but results/ kept a stale copy, so the bootstrap silently resampled the old
# grid while the point estimates came from the new one.
grid_path <- file.path(dirs$in_dir, "grid_mv_step2.rds")
if (!file.exists(grid_path))
  stop("grid_mv_step2.rds not found at: ", grid_path)

grid_mv <- readRDS(grid_path)
cat("  ✓ Loaded:", format(nrow(grid_mv), big.mark = ","), "rows,",
    format(n_distinct(grid_mv$SessionId), big.mark = ","), "sessions\n\n")

# Keep only the columns the collapse below reads. The baseline grid is 6.4M
# rows x 12 columns and the join and mutate that follow copy it, which put the
# peak past what a 16 GB machine had free. Age, Cars, City, Male, TimeMSM,
# d_cum_km and active are never referenced in this script, so dropping them
# changes nothing but the footprint.
grid_mv <- grid_mv[, c("SessionId", "AgeGroup", "SessionWeight", "Year", "r_rad_km", "TimeMSM")]
gc(verbose = FALSE)

# ============================================================================
# 3.  COLLAPSE TO SESSION-LEVEL SUMMARIES
# ============================================================================

cat("Precomputing per-cell KDE structures for the Eq. (7) bootstrap...
")

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

grid_mv <- grid_mv %>%
  left_join(year_bin_map, by = "Year") %>%
  filter(!is.na(YearBin), !is.na(AgeGroup))

CELLS <- expand.grid(AgeGroup = AGE_LEVELS,
                     YearBin  = sort(unique(grid_mv$YearBin)),
                     stringsAsFactors = FALSE)

PREP <- vector("list", nrow(CELLS))
for (k in seq_len(nrow(CELLS))) {
  rows <- grid_mv[grid_mv$AgeGroup == CELLS$AgeGroup[k] &
                  grid_mv$YearBin  == CELLS$YearBin[k], ]
  PREP[[k]] <- bk_prepare(rows, h_t = BOOT_H_T, h_d = BOOT_H_D)
}
names(PREP) <- paste(CELLS$AgeGroup, CELLS$YearBin)
rm(grid_mv); gc(verbose = FALSE)

cat("  OK", sum(!vapply(PREP, is.null, logical(1))), "of", length(PREP),
    "age-period cells prepared

")

# ============================================================================
# 4.  HELPERS
# ============================================================================

# One bootstrap replicate. Sessions are drawn with replacement inside each
# age-period cell, the away density is rebuilt from the drawn rows, and the
# Eq. (7) functional is evaluated on it -- the quantity the figure plots.
#
# The draw is kept as per-slice vectors rather than collapsed by bk_metrics(),
# because Eq. (7) is now a fixed-weight mean and its reference profile spans
# an age group's five baseline periods. Rebuilding the reference from the same
# replicate makes the interval carry the uncertainty in the weights too.
# eq7_cell_means() applies the identical rule that eq7_metrics.R applies to
# the point estimate.
# ---------------------------------------------------------------------------
# Eq. (8): linear density-weighted mean time of day when away.
#
# The point estimate (step3_daytime.R) evaluates this on the SMOOTHED temporal
# marginal: step 2 convolves the per-slice away mass with a 0.5 h Gaussian and
# stats::filter() leaves NA at each edge, which step 3 drops before taking the
# mean. The bootstrap has to use the same convention or its interval would not
# be centred on the published quantity. bk_slices()$occ is that same per-slice
# away mass, normalised, so the smoothing and the mean apply directly to it.
# ---------------------------------------------------------------------------
EQ8_T    <- seq(0, 24, length.out = BK_N_T)
EQ8_KERN <- local({
  sigma_bins <- 0.5 / (24 / BK_N_T)                 # SIGMA_HOURS / dt, as step 2
  win <- ceiling(6 * sigma_bins)
  if (win %% 2 == 0) win <- win + 1
  k <- dnorm(seq(-3 * sigma_bins, 3 * sigma_bins, length.out = win),
             mean = 0, sd = sigma_bins)
  k / sum(k)
})

eq8_mean_hour <- function(occ) {
  if (is.null(occ) || !any(is.finite(occ))) return(NA_real_)
  d  <- as.numeric(stats::filter(occ, EQ8_KERN, sides = 2))
  ok <- is.finite(d) & d > 0
  if (!any(ok)) return(NA_real_)
  sum(EQ8_T[ok] * d[ok]) / sum(d[ok])
}

# Mean hour per age-period cell, for one set of slice draws.
eq8_cell_hours <- function(SL) {
  tibble(
    AgeGroup  = CELLS$AgeGroup,
    YearBin   = CELLS$YearBin,
    mean_hour = vapply(SL, function(s) if (is.null(s)) NA_real_ else eq8_mean_hour(s$occ),
                       numeric(1), USE.NAMES = FALSE)
  )
}

# Endpoint-minus-baseline shift in mean hour, on the same reference as the
# distance and away-fraction axes.
eq8_shift <- function(SL, is_pre_pandemic = FALSE) {
  th <- eq8_cell_hours(SL)
  if (is_pre_pandemic) {
    tb <- th %>% filter(YearBin == PRE_START) %>% select(AgeGroup, h_base = mean_hour)
    te <- th %>% filter(YearBin == PRE_END)   %>% select(AgeGroup, h_end  = mean_hour)
  } else {
    tb <- th %>% filter(in_baseline(YearBin)) %>%
      group_by(AgeGroup) %>%
      summarise(h_base = mean(mean_hour, na.rm = TRUE), .groups = "drop")
    te <- th %>% filter(YearBin == max(YearBin)) %>% select(AgeGroup, h_end = mean_hour)
  }
  tb %>% inner_join(te, by = "AgeGroup") %>% mutate(d_hour = h_end - h_base)
}

compute_drift_boot_kde <- function(replicate_id, is_pre_pandemic = FALSE) {
  SL <- lapply(PREP, function(P) {
    if (is.null(P)) return(NULL)
    bk_slices(P, bk_draw(P$n_sess))
  })
  cell_means <- eq7_cell_means(SL, AGE_LEVELS, sort(unique(CELLS$YearBin)))

  if (is_pre_pandemic) {
    base <- cell_means %>% filter(YearBin == PRE_START) %>%
      select(AgeGroup, x_base = frac_away, y_base = mean_dist)
    ep <- cell_means %>% filter(YearBin == PRE_END) %>%
      select(AgeGroup, x_end = frac_away, y_end = mean_dist)
  } else {
    base <- cell_means %>% filter(in_baseline(YearBin)) %>%
      group_by(AgeGroup) %>%
      summarise(x_base = mean(frac_away, na.rm = TRUE),
                y_base = mean(mean_dist,  na.rm = TRUE), .groups = "drop")
    ep <- cell_means %>% filter(YearBin == max(YearBin)) %>%
      select(AgeGroup, x_end = frac_away, y_end = mean_dist)
  }

  base %>% inner_join(ep, by = "AgeGroup") %>%
    inner_join(eq8_shift(SL, is_pre_pandemic), by = "AgeGroup") %>%
    mutate(dx = x_end - x_base,
           dy = y_end - y_base,
           pct_change_distance = 100 * dy / y_base,
           replicate = replicate_id)
}

# ============================================================================
# 5.  RUN BOOTSTRAP (FULL SCENARIO)
# ============================================================================

cat("Running", B, "bootstrap replicates (Full Scenario)...\n")

boot_results <- map_dfr(seq_len(B), function(b) {
  if (b %% 50 == 0) cat("    replicate", b, "/", B, "
")
  compute_drift_boot_kde(b, is_pre_pandemic = FALSE)
})

cat("  OK Full Scenario Bootstrap complete

")

# NO RE-CENTRING. The replicates estimate the same functional as the plotted
# point, so the percentiles are the sampling distribution of that quantity.
# Re-centring the cloud on the KDE point estimate used to hide the fact that
# the two were different estimands; with the corrected sample it produced a
# point estimate lying outside its own 95% interval.
boot_results <- boot_results %>%
  left_join(kde_drift %>% select(AgeGroup, kde_dx = dx, kde_dy = dy,
                                 kde_x_base = x_base, kde_y_base = y_base),
            by = "AgeGroup")

# ============================================================================
# 6.  RUN BOOTSTRAP (PRE-PANDEMIC SCENARIO)
# ============================================================================

cat("Running", B, "bootstrap replicates (Pre-Pandemic)...\n")

boot_results_pre <- map_dfr(seq_len(B), function(b) {
  compute_drift_boot_kde(b, is_pre_pandemic = TRUE)
})

cat("  OK Pre-Pandemic Bootstrap complete

")

boot_results_pre <- boot_results_pre %>%
  left_join(kde_drift_pre %>% select(AgeGroup, kde_dx = dx, kde_dy = dy,
                                     kde_x_base = x_base, kde_y_base = y_base),
            by = "AgeGroup")

# ============================================================================
# 7.  OUTPUT SUMMARIES
# ============================================================================

# Both axes of Figure 6a get an interval. The away-fraction axis carries the
# "propensity to leave home" claim and previously shipped with no uncertainty
# at all, so its percentage drift is summarised here on the same replicates.
boot_summary <- boot_results %>%
  mutate(pct_change_away = 100 * dx / kde_x_base) %>%
  group_by(AgeGroup) %>%
  summarise(
    boot_median_pct = median(pct_change_distance, na.rm = TRUE),
    boot_lo         = quantile(pct_change_distance, 0.025, na.rm = TRUE),
    boot_hi         = quantile(pct_change_distance, 0.975, na.rm = TRUE),
    away_median_pct = median(pct_change_away, na.rm = TRUE),
    away_lo         = quantile(pct_change_away, 0.025, na.rm = TRUE),
    away_hi         = quantile(pct_change_away, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

out_summary <- kde_drift %>%
  mutate(kde_away_pct = 100 * dx / x_base) %>%
  select(AgeGroup, kde_pct = pct_change_distance, kde_away_pct) %>%
  left_join(boot_summary, by = "AgeGroup") %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

write.csv(out_summary, file.path(OUT_DIR, "bootstrap_summary.csv"), row.names = FALSE)
cat("  ✓ bootstrap_summary.csv saved\n")

# ---------------------------------------------------------------------------
# Eq. (8) marginal: mean time of day when away, endpoint against the same
# five-period baseline. Reviewer 2 asked for uncertainty on the three marginal
# quantities that anchor the claims; distance and away fraction are above, and
# this is the third. The point estimate is the unresampled slice draw, so it
# shares one code path with the replicates.
# ---------------------------------------------------------------------------
SL_point     <- lapply(PREP, function(P) if (is.null(P)) NULL else bk_slices(P, NULL))
timing_point <- eq8_shift(SL_point, is_pre_pandemic = FALSE) %>%
  select(AgeGroup, hour_base = h_base, hour_end = h_end, hour_shift = d_hour)

timing_ci <- boot_results %>%
  group_by(AgeGroup) %>%
  summarise(
    hour_shift_median = median(d_hour, na.rm = TRUE),
    hour_shift_lo     = quantile(d_hour, 0.025, na.rm = TRUE),
    hour_shift_hi     = quantile(d_hour, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

timing_summary <- timing_point %>%
  left_join(timing_ci, by = "AgeGroup") %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

write.csv(timing_summary, file.path(OUT_DIR, "bootstrap_timing_summary.csv"),
          row.names = FALSE)
cat("  OK bootstrap_timing_summary.csv saved
")
print(timing_summary)

# Pre-pandemic point estimates and bootstrap intervals.
# Written out because the response letter quotes these numbers; without them
# nothing in results/ lets a reader check Supplementary Figure S8.
pre_point <- kde_drift_pre %>%
  select(AgeGroup, x_base, y_base, x_end, y_end, dx, dy, pct_change_distance)

pre_ci <- boot_results_pre %>%
  group_by(AgeGroup) %>%
  summarise(
    dx_lo = quantile(dx, 0.025, na.rm = TRUE),
    dx_hi = quantile(dx, 0.975, na.rm = TRUE),
    dy_lo = quantile(dy, 0.025, na.rm = TRUE),
    dy_hi = quantile(dy, 0.975, na.rm = TRUE),
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
            select(replicate, AgeGroup, dx, dy),
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
        fan_xend  = kde_x_base + dx,
        fan_yend  = kde_y_base + dy
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
    # Wrapped onto two lines: this figure is exported at 5.8 in so it can
    # reach 7 pt in a 0.48\textwidth subfigure, and a single-line title of this
    # length runs off that canvas.
    title    = if (SCENARIO == "baseline")
                 "Drift vector stability: 500 bootstrap\nresamples (2022-2024 vs baseline)"
               else
                 paste0("Drift vector stability (2022-2024\nvs baseline): ",
                        get_scenario_label(SCENARIO)),
    # No subtitle. What the faint arrows, thick arrows and diamonds mean is
    # stated in the caption of Fig. 6a and of Supplementary Fig. S5, which is
    # where it belongs; repeating it inside the panel only costs space.
    x = "Fraction of day away from home",
    y = "Mean distance when away (km)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 11),
    legend.title    = element_text(size = 11),
    legend.key.size = unit(10, "pt"),
    plot.title      = element_text(face = "bold", size = 17),
    panel.border    = element_rect(color = "gray80", fill = NA, linewidth = 0.4)
  ) +
  # Two rows: five age groups plus the legend title overrun a 5.8 in canvas on
  # one row, and the last key gets clipped.
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

# Figure 6a and the two Supplementary Figure S5 panels all sit in
# 0.48\textwidth subfigures (about 3.2 in), so the canvas has to be small for
# base_size 16 to reach 7 pt on the page. See R2 comment 2.6.
ggsave(file.path(OUT_DIR, "drift_fan_full.png"), p_fan_full, width = 5.8, height = 4.7, dpi = 400)
if (!is.na(MANUSCRIPT_FIGURE)) {
  ggsave(file.path(get_manuscript_fig_dir(), MANUSCRIPT_FIGURE),
         p_fan_full, width = 5.8, height = 4.7, dpi = 400)
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
        fan_xend  = kde_x_base + dx,
        fan_yend  = kde_y_base + dy
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
    pct_boot = 100 * dy / kde_y_base
  ) %>%
  ggplot(aes(x = AgeGroup, y = pct_boot, fill = AgeGroup)) +
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
