######################################################################
# STEP 7 – URBAN ROBUSTNESS: SETTLEMENT-TYPE COMPARISON
#
# PURPOSE
# -------
# Produces a revised robustness figure comparing drift vectors across
# city-size scenarios on a shared scale, replacing the previous SI4
# which showed nested subsets independently.
#
# The existing kernels were estimated on nested subsets:
#   baseline    = all individuals (reference)
#   city_10000  = residents of cities >= 10,000
#   city_25000  = residents of cities >= 25,000
#   city_50000  = residents of cities >= 50,000
#   city_100000 = residents of cities >= 100,000
#
# This script compares drift vectors across all five scenarios on a
# shared axis system, making visible whether the age-group ordering
# and drift direction are preserved across urban restriction thresholds.
#
# Note: Truly non-nested strata (e.g. rural-only, small-urban-only)
# would require re-running step2_kernel_generation.R on the raw
# microdata with stratum-specific filters. The present approach uses
# the best evidence available from the shared kernel artifacts.
#
# INPUTS
# ------
# results/<scenario>/action_space_metrics_TRIPWEIGHTED.csv  (from step4)
#   for scenarios: baseline, city_10000, city_25000, city_50000, city_100000
#
# OUTPUTS  (written to results/urban_robustness/)
# -------
# urban_drift_comparison.png        — 5-panel drift vector comparison
# urban_drift_summary.png           — dot plot: drift magnitude by scenario
# urban_drift_data.csv              — underlying drift values
#
# RUN
# ---
# Rscript code/step7_urban_robustness.R
######################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(grid)
  library(purrr)
})

source("code/utils_io.R")

cat("\n=== STEP 7: URBAN ROBUSTNESS COMPARISON ===\n\n")

# ============================================================================
# CONFIG
# ============================================================================

SCENARIOS <- c("baseline", "city_10000", "city_25000", "city_50000", "city_100000")

SCENARIO_LABELS <- c(
  baseline    = "All individuals\n(full sample)",
  city_10000  = "Cities \u2265 10,000\n(urban restricted)",
  city_25000  = "Cities \u2265 25,000",
  city_50000  = "Cities \u2265 50,000",
  city_100000 = "Cities \u2265 100,000\n(large urban only)"
)

AGE_LEVELS <- c("10-17", "18-30", "31-55", "56-65", "66+")

OUT_DIR <- file.path("results", "urban_robustness")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# ============================================================================
# HELPERS
# ============================================================================

normalize_dash <- function(x) {
  gsub("[\u2012\u2013\u2014\u2212]", "-", x, perl = TRUE)
}

extract_end_year <- function(x) {
  x2 <- normalize_dash(x)
  as.numeric(sub(".*?(\\d{4})[^\\d]*$", "\\1", x2))
}

# ============================================================================
# LOAD STEP 4 OUTPUTS FOR ALL SCENARIOS
# ============================================================================

cat("Loading step 4 outputs...\n")

load_scenario <- function(sc) {
  path <- file.path("results", sc, "action_space_metrics_TRIPWEIGHTED.csv")
  if (!file.exists(path)) {
    warning("Missing step 4 output for scenario '", sc,
            "'. Run the pipeline for this scenario first.\n  Expected: ", path)
    return(NULL)
  }
  df <- read.csv(path, check.names = FALSE) %>%
    mutate(
      scenario = sc,
      scenario_label = SCENARIO_LABELS[sc],
      Period = normalize_dash(Period)
    )
  cat("  \u2713 Loaded:", sc, "(", nrow(df), "rows)\n")
  df
}

all_data <- map(SCENARIOS, load_scenario) %>%
  compact() %>%
  bind_rows()

loaded_scenarios <- unique(all_data$scenario)
cat("\nLoaded", length(loaded_scenarios), "of", length(SCENARIOS), "scenarios.\n\n")

if (length(loaded_scenarios) < 2) {
  stop("Need at least 2 scenarios to compare. ",
       "Run step4 for the missing scenarios first.")
}

# ============================================================================
# COMPUTE DRIFT VECTORS FOR EACH SCENARIO
# ============================================================================

cat("Computing drift vectors per scenario...\n")

compute_drift <- function(df) {
  df <- df %>%
    filter(Period != "Baseline (2007-2009)") %>%
    mutate(end_year = extract_end_year(Period))

  latest_period <- df %>%
    distinct(Period, end_year) %>%
    arrange(desc(end_year)) %>%
    slice(1) %>%
    pull(Period)

  baseline <- df %>%
    group_by(AgeGroup) %>%
    summarise(
      x_base = mean(fraction_away,        na.rm = TRUE),
      y_base = mean(mean_distance_active,  na.rm = TRUE),
      .groups = "drop"
    )

  endpoint <- df %>%
    filter(Period == latest_period) %>%
    select(AgeGroup, x_end = fraction_away, y_end = mean_distance_active)

  baseline %>%
    inner_join(endpoint, by = "AgeGroup") %>%
    mutate(
      dx  = x_end - x_base,
      dy  = y_end - y_base,
      pct_change_distance = 100 * dy / y_base,
      pct_change_time     = 100 * dx / x_base,
      magnitude           = sqrt(dx^2 + dy^2),
      latest_period       = latest_period
    )
}

drift_all <- all_data %>%
  group_by(scenario, scenario_label) %>%
  group_modify(~ compute_drift(.x)) %>%
  ungroup() %>%
  mutate(
    AgeGroup = factor(AgeGroup, levels = AGE_LEVELS),
    scenario = factor(scenario, levels = SCENARIOS),
    scenario_label = factor(scenario_label,
                            levels = SCENARIO_LABELS[SCENARIOS[SCENARIOS %in% loaded_scenarios]])
  )

cat("  \u2713 Drift vectors computed for all scenarios\n\n")

# Print summary
cat("--- Drift summary (% change in mean distance, baseline \u2192 2022-24) ---\n")
drift_all %>%
  select(scenario, AgeGroup, pct_change_distance, pct_change_time) %>%
  arrange(scenario, AgeGroup) %>%
  print(n = Inf)
cat("\n")

# ============================================================================
# FIGURE 1: 5-PANEL DRIFT VECTOR COMPARISON
# ============================================================================
# Each panel = one scenario, identical axis limits for comparability.
# Arrows run from the period-averaged baseline (diamond) to the 2022-24
# endpoint. Colour encodes age group, consistent across panels.

cat("Creating 5-panel drift comparison figure...\n")

# Shared axis limits (computed from baseline scenario, padded slightly)
base_data <- all_data %>% filter(scenario == "baseline", Period != "Baseline (2007-2009)")

x_pad <- 0.02
y_pad <- 0.5

x_lim <- range(base_data$fraction_away,       na.rm = TRUE) + c(-x_pad, x_pad)
y_lim <- range(base_data$mean_distance_active, na.rm = TRUE) + c(-y_pad, y_pad)

age_colours <- c(
  "10-17" = "#440154",
  "18-30" = "#3B528B",
  "31-55" = "#21908C",
  "56-65" = "#5DC963",
  "66+"   = "#FDE725"
)

p_panels <- ggplot(drift_all, aes(color = AgeGroup)) +

  # Arrow: baseline diamond -> 2022-24 endpoint
  geom_segment(
    aes(x = x_base, y = y_base, xend = x_end, yend = y_end),
    arrow = arrow(type = "closed", length = unit(0.10, "in"), angle = 20),
    linewidth = 1.2, alpha = 0.9, lineend = "round"
  ) +

  # Baseline position (diamond)
  geom_point(aes(x = x_base, y = y_base), shape = 18, size = 3.5, alpha = 0.8) +

  # Endpoint position (circle)
  geom_point(aes(x = x_end, y = y_end), shape = 16, size = 2.5) +

  # Age group label at endpoint
  geom_text(
    aes(x = x_end, y = y_end, label = AgeGroup),
    size = 3, fontface = "bold", nudge_y = 0.3, show.legend = FALSE,
    color = "gray30"
  ) +

  # Reference lines at (0, 0) drift
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray70", linewidth = 0.4) +

  facet_wrap(~ scenario_label, ncol = 3) +

  scale_color_manual(values = age_colours, name = "Age group") +
  scale_x_continuous(labels = label_percent(accuracy = 1),
                     limits = x_lim) +
  scale_y_continuous(limits = y_lim) +

  labs(
    title    = "Drift vector robustness across urban restriction thresholds",
    subtitle = paste0(
      "Each arrow: period-averaged baseline (diamond) \u2192 2022\u20132024 endpoint.\n",
      "Identical axis scales across panels. Colour = age group."
    ),
    x = "Fraction of day away from home",
    y = "Mean distance when away (km)"
  ) +

  theme_minimal(base_size = 12) +
  theme(
    strip.text       = element_text(face = "bold", size = 10),
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(size = 9, color = "gray40"),
    panel.border     = element_rect(color = "gray80", fill = NA, linewidth = 0.4)
  )

ggsave(file.path(OUT_DIR, "urban_drift_comparison.png"),
       p_panels, width = 13, height = 9, dpi = 300)
cat("  \u2713 urban_drift_comparison.png\n")

# ============================================================================
# FIGURE 2: SUMMARY — % DISTANCE CHANGE BY SCENARIO AND AGE GROUP
# ============================================================================
# Dot + range chart showing pct_change_distance for each age group (x-axis)
# across scenarios (colour/shape). Horizontal zero line = no change.

cat("Creating drift summary dot plot...\n")

p_summary <- drift_all %>%
  mutate(scenario_label_short = recode(as.character(scenario),
    baseline    = "All",
    city_10000  = "\u226510k",
    city_25000  = "\u226525k",
    city_50000  = "\u226550k",
    city_100000 = "\u2265100k"
  ),
  scenario_label_short = factor(scenario_label_short,
    levels = c("All", "\u226510k", "\u226525k", "\u226550k", "\u2265100k"))
  ) %>%
  ggplot(aes(x = AgeGroup, y = pct_change_distance,
             color = scenario_label_short, shape = scenario_label_short,
             group = scenario_label_short)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_line(alpha = 0.5, linewidth = 0.8) +
  geom_point(size = 3.5) +
  scale_color_viridis_d(option = "D", name = "Min. city size") +
  scale_shape_manual(
    values = c(16, 17, 15, 18, 8),
    name   = "Min. city size"
  ) +
  scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")) +
  labs(
    title    = "% change in mean distance: robustness across urban thresholds",
    subtitle = paste0(
      "Each point: % change from period-averaged baseline to 2022\u20132024, by age group.\n",
      "Lines connect same scenario across age groups."
    ),
    x = "Age group",
    y = "% change in mean distance when away"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title      = element_text(face = "bold"),
    plot.subtitle   = element_text(size = 9, color = "gray40")
  )

ggsave(file.path(OUT_DIR, "urban_drift_summary.png"),
       p_summary, width = 10, height = 6, dpi = 300)
cat("  \u2713 urban_drift_summary.png\n")

# ============================================================================
# SAVE DATA TABLE
# ============================================================================

out_table <- drift_all %>%
  select(scenario, AgeGroup,
         x_base, y_base, x_end, y_end,
         pct_change_distance, pct_change_time, magnitude) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

write.csv(out_table, file.path(OUT_DIR, "urban_drift_data.csv"), row.names = FALSE)
cat("  \u2713 urban_drift_data.csv\n\n")

# ============================================================================
# CONSISTENCY CHECKS
# ============================================================================

cat("--- Consistency check: age-group ordering preserved? ---\n")
cat("(Spatial expansion ranking should be: 18-30 > 10-17 \u2248 31-55 > 56-65 \u2248 66+)\n\n")

drift_all %>%
  select(scenario, AgeGroup, pct_change_distance) %>%
  pivot_wider(names_from = scenario, values_from = pct_change_distance) %>%
  arrange(desc(baseline)) %>%
  mutate(across(where(is.numeric), ~ round(., 1))) %>%
  print()

cat("\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat(rep("=", 65), "\n", sep = "")
cat("STEP 7 COMPLETE: Urban robustness comparison\n")
cat(rep("=", 65), "\n\n", sep = "")
cat("Outputs written to: ", OUT_DIR, "\n")
cat("  urban_drift_comparison.png  — 5-panel drift figure (replaces SI4)\n")
cat("  urban_drift_summary.png     — % change summary across thresholds\n")
cat("  urban_drift_data.csv        — underlying data\n\n")

cat("Interpretation guidance:\n")
cat("  If drift direction and age-group ordering are consistent across\n")
cat("  panels, this confirms that the headline results are not driven by\n")
cat("  rural/urban composition.\n")
cat("  Larger absolute magnitudes in city-restricted panels are expected\n")
cat("  because rural individuals (who have less spatial expansion) are\n")
cat("  progressively excluded.\n\n")

cat("Note on non-nested strata:\n")
cat("  Truly non-nested strata (rural-only, small-urban-only, etc.) would\n")
cat("  require re-running step2_kernel_generation.R on raw microdata with\n")
cat("  stratum-specific filters. These results use the best evidence\n")
cat("  available from the shared kernel artifacts.\n\n")
