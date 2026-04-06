# =======================
# I/O: folder-agnostic
# =======================
# Expect in_dir/out_dir from run_one_scenario.R
# (Fallback allows running this script standalone.)
if (!exists("in_dir") || !exists("out_dir")) {
  source("code/utils_io.R")
  scenario <- if (exists("scenario")) scenario else "baseline"
  dirs <- get_dirs(scenario)
  in_dir  <- dirs$in_dir
  out_dir <- dirs$out_dir
}

cat("Reading inputs from: ", in_dir, "\n")
cat("Writing outputs to:  ", out_dir, "\n")


baseline_kde <- readRDS(file.path(in_dir, "kde_baseline_by_cohort.rds"))
yearbin_kde  <- readRDS(file.path(in_dir, "kde_yearbin_by_cohort.rds"))
diff_kde     <- readRDS(file.path(in_dir, "kde_difference_by_cohort.rds"))


# Grid cell sizes (match Step 2)
dt <- 24 / 96
dd <- 60 / 96

library(ggplot2)
library(viridis)
library(scales)
library(dplyr)

# ======================================
# FIX 1: Harmonize column naming scheme
# ======================================
# Step 2 saves: baseline_kde(t,d,density,AgeGroup), yearbin_kde(..., YearBin), diff_kde(..., YearBin, density_diff)
if ("YearBin" %in% names(yearbin_kde) && !("year_bin" %in% names(yearbin_kde))) {
  yearbin_kde <- dplyr::rename(yearbin_kde, year_bin = YearBin)
}
if ("YearBin" %in% names(diff_kde) && !("year_bin" %in% names(diff_kde))) {
  diff_kde <- dplyr::rename(diff_kde, year_bin = YearBin)
}
# Standardize difference column
if ("density_diff" %in% names(diff_kde) && !("diff_density" %in% names(diff_kde))) {
  diff_kde <- dplyr::rename(diff_kde, diff_density = density_diff)
}

cat("\n", rep("=", 70), "\n", sep="")
cat("DIAGNOSTIC MODE - CHECKING YOUR DATA\n")
cat(rep("=", 70), "\n\n", sep="")

######################################################################
# DIAGNOSE THE DATA
######################################################################

cat("1. BASELINE_KDE structure:\n")
cat("   Rows:", nrow(baseline_kde), "\n")
cat("   Columns:", paste(names(baseline_kde), collapse=", "), "\n")
print(head(baseline_kde))

cat("\n2. Density statistics:\n")
cat("   Min density:", min(baseline_kde$density, na.rm=TRUE), "\n")
cat("   Max density:", max(baseline_kde$density, na.rm=TRUE), "\n")
cat("   Median density:", median(baseline_kde$density, na.rm=TRUE), "\n")

# Check distance distribution
cat("\n3. Distance distribution:\n")
d_summary <- baseline_kde %>%
  group_by(d) %>%
  summarize(
    n = n(),
    mean_dens = mean(density, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  arrange(d) %>%
  head(10)
print(d_summary)

# Check home vs away
cat("\n4. Home vs Away split:\n")
home_away <- baseline_kde %>%
  mutate(location = ifelse(d <= 0.5, "home", "away")) %>%
  group_by(location) %>%
  summarize(
    n_cells = n(),
    total_density = sum(density, na.rm=TRUE),
    mean_density = mean(density, na.rm=TRUE),
    .groups = "drop"
  )
print(home_away)

# Calculate mass properly
if (!"mass" %in% names(baseline_kde)) {
  baseline_kde <- baseline_kde %>% mutate(mass = density * dt * dd)
  cat("\n   Added mass column: density * dt * dd\n")
}

# Check total mass by age group
cat("\n5. Total probability mass by age group:\n")
mass_check <- baseline_kde %>%
  group_by(AgeGroup) %>%
  summarize(
    total_mass = sum(mass, na.rm=TRUE),
    .groups = "drop"
  )
print(mass_check)

# Check home probability calculation
cat("\n6. Home probability calculation:\n")
home_prob <- baseline_kde %>%
  mutate(is_home = d <= 0.5) %>%
  group_by(AgeGroup, t) %>%
  summarize(
    home_mass = sum(mass[is_home], na.rm=TRUE),
    total_mass = sum(mass, na.rm=TRUE),
    prob_home = home_mass / total_mass,
    .groups = "drop"
  )

cat("   Home probability range:", 
    sprintf("[%.3f, %.3f]", min(home_prob$prob_home), max(home_prob$prob_home)), "\n")

# Show a sample
cat("\n   Sample home probabilities for Age 31-55:\n")
print(home_prob %>% filter(AgeGroup == "31-55") %>% head(10))

# Check diff_kde structure
cat("\n7. DIFF_KDE structure:\n")
cat("   Rows:", nrow(diff_kde), "\n")
cat("   Columns:", paste(names(diff_kde), collapse=", "), "\n")
if (nrow(diff_kde) > 0) {
  print(head(diff_kde))
}

# Check yearbin_kde structure
cat("\n8. YEARBIN_KDE structure:\n")
cat("   Rows:", nrow(yearbin_kde), "\n")
cat("   Columns:", paste(names(yearbin_kde), collapse=", "), "\n")
if ("year_bin" %in% names(yearbin_kde)) {
  cat("   Periods:", paste(unique(yearbin_kde$year_bin), collapse=", "), "\n")
}

cat("\n", rep("=", 70), "\n", sep="")
cat("CREATING FIXED VISUALIZATIONS\n")
cat(rep("=", 70), "\n\n", sep="")



######################################################################
# PLOT 1: HOME PROBABILITY - FIXED
######################################################################
cat("1. Creating FIXED home probability plot...\n")

# Use the properly calculated home_prob from above
p_home <- ggplot(home_prob, aes(x = t, y = prob_home, color = AgeGroup)) +
  geom_line(linewidth = 1.2) +
  scale_color_viridis_d(option = "turbo", end = 0.9) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  labs(
    title = "Probability of Being at Home Over Time of Day",
    subtitle = "Baseline distribution (pooled 2007-2024) - FIXED CALCULATION",
    x = "Time of Day (hours)",
    y = "P(at home)",
    color = "Age Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right", panel.grid.minor = element_blank())

ggsave(file.path(out_dir, "kde_home_ACTUALLY_FIXED.png"), p_home, width = 10, height = 6, dpi = 300)

cat("   ✓ Saved: kde_home_ACTUALLY_FIXED.png\n")

######################################################################
# PLOT 2: AWAY TIME - AGGRESSIVE FILTERING
######################################################################
cat("\n2. Creating away-time plot with AGGRESSIVE filtering...\n")

# Filter to away time and use MUCH more aggressive density threshold
# Only keep the top part of the distribution
baseline_away <- baseline_kde %>% 
  filter(d > 0.5, density > 1e-20)

# Calculate very aggressive percentiles
dens_quantiles <- quantile(baseline_away$density, 
                           c(0.10, 0.90, 0.95, 0.99), 
                           na.rm = TRUE)

cat("   Density quantiles:\n")
cat(sprintf("     10th: %.2e\n", dens_quantiles[1]))
cat(sprintf("     90th: %.2e\n", dens_quantiles[2]))
cat(sprintf("     95th: %.2e\n", dens_quantiles[3]))
cat(sprintf("     99th: %.2e\n", dens_quantiles[4]))

# Use 10th to 95th percentile for better contrast
lower_limit <- dens_quantiles[1]  # 10th percentile
upper_limit <- dens_quantiles[3]  # 95th percentile

cat(sprintf("   Using color limits: [%.2e, %.2e]\n", lower_limit, upper_limit))

p_away <- ggplot(baseline_away, aes(x = t, y = d, fill = density)) +
  geom_tile() +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "log10",
    labels = label_scientific(),
    limits = c(lower_limit, upper_limit),
    oob = scales::squish
  ) +
  facet_wrap(~AgeGroup, ncol = 3) +
  labs(
    title = "Baseline Action-Space Density (Away Time, d > 0.5 km)",
    subtitle = sprintf("Log scale, 10th-95th percentile [%.1e to %.1e]", lower_limit, upper_limit),
    x = "Time of Day (hours)",
    y = "Distance from Home (km)",
    fill = "Density\n(log10)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )

ggsave(file.path(out_dir, "kde_away_AGGRESSIVE_FILTER.png"), p_away, width = 12, height = 8, dpi = 300)

cat("   ✓ Saved: kde_away_AGGRESSIVE_FILTER.png\n")

######################################################################
# PLOT 3: DIFFERENCE MAPS - IF DATA EXISTS
######################################################################
cat("\n3. Attempting to create difference maps...\n")

# Check what columns exist
cat("   diff_kde columns:", paste(names(diff_kde), collapse=", "), "\n")

# Try to find the right column
diff_col <- NULL
if ("diff_density" %in% names(diff_kde)) {
  diff_col <- "diff_density"
  cat("   Found 'diff_density' column\n")
} else if ("density" %in% names(diff_kde) && "year_bin" %in% names(diff_kde)) {
  cat("   WARNING: 'diff_kde' has 'density' but no 'diff_density'\n")
  cat("   This might be period-specific density, not differences!\n")
  cat("   Skipping difference plot - data structure unclear\n")
} else {
  cat("   ERROR: Cannot identify difference column\n")
  cat("   Available columns:", paste(names(diff_kde), collapse=", "), "\n")
}

if (!is.null(diff_col)) {
  diff_kde <- diff_kde %>% mutate(diff_value = .data[[diff_col]])
  
  diff_away <- diff_kde %>%
    filter(d > 0.5, is.finite(diff_value))
  
  if (nrow(diff_away) > 0) {
    diff_quantiles <- quantile(diff_away$diff_value, c(0.05, 0.95), na.rm = TRUE)
    max_abs <- max(abs(diff_quantiles))
    
    cat(sprintf("   Difference range: [%.2e, %.2e]\n",
                min(diff_away$diff_value), max(diff_away$diff_value)))
    cat(sprintf("   Using symmetric limits: [%.2e, %.2e]\n", -max_abs, max_abs))
    
    p_diff <- ggplot(diff_away, aes(x = t, y = d, fill = diff_value)) +
      geom_tile() +
      scale_fill_gradient2(
        low = "blue", mid = "white", high = "red",
        midpoint = 0,
        limits = c(-max_abs, max_abs),
        oob = scales::squish,
        labels = label_scientific()
      ) +
      facet_grid(year_bin ~ AgeGroup) +
      labs(
        title = "Difference in Action-Space Density (Period - Baseline)",
        subtitle = "Red = increased, Blue = decreased, 5th-95th percentile",
        x = "Time of Day (hours)",
        y = "Distance from Home (km)",
        fill = "Δ Density"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        legend.position = "right",
        strip.background = element_rect(fill = "grey90", color = NA),
        strip.text = element_text(size = 8)
      )
    
    ggsave(file.path(out_dir, "kde_DIFFERENCE_maps_FIXED.png"), p_diff, width = 14, height = 10, dpi = 300)
    
    cat("   ✓ Saved: kde_DIFFERENCE_maps_FIXED.png\n")
  } else {
    cat("   ✗ No data after filtering\n")
  }
} else {
  cat("   ✗ Skipping difference maps - data issue\n")
}

######################################################################
# PLOT 4: YEAR-BIN COMPARISON
######################################################################
cat("\n4. Creating year-bin comparison...\n")

if ("year_bin" %in% names(yearbin_kde)) {
  if (!"mass" %in% names(yearbin_kde)) {
    yearbin_kde <- yearbin_kde %>% mutate(mass = density * dt * dd)
  }
  
  periods <- sort(unique(yearbin_kde$year_bin))
  cat("   Available periods:", paste(periods, collapse=", "), "\n")
  
  if (length(periods) >= 2) {
    selected <- c(periods[1], periods[length(periods)])
    cat("   Comparing:", selected[1], "vs", selected[2], "\n")
    
    yearbin_away <- yearbin_kde %>%
      filter(year_bin %in% selected, d > 0.5, density > 1e-20)
    
    if (nrow(yearbin_away) > 0) {
      year_quantiles <- quantile(yearbin_away$density, c(0.10, 0.95), na.rm = TRUE)
      
      p_yearbin <- ggplot(yearbin_away, aes(x = t, y = d, fill = density)) +
        geom_tile() +
        scale_fill_viridis_c(
          option = "plasma",
          trans = "log10",
          limits = c(year_quantiles[1], year_quantiles[2]),
          oob = scales::squish
        ) +
        facet_grid(year_bin ~ AgeGroup) +
        labs(
          title = "Action-Space Density: Early vs Late Period",
          subtitle = "Away time, 10th-95th percentile",
          x = "Time of Day (hours)",
          y = "Distance from Home (km)"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          legend.position = "right",
          strip.background = element_rect(fill = "grey90", color = NA),
          strip.text = element_text(face = "bold", size = 9)
        )
      
      ggsave(file.path(out_dir, "kde_YEARBIN_comparison_FIXED.png"), p_yearbin, width = 14, height = 8, dpi = 300)
      
      cat("   ✓ Saved: kde_YEARBIN_comparison_FIXED.png\n")
    } else {
      cat("   ✗ No data after filtering\n")
    }
  } else {
    cat("   ✗ Not enough periods for comparison\n")
  }
} else {
  cat("   ✗ year_bin column not found\n")
}

######################################################################
# SUMMARY
######################################################################
cat("\n", rep("=", 70), "\n", sep="")
cat("COMPLETE!\n")
cat(rep("=", 70), "\n\n", sep="")

cat("📁 Check your output directory:\n")
cat("   ", out_dir, "\n\n")

cat("📊 Files created:\n")
cat("  1. kde_home_ACTUALLY_FIXED.png - Should show proper 60-100% curves\n")
cat("  2. kde_away_AGGRESSIVE_FILTER.png - 10th-95th percentile for contrast\n")
if (!is.null(diff_col)) {
  cat("  3. kde_DIFFERENCE_maps_FIXED.png - Red/blue difference maps\n")
}
if ("year_bin" %in% names(yearbin_kde)) {
  cat("  4. kde_YEARBIN_comparison_FIXED.png - Early vs late periods\n")
}

cat("\n💡 If plots still look wrong, review the diagnostic output above!\n")
cat(rep("=", 70), "\n", sep="")
######################################################################