######################################################################
# SHARP DIFFERENCE MAPS — TRANSPOSED (Age Groups as ROWS)
# Age groups go vertically, year periods go horizontally
######################################################################

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

cat("Reading inputs from: ", in_dir, "\n")
cat("Writing outputs to:  ", out_dir, "\n")

diff_kde <- readRDS(file.path(in_dir, "kde_difference_by_cohort.rds"))


library(ggplot2)
library(dplyr)
library(scales)

# --- 1) Harmonize names UNCONDITIONALLY (so we can use a single reference) ---
if ("YearBin"      %in% names(diff_kde)) diff_kde <- dplyr::rename(diff_kde, year_bin    = YearBin)
if ("density_diff" %in% names(diff_kde)) diff_kde <- dplyr::rename(diff_kde, diff_density = density_diff)

# sanity
stopifnot(all(c("t","d","AgeGroup","year_bin","diff_density") %in% names(diff_kde)))

cat("\n=== CREATING TRANSPOSED DIFFERENCE MAPS ===\n")
cat("Layout: Age groups as ROWS, year periods as COLUMNS\n")
cat("Rows in diff_kde:", nrow(diff_kde), "\n")

# --- 2) Filter and compute robust color limits ---
diff_away <- diff_kde %>%
  filter(is.finite(diff_density), d > 0.5)

cat("Rows after away-filter:", nrow(diff_away), "\n")
if (nrow(diff_away) == 0) stop("No rows after filtering; check 'd' units (>0.5 km) and columns.")

q <- quantile(diff_away$diff_density, c(0.05, 0.95), na.rm = TRUE)
max_abs <- max(abs(q))
if (!is.finite(max_abs) || max_abs == 0) max_abs <- max(abs(diff_away$diff_density), na.rm = TRUE)
if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1e-6
color_limits <- c(-max_abs, max_abs)

cat(sprintf("Color limits (5–95%%): [%.2e, %.2e]\n\n", color_limits[1], color_limits[2]))

# --- 3) Build plots (TRANSPOSED: AgeGroup as ROWS, year_bin as COLUMNS) ---
base_theme <- theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text.y = element_text(size = 10, face = "bold"),  # Age groups on left
    strip.text.x = element_text(size = 9),                   # Years on top
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5)
  )

# SHARP version (5-95 percentile)
p_sharp <- ggplot(diff_away, aes(x = t, y = d, fill = diff_density)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limits = color_limits, oob = squish,
                       labels = label_scientific()) +
  facet_grid(AgeGroup ~ year_bin) +   # *** TRANSPOSED: rows=AgeGroup, cols=year_bin ***
  labs(title = "Change in Action-Space Density Over Time by Age Group",
       subtitle = "Each row = one cohort across time periods. Red = increased density, Blue = decreased density",
       x = "Time of Day (hours)", y = "Distance from Home (km)", fill = "Δ Density") +
  base_theme

ggsave(file.path(out_dir, "kde_DIFFERENCE_by_cohort_SHARP.png"),
       p_sharp, width = 18, height = 12, dpi = 600, type = "cairo")
cat("✓ Saved: kde_DIFFERENCE_by_cohort_SHARP.png\n")

# Aggressive contrast (10–90%)
q_agg <- quantile(diff_away$diff_density, c(0.10, 0.90), na.rm = TRUE)
max_abs_agg <- max(abs(q_agg)); if (!is.finite(max_abs_agg) || max_abs_agg == 0) max_abs_agg <- max_abs

p_sharp_agg <- ggplot(diff_away, aes(x = t, y = d, fill = diff_density)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limits = c(-max_abs_agg, max_abs_agg), oob = squish,
                       labels = label_scientific()) +
  facet_grid(AgeGroup ~ year_bin) +   # *** TRANSPOSED ***
  labs(title = "Change in Action-Space Density — ENHANCED CONTRAST",
       subtitle = "Each row = one cohort. 10th–90th percentile scaling, 600 DPI",
       x = "Time of Day (hours)", y = "Distance from Home (km)", fill = "Δ Density") +
  base_theme

ggsave(file.path(out_dir, "kde_DIFFERENCE_by_cohort_aggressive_SHARP.png"),
       p_sharp_agg, width = 18, height = 12, dpi = 600, type = "cairo")
cat("✓ Saved: kde_DIFFERENCE_by_cohort_aggressive_SHARP.png\n")

# Publication quality (1200 DPI)
p_publication <- ggplot(diff_away, aes(x = t, y = d, fill = diff_density)) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradient2(low = "#0571b0", mid = "white", high = "#ca0020",
                       midpoint = 0, limits = color_limits, oob = squish,
                       labels = label_scientific()) +
  facet_grid(AgeGroup ~ year_bin) +   # *** TRANSPOSED ***
  labs(title = "Change in Action-Space Density Over Time by Age Group",
       subtitle = "Publication quality (1200 DPI). Each row = one cohort. Red = increased, Blue = decreased",
       x = "Time of Day (hours)", y = "Distance from Home (km)", fill = "Δ Density") +
  base_theme + theme(base_size = 12)

ggsave(file.path(out_dir, "kde_DIFFERENCE_by_cohort_PUBLICATION.png"),
       p_publication, width = 18, height = 12, dpi = 1200, type = "cairo")
cat("✓ Saved: kde_DIFFERENCE_by_cohort_PUBLICATION.png\n")

cat("\n=== TRANSPOSITION COMPLETE ===\n")
cat("Layout now:\n")
cat("  Rows: Age Groups (10-17, 18-30, 31-55, etc.)\n")
cat("  Columns: Year Periods (2007-2009, 2010-2012, etc.)\n")
cat("Each horizontal strip shows one cohort's evolution across time\n\n")