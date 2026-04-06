######################################################################
# STEP 6 - PATH COMPLEXITY METRICS
# Calculate tortuosity and directional dispersion for drift analysis
# Outputs include visualizations, CSV data, and LaTeX tables
######################################################################

library(dplyr)
library(ggplot2)
library(viridis)

cat("\n=== STEP 6: COMPUTING PATH COMPLEXITY METRICS ===\n\n")

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
output_dir <- out_dir

cat("\n=== STEP 6: LOADING STEP 5 OBJECTS ===\n")
all_sequential_file <- file.path(output_dir, "all_sequential.rds")
drift_vectors_file  <- file.path(output_dir, "drift_vectors.rds")

if (!file.exists(all_sequential_file) || !file.exists(drift_vectors_file)) {
  stop(
    "Step 6 cannot find Step 5 outputs in: ", output_dir, "\n",
    "Missing:\n",
    " - ", all_sequential_file, "\n",
    " - ", drift_vectors_file, "\n",
    "Run: Rscript code/run_one_scenario.R <scenario>\n"
  )
}

all_sequential <- readRDS(all_sequential_file)
drift_vectors  <- readRDS(drift_vectors_file)

# ============================================================================
# METRIC 1: TORTUOSITY (Path Length / Displacement Ratio)
# ============================================================================

cat("Computing tortuosity metrics...\n")

# Calculate total path length for each age group
path_metrics <- all_sequential %>%
  mutate(
    segment_length = sqrt(dx^2 + dy^2)  # Length of each sequential vector
  ) %>%
  group_by(AgeGroup) %>%
  summarise(
    path_length = sum(segment_length, na.rm = TRUE),
    n_segments = n(),
    mean_segment_length = mean(segment_length, na.rm = TRUE),
    .groups = "drop"
  )

# Join with endpoint displacement
path_metrics <- path_metrics %>%
  left_join(
    drift_vectors %>% select(AgeGroup, endpoint_displacement = magnitude),
    by = "AgeGroup"
  ) %>%
  mutate(
    tortuosity = path_length / endpoint_displacement,
    # Tortuosity interpretation
    path_type = case_when(
      tortuosity < 1.1 ~ "Nearly straight",
      tortuosity < 1.3 ~ "Slightly winding",
      tortuosity < 1.5 ~ "Moderately winding",
      TRUE ~ "Highly tortuous"
    )
  )

cat("\n=== TORTUOSITY RESULTS ===\n")
print(path_metrics)

cat("\nInterpretation:\n")
cat("- Tortuosity = 1.0: Perfectly straight path\n")
cat("- Tortuosity = 1.5: Path is 50% longer than straight line\n")
cat("- Higher values indicate more meandering/fluctuations\n\n")

# ============================================================================
# METRIC 2: DIRECTIONAL DISPERSION (Angular Consistency)
# ============================================================================

cat("Computing directional dispersion...\n")

# Calculate angle for each sequential vector and circular statistics
directional_stats <- all_sequential %>%
  mutate(
    angle_rad = atan2(dy, dx),  # Angle in radians [-π, π]
    angle_deg = angle_rad * 180 / pi  # Convert to degrees
  ) %>%
  group_by(AgeGroup) %>%
  summarise(
    # Circular mean direction (resultant vector)
    mean_angle_rad = atan2(sum(sin(angle_rad)), sum(cos(angle_rad))),
    mean_angle_deg = mean_angle_rad * 180 / pi,
    
    # Mean resultant length (R) - measure of concentration
    # R = 1: all vectors point exactly the same direction
    # R = 0: vectors point in all directions (uniform dispersion)
    R = sqrt(sum(sin(angle_rad))^2 + sum(cos(angle_rad))^2) / n(),
    
    # Circular variance (1 - R)
    circular_variance = 1 - R,
    
    # Circular standard deviation (degrees)
    # This is a more intuitive measure of angular spread
    circular_sd = sqrt(-2 * log(R)) * 180 / pi,
    
    # Alternative: just standard deviation of angles (for small dispersions)
    # Note: this is approximate and not strictly correct for circular data
    angle_sd_approx = sd(angle_deg, na.rm = TRUE),
    
    n_transitions = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Consistency interpretation
    consistency = case_when(
      R > 0.95 ~ "Highly consistent",
      R > 0.85 ~ "Moderately consistent",
      R > 0.70 ~ "Somewhat variable",
      TRUE ~ "Highly variable"
    )
  )

cat("\n=== DIRECTIONAL DISPERSION RESULTS ===\n")
print(directional_stats)

cat("\nInterpretation:\n")
cat("- R (Mean Resultant Length): 0-1 scale\n")
cat("  * R close to 1: All vectors point in nearly same direction (consistent)\n")
cat("  * R close to 0: Vectors point in many different directions (dispersed)\n")
cat("- Circular SD: Angular spread in degrees\n")
cat("  * Low values: Tight angular clustering\n")
cat("  * High values: Wide angular spread\n\n")

# ============================================================================
# COMBINED METRICS TABLE
# ============================================================================

combined_metrics <- path_metrics %>%
  left_join(directional_stats, by = "AgeGroup") %>%
  select(AgeGroup, 
         endpoint_displacement, 
         path_length, 
         tortuosity, 
         path_type,
         R, 
         circular_sd, 
         consistency)

cat("\n=== COMBINED PATH COMPLEXITY METRICS ===\n")
print(combined_metrics)

# ============================================================================
# VISUALIZATIONS
# ============================================================================

cat("\nCreating visualizations...\n")

# Plot 1: Tortuosity by age group
p_tortuosity <- ggplot(path_metrics, aes(x = AgeGroup, y = tortuosity, fill = AgeGroup)) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", tortuosity)), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_viridis_d(option = "D") +
  labs(title = "Path Tortuosity by Age Group",
       subtitle = "Ratio of actual path length to straight-line displacement (2007-2024)",
       x = "Age Group",
       y = "Tortuosity Ratio",
       caption = "Value of 1.0 = straight path, >1.0 = winding/fluctuating path") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

print(p_tortuosity)

ggsave(file.path(output_dir, "path_tortuosity.png"), 
       p_tortuosity, width = 10, height = 6, dpi = 300)

# Plot 2: Directional consistency (R value)
p_consistency <- ggplot(directional_stats, aes(x = AgeGroup, y = R, fill = AgeGroup)) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "blue", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.3f", R)), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_viridis_d(option = "D") +
  scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.2)) +
  labs(title = "Directional Consistency by Age Group",
       subtitle = "Mean resultant length (R) - higher values indicate more consistent direction",
       x = "Age Group",
       y = "R (Directional Consistency)",
       caption = "R = 1: All vectors same direction, R = 0: Uniform angular dispersion") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

print(p_consistency)

ggsave(file.path(output_dir, "directional_consistency.png"), 
       p_consistency, width = 10, height = 6, dpi = 300)

# Plot 3: Combined view - tortuosity vs directional consistency
p_combined <- ggplot(combined_metrics, 
                     aes(x = tortuosity, y = R, color = AgeGroup, size = endpoint_displacement)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = AgeGroup), vjust = -1.2, size = 4, fontface = "bold", show.legend = FALSE) +
  scale_color_viridis_d(option = "D") +
  scale_size_continuous(range = c(3, 10), name = "Displacement\n(km)") +
  labs(title = "Path Complexity: Tortuosity vs Directional Consistency",
       subtitle = "Bubble size represents total displacement magnitude",
       x = "Tortuosity (path length / displacement)",
       y = "R (Directional Consistency)",
       caption = "Top-left: Straight & consistent | Bottom-right: Winding & variable") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

print(p_combined)

ggsave(file.path(output_dir, "path_complexity_combined.png"), 
       p_combined, width = 11, height = 7, dpi = 300)

# ============================================================================
# SAVE CSV RESULTS
# ============================================================================

write.csv(combined_metrics,
          file.path(output_dir, "path_complexity_metrics.csv"),
          row.names = FALSE)

write.csv(all_sequential %>%
            mutate(angle_deg = atan2(dy, dx) * 180 / pi,
                   segment_length = sqrt(dx^2 + dy^2)),
          file.path(output_dir, "sequential_vectors_with_angles.csv"),
          row.names = FALSE)

# ============================================================================
# GENERATE LATEX TABLES
# ============================================================================

cat("\nGenerating LaTeX tables...\n")

# Helper function to format numbers for LaTeX
fmt <- function(x, digits = 3) {
  sprintf(paste0("%.", digits, "f"), x)
}

# --- TABLE 1: Path Complexity Metrics (Main/Supplementary) ---

latex_table_1 <- paste0(
  "% Path Complexity Metrics - LaTeX Table for Publication\n",
  "% Auto-generated by Step 6\n\n",
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{\\textbf{Path complexity metrics for cohort trajectories (2007--2024).} \n",
  "Tortuosity measures the ratio of actual path length to straight-line displacement; \n",
  "values near 1.0 indicate direct movement while higher values reflect meandering or \n",
  "period-specific fluctuations. The mean resultant length (R) quantifies directional \n",
  "consistency, with values near 1.0 indicating all period-to-period transitions point \n",
  "in similar directions. Circular standard deviation provides an intuitive measure of \n",
  "angular spread.}\n",
  "\\label{tab:path_complexity}\n\n",
  "\\begin{tabular}{lccccc}\n",
  "\\toprule\n",
  "\\textbf{Age Group} & \n",
  "\\textbf{Displacement} & \n",
  "\\textbf{Path Length} & \n",
  "\\textbf{Tortuosity} & \n",
  "\\textbf{R} & \n",
  "\\textbf{Circular SD} \\\\\n",
  " & (km) & (km) & (ratio) & & (degrees) \\\\\n",
  "\\midrule\n"
)

# Add data rows
for (i in 1:nrow(combined_metrics)) {
  row <- combined_metrics[i, ]
  latex_table_1 <- paste0(
    latex_table_1,
    row$AgeGroup, " & ",
    fmt(row$endpoint_displacement, 3), " & ",
    fmt(row$path_length, 3), " & ",
    fmt(row$tortuosity, 2), " & ",
    fmt(row$R, 3), " & ",
    fmt(row$circular_sd, 1), " \\\\\n"
  )
}

latex_table_1 <- paste0(
  latex_table_1,
  "\\bottomrule\n",
  "\\end{tabular}\n\n",
  "\\vspace{0.3cm}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item \\textit{Displacement}: Straight-line distance from baseline (2007--09) to final period (2022--24).\n",
  "\\item \\textit{Path Length}: Total distance traveled through action-space across all period-to-period transitions.\n",
  "\\item \\textit{Tortuosity}: Path length / displacement. Values of 1.0 indicate perfectly straight trajectories; \n",
  "higher values indicate deviation from direct movement.\n",
  "\\item \\textit{R (Mean Resultant Length)}: Circular statistic measuring directional consistency (range: 0--1). \n",
  "Higher values indicate period-to-period vectors point in more similar directions.\n",
  "\\item \\textit{Circular SD}: Angular dispersion of trajectory directions in degrees. \n",
  "Lower values indicate tighter angular clustering.\n",
  "\\end{tablenotes}\n",
  "\\end{table}\n"
)

# Write to file
writeLines(latex_table_1, file.path(output_dir, "table_path_complexity.tex"))
cat("  ✓ table_path_complexity.tex\n")

# --- TABLE 2: Simplified version ---

latex_table_2 <- paste0(
  "% Simplified Path Complexity Table\n",
  "% Auto-generated by Step 6\n\n",
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{\\textbf{Path complexity metrics for cohort trajectories (2007--2024).}}\n",
  "\\label{tab:path_complexity_simple}\n\n",
  "\\begin{tabular}{lcccc}\n",
  "\\toprule\n",
  "\\textbf{Age Group} & \n",
  "\\textbf{Displacement (km)} & \n",
  "\\textbf{Tortuosity} & \n",
  "\\textbf{R} & \n",
  "\\textbf{Path Type} \\\\\n",
  "\\midrule\n"
)

for (i in 1:nrow(combined_metrics)) {
  row <- combined_metrics[i, ]
  latex_table_2 <- paste0(
    latex_table_2,
    row$AgeGroup, " & ",
    fmt(row$endpoint_displacement, 3), " & ",
    fmt(row$tortuosity, 2), " & ",
    fmt(row$R, 3), " & ",
    row$path_type, " \\\\\n"
  )
}

latex_table_2 <- paste0(
  latex_table_2,
  "\\bottomrule\n",
  "\\end{tabular}\n\n",
  "\\vspace{0.2cm}\n",
  "\\footnotesize{\n",
  "\\textit{Displacement}: Straight-line distance from baseline to final period.\n",
  "\\textit{Tortuosity}: Ratio of total path length to displacement (1.0 = straight path).\n",
  "\\textit{R}: Directional consistency measure (1.0 = perfectly consistent direction).\n",
  "}\n",
  "\\end{table}\n"
)

writeLines(latex_table_2, file.path(output_dir, "table_path_complexity_simple.tex"))
cat("  ✓ table_path_complexity_simple.tex\n")

# --- TABLE 3: Drift method comparison (if trajectory_drift_vectors exists) ---

if (exists("trajectory_drift_vectors")) {
  
  comparison <- drift_vectors %>%
    select(AgeGroup, mag_endpoint = magnitude, angle_endpoint = angle) %>%
    inner_join(
      trajectory_drift_vectors %>%
        select(AgeGroup, mag_trajectory = magnitude, angle_trajectory = angle),
      by = "AgeGroup"
    ) %>%
    mutate(
      ratio = mag_trajectory / mag_endpoint,
      angle_diff = abs(angle_trajectory - angle_endpoint)
    )
  
  latex_table_3 <- paste0(
    "% Comparison: Endpoint vs Trajectory-Averaged Drift\n",
    "% Auto-generated by Step 6\n\n",
    "\\begin{table}[htbp]\n",
    "\\centering\n",
    "\\caption{\\textbf{Comparison of drift calculation methods.} \n",
    "Endpoint drift measures direct displacement from baseline (2007--09) to final period (2022--24), \n",
    "while trajectory-averaged drift computes the mean of all sequential period-to-period transition \n",
    "vectors. The methods yield identical directional information, with trajectory-averaged magnitudes \n",
    "equal to endpoint magnitudes divided by the number of transitions.}\n",
    "\\label{tab:drift_comparison}\n\n",
    "\\begin{tabular}{lcccc}\n",
    "\\toprule\n",
    "\\textbf{Age Group} & \n",
    "\\textbf{Endpoint} & \n",
    "\\textbf{Trajectory-Avg} & \n",
    "\\textbf{Ratio} & \n",
    "\\textbf{Angular} \\\\\n",
    " & \n",
    "\\textbf{(km)} & \n",
    "\\textbf{(km)} & \n",
    "\\textbf{(Traj/End)} & \n",
    "\\textbf{Difference (°)} \\\\\n",
    "\\midrule\n"
  )
  
  for (i in 1:nrow(comparison)) {
    row <- comparison[i, ]
    latex_table_3 <- paste0(
      latex_table_3,
      row$AgeGroup, " & ",
      fmt(row$mag_endpoint, 3), " & ",
      fmt(row$mag_trajectory, 3), " & ",
      fmt(row$ratio, 2), " & ",
      ifelse(row$angle_diff < 1, "$<$1", fmt(row$angle_diff, 1)), " \\\\\n"
    )
  }
  
  latex_table_3 <- paste0(
    latex_table_3,
    "\\bottomrule\n",
    "\\end{tabular}\n\n",
    "\\vspace{0.2cm}\n",
    "\\footnotesize{\n",
    "All cohorts show a ratio of approximately ", fmt(mean(comparison$ratio), 2), 
    ", corresponding to the ", nrow(all_sequential) / length(unique(all_sequential$AgeGroup)),
    " period-to-period transitions in the observation window. Angular differences are \n",
    "negligible, confirming that both methods identify the same directional trends. \n",
    "Given this equivalence, main results present endpoint drift for interpretability.\n",
    "}\n",
    "\\end{table}\n"
  )
  
  writeLines(latex_table_3, file.path(output_dir, "table_drift_comparison.tex"))
  cat("  ✓ table_drift_comparison.tex\n")
}

cat("\n✓ Path complexity analysis complete!\n")
cat("  Files created:\n")
cat("  - path_tortuosity.png\n")
cat("  - directional_consistency.png\n")
cat("  - path_complexity_combined.png\n")
cat("  - path_complexity_metrics.csv\n")
cat("  - sequential_vectors_with_angles.csv\n")
cat("  - table_path_complexity.tex\n")
cat("  - table_path_complexity_simple.tex\n")
if (exists("trajectory_drift_vectors")) {
  cat("  - table_drift_comparison.tex\n")
}
cat("\n")

cat("=", rep("=", 60), "\n", sep="")
cat("STEP 6 COMPLETE: Path complexity metrics and LaTeX tables\n")
cat("=", rep("=", 60), "\n\n", sep="")