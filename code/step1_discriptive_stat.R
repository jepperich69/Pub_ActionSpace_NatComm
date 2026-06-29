######################################################################
# STEP 1 – ACTION-SPACE DESCRIPTIVE SUMMARY  (Start from 2007, weekdays only)
# Computes descriptive metrics before KDE fitting
######################################################################

library(RODBC)
library(dplyr)
library(tidyr)

cat("\n=== LOADING DATA ===\n")

# -------------------------------------------------------------------
# 1. Database connection and loading
# -------------------------------------------------------------------
dbname <- "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Pub_PopulationVianuey_TBA/TU0624v1.accdb"
con <- RODBC::odbcConnectAccess2007(dbname)

tur <- RODBC::sqlFetch(con, "tur")
session <- RODBC::sqlFetch(con, "session")

cat("Loaded:", nrow(tur), "trips,", nrow(session), "sessions\n")

# -------------------------------------------------------------------
# 2. Clean and enrich trip data
# -------------------------------------------------------------------

session_filt <- session %>%
  filter(
    DiaryYear >= 2007,
    DiaryDaytype %in% c(11, 12),
    DiaryMonth != 7,
    RespAgeCorrect >= 10                 # <— drop younger than 10
  ) %>%
  dplyr::select(SessionId, RespAgeCorrect, RespSex, SessionWeight,
                HomeAdrCitySize, DiaryYear)

tur_clean <- tur %>%
  semi_join(session_filt, by = "SessionId") %>%
  dplyr::select(SessionId, TurId, TurNr, DepartMSM, ArrivalMSM, GISdist) %>%
  left_join(session_filt, by = "SessionId") %>%
  mutate(
    AgeGroup = case_when(
      RespAgeCorrect <= 17 ~ "10-17",
      RespAgeCorrect <= 30 ~ "18-30",
      RespAgeCorrect <= 55 ~ "31-55",
      RespAgeCorrect <= 65 ~ "56-65",
      TRUE                 ~ "66+"
    ),
    AgeGroup = factor(AgeGroup,
                      levels = c("10-17","18-30","31-55","56-65","66+"),
                      ordered = TRUE),
    Year = DiaryYear
  )

# -------------------------------------------------------------------
# 3. Add individuals with no trips (from filtered sessions only)
# -------------------------------------------------------------------
zero_trip <- session_filt %>%
  anti_join(tur_clean %>% distinct(SessionId), by = "SessionId") %>%
  mutate(
    AgeGroup = case_when(
      RespAgeCorrect <= 17 ~ "10-17",
      RespAgeCorrect <= 30 ~ "18-30",
      RespAgeCorrect <= 55 ~ "31-55",
      RespAgeCorrect <= 65 ~ "56-65",
      TRUE                 ~ "66+"
    ),
    AgeGroup = factor(AgeGroup,
                      levels = c("10-17","18-30","31-55","56-65","66+"),
                      ordered = TRUE),
    Year      = DiaryYear,
    AvgDist   = 0,
    FirstTrip = NA_real_,
    LastTrip  = NA_real_,
    HoursAway = 0
  ) %>%
  dplyr::select(SessionId, AgeGroup, Year, AvgDist, FirstTrip, LastTrip, HoursAway)

# -------------------------------------------------------------------
# 4. Compute per-person trip metrics
# -------------------------------------------------------------------
per_person <- tur_clean %>%
  filter(!is.na(DepartMSM), !is.na(ArrivalMSM), ArrivalMSM > DepartMSM) %>%
  group_by(SessionId, AgeGroup, Year) %>%
  summarise(
    AvgDist = sum(GISdist, na.rm = TRUE),
    FirstTrip = min(DepartMSM, na.rm = TRUE),
    LastTrip  = max(ArrivalMSM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(HoursAway = (LastTrip - FirstTrip) / 60)  # convert minutes → hours

# -------------------------------------------------------------------
# 5. Combine trippers and non-trippers
# -------------------------------------------------------------------
summary_all <- bind_rows(per_person, zero_trip)

# -------------------------------------------------------------------
# 6. Summarize by cohort and year
# -------------------------------------------------------------------
summary_stats_wd <- summary_all %>%
  group_by(AgeGroup, Year) %>%
  summarise(
    n = n(),
    share_no_trips  = mean(is.na(FirstTrip) | HoursAway == 0),
    
    avg_distance_km = mean(AvgDist, na.rm = TRUE),
    sd_distance_km  = sd(AvgDist, na.rm = TRUE),
    
    avg_hours_away  = mean(HoursAway, na.rm = TRUE),
    sd_hours_away   = sd(HoursAway, na.rm = TRUE),
    
    avg_first_trip_h = mean(FirstTrip, na.rm = TRUE) / 60,
    sd_first_trip_h  = sd(FirstTrip / 60, na.rm = TRUE),
    n_trippers       = sum(!is.na(FirstTrip)),
    
    avg_last_trip_h  = mean(LastTrip,  na.rm = TRUE) / 60,
    sd_last_trip_h   = sd(LastTrip / 60, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(Year, AgeGroup)

cat("\n=== SUMMARY STATISTICS (2007+) ===\n")
print(summary_stats_wd, n = Inf)

# -------------------------------------------------------------------
# 6. Summarize by cohort over years
# -------------------------------------------------------------------
summary_stats_all <- summary_all %>%
  group_by(AgeGroup) %>%
  summarise(
    n = n(),
    share_no_trips = mean(is.na(FirstTrip) | HoursAway == 0),
    avg_distance_km = mean(AvgDist, na.rm = TRUE),
    avg_hours_away = mean(HoursAway, na.rm = TRUE),
    avg_first_trip_h = mean(FirstTrip, na.rm = TRUE) / 60,
    avg_last_trip_h  = mean(LastTrip, na.rm = TRUE) / 60,
    .groups = "drop"
  ) %>%
  arrange(AgeGroup)

cat("\n=== SUMMARY STATISTICS (2007+) ===\n")
print(summary_stats_all, n = Inf)

# -------------------------------------------------------------------
# 6b. Summarize by 3-year bins (2007–2024)
# -------------------------------------------------------------------
summary_stats_3yr <- summary_all %>%
  # clamp to requested window
  filter(Year >= 2007, Year <= 2024) %>%
  mutate(
    YearBinStart = 2007 + 3 * floor((Year - 2007) / 3),
    YearBinEnd   = YearBinStart + 2,
    YearBin      = factor(
      paste0(YearBinStart, "–", YearBinEnd),
      levels = paste0(seq(2007, 2022, by = 3), "–", seq(2009, 2024, by = 3)),
      ordered = TRUE
    )
  ) %>%
  group_by(AgeGroup, YearBin) %>%
  summarise(
    n = n(),
    share_no_trips  = mean(is.na(FirstTrip) | HoursAway == 0),
    
    avg_distance_km = mean(AvgDist, na.rm = TRUE),
    sd_distance_km  = sd(AvgDist, na.rm = TRUE),
    
    avg_hours_away  = mean(HoursAway, na.rm = TRUE),
    sd_hours_away   = sd(HoursAway, na.rm = TRUE),
    
    avg_first_trip_h = mean(FirstTrip, na.rm = TRUE) / 60,
    sd_first_trip_h  = sd(FirstTrip / 60, na.rm = TRUE),
    n_trippers       = sum(!is.na(FirstTrip)),
    
    avg_last_trip_h  = mean(LastTrip,  na.rm = TRUE) / 60,
    sd_last_trip_h   = sd(LastTrip / 60, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(YearBin, AgeGroup)

cat("\n=== SUMMARY STATISTICS (3-year bins, 2007–2024) ===\n")
print(summary_stats_3yr, n = Inf)

# -------------------------------------------------------------------
# 7. Save results (extended with 3-year bins)
# -------------------------------------------------------------------
output_dir <- "../results/baseline/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Calculate 95% CIs
summary_stats_3yr <- summary_stats_3yr %>%
  mutate(
    # Share 0 trips CI
    se_no_trips = sqrt(share_no_trips * (1 - share_no_trips) / n),
    ci_no_trips_low  = pmax(0, share_no_trips - 1.96 * se_no_trips),
    ci_no_trips_high = pmin(1, share_no_trips + 1.96 * se_no_trips),
    ci_no_trips_label = paste0(round(ci_no_trips_low * 100, 1), "--", round(ci_no_trips_high * 100, 1)),
    
    # Distance CI
    se_distance = sd_distance_km / sqrt(n),
    ci_distance_low = pmax(0, avg_distance_km - 1.96 * se_distance),
    ci_distance_high = avg_distance_km + 1.96 * se_distance,
    ci_distance_label = paste0(round(ci_distance_low, 1), "--", round(ci_distance_high, 1)),
    
    # Hours away CI
    se_hours = sd_hours_away / sqrt(n),
    ci_hours_low = pmax(0, avg_hours_away - 1.96 * se_hours),
    ci_hours_high = avg_hours_away + 1.96 * se_hours,
    ci_hours_label = paste0(round(ci_hours_low, 2), "--", round(ci_hours_high, 2)),
    
    # First trip timing CI
    se_first = sd_first_trip_h / sqrt(n_trippers),
    ci_first_low = avg_first_trip_h - 1.96 * se_first,
    ci_first_high = avg_first_trip_h + 1.96 * se_first,
    ci_first_label = paste0(round(ci_first_low, 2), "--", round(ci_first_high, 2)),
    
    # Last trip timing CI
    se_last = sd_last_trip_h / sqrt(n_trippers),
    ci_last_low = avg_last_trip_h - 1.96 * se_last,
    ci_last_high = avg_last_trip_h + 1.96 * se_last,
    ci_last_label = paste0(round(ci_last_low, 2), "--", round(ci_last_high, 2))
  )

saveRDS(summary_stats_wd,   paste0(output_dir, "step1_descriptive_summary_2007on.rds"))
write.csv(summary_stats_wd, paste0(output_dir, "step1_descriptive_summary_2007_WD.csv"), row.names = FALSE)

write.csv(summary_stats_all, paste0(output_dir, "step1_descriptive_summary_2007_WD_all.csv"), row.names = FALSE)

saveRDS(summary_stats_3yr,   paste0(output_dir, "step1_descriptive_summary_3yrbins_2007_2024.rds"))
write.csv(summary_stats_3yr, paste0(output_dir, "step1_descriptive_summary_2007_WD_3yrbins.csv"), row.names = FALSE)

# -------------------------------------------------------------------
# 8. Generate LaTeX Table S1
# -------------------------------------------------------------------
cat("\n=== GENERATING LATEX TABLE S1 ===\n")

latex_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\scriptsize",
  "\\caption{Descriptive statistics by age group and 3-year bins, 2007--2024: Weekdays (excluding holidays)}",
  "\\begin{tabular}{l l r c c c c c}",
  "\\toprule",
  "\\textbf{Age group} & \\textbf{Year bin} & \\textbf{n} & \\textbf{Share 0 trips (\\%) [95\\% CI]} & \\textbf{Avg. dist. (km) [95\\% CI]} & \\textbf{Avg. hrs away [95\\% CI]} & \\textbf{First trip (h) [95\\% CI]} & \\textbf{Last trip (h) [95\\% CI]} \\\\",
  "\\midrule"
)

for (i in 1:nrow(summary_stats_3yr)) {
  row <- summary_stats_3yr[i, ]
  line <- paste0(
    row$AgeGroup, " & ", 
    gsub("–", "--", row$YearBin), " & ", 
    format(row$n, big.mark=","), " & ",
    round(row$share_no_trips * 100, 1), " (", row$ci_no_trips_label, ") & ",
    round(row$avg_distance_km, 1), " (", row$ci_distance_label, ") & ",
    sprintf("%.2f (%.2f--%.2f)", row$avg_hours_away, row$ci_hours_low, row$ci_hours_high), " & ",
    sprintf("%.2f (%.2f--%.2f)", row$avg_first_trip_h, row$ci_first_low, row$ci_first_high), " & ",
    sprintf("%.2f (%.2f--%.2f)", row$avg_last_trip_h, row$ci_last_low, row$ci_last_high), " \\\\"
  )
  latex_lines <- c(latex_lines, line)
}

latex_lines <- c(
  latex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  "\\item \"Weekdays\" exclude holidays according to survey coding (\\texttt{DiaryDaytype}~$\\in\\{11,12\\}$, \\texttt{DiaryMonth}~$\\neq 7$). Sample restricted to \\texttt{DiaryYear}~$\\ge 2007$. \"Share 0 trips\" denotes the proportion of diary days with no recorded travel. Confidence intervals (95\\%) are reported in parentheses: for \"Share 0 trips\", it is the binomial normal approximation CI; for all other metrics, it is the standard normal confidence interval calculated using cell sample sizes. Times are in hours since midnight; distances in kilometers.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(latex_lines, paste0(output_dir, "table_s1_descriptive.tex"))

cat("\n✓ Descriptive summaries and LaTeX Table S1 saved to:\n  ", output_dir, "\n")

# -------------------------------------------------------------------
# 9. Generate Supplementary Figure S7 (Annual Trends with 95% CIs)
# -------------------------------------------------------------------
cat("\n=== GENERATING SUPPLEMENTARY FIGURE S7 ===\n")
library(ggplot2)
library(patchwork)

# Calculate annual 95% CIs
summary_stats_wd <- summary_stats_wd %>%
  mutate(
    # Share 0 trips CI
    se_no_trips = sqrt(share_no_trips * (1 - share_no_trips) / n),
    ci_no_trips_low  = pmax(0, share_no_trips - 1.96 * se_no_trips),
    ci_no_trips_high = pmin(1, share_no_trips + 1.96 * se_no_trips),
    
    # Distance CI
    se_distance = sd_distance_km / sqrt(n),
    ci_distance_low = pmax(0, avg_distance_km - 1.96 * se_distance),
    ci_distance_high = avg_distance_km + 1.96 * se_distance,
    
    # Hours away CI
    se_hours = sd_hours_away / sqrt(n),
    ci_hours_low = pmax(0, avg_hours_away - 1.96 * se_hours),
    ci_hours_high = avg_hours_away + 1.96 * se_hours
  )

# Color scale
age_colours <- c(
  "10-17" = "#440154",
  "18-30" = "#3B528B",
  "31-55" = "#21908C",
  "56-65" = "#5DC963",
  "66+"   = "#FDE725"
)

# Plot 1: Share of days with 0 trips
p1 <- ggplot(summary_stats_wd, aes(x = Year, y = share_no_trips * 100, color = AgeGroup, fill = AgeGroup)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = ci_no_trips_low * 100, ymax = ci_no_trips_high * 100), alpha = 0.15, color = NA) +
  scale_color_manual(values = age_colours) +
  scale_fill_manual(values = age_colours) +
  scale_x_continuous(breaks = seq(2008, 2024, by = 4)) +
  labs(
    y = "Share of days with zero trips (%)",
    x = "Year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16)
  )

# Plot 2: Average distance when away
p2 <- ggplot(summary_stats_wd, aes(x = Year, y = avg_distance_km, color = AgeGroup, fill = AgeGroup)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = ci_distance_low, ymax = ci_distance_high), alpha = 0.15, color = NA) +
  scale_color_manual(values = age_colours) +
  scale_fill_manual(values = age_colours) +
  scale_x_continuous(breaks = seq(2008, 2024, by = 4)) +
  labs(
    y = "Mean active distance (km)",
    x = "Year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# Plot 3: Average hours away from home
p3 <- ggplot(summary_stats_wd, aes(x = Year, y = avg_hours_away, color = AgeGroup, fill = AgeGroup)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = ci_hours_low, ymax = ci_hours_high), alpha = 0.15, color = NA) +
  scale_color_manual(values = age_colours) +
  scale_fill_manual(values = age_colours) +
  scale_x_continuous(breaks = seq(2008, 2024, by = 4)) +
  labs(
    y = "Mean hours away from home (h)",
    x = "Year",
    color = "Age group",
    fill = "Age group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

# Combine plots using patchwork
combined_plot <- p1 + p2 + p3 + 
  plot_layout(ncol = 3, widths = c(1, 1, 1.3)) + 
  plot_annotation(
    title = "Annual mobility trends by age group with 95% confidence intervals, 2007-2024",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
  )

# Save plots
overleaf_fig_path <- "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Pub_ActionSpace_NatComm/Overleaf_source/figures/Figure_R2_S7.png"
ggsave(overleaf_fig_path, combined_plot, width = 16, height = 5.5, dpi = 300)
ggsave(paste0(output_dir, "Figure_R2_S7.png"), combined_plot, width = 16, height = 5.5, dpi = 300)
cat("\n✓ Supplementary Figure S7 saved to:\n  ", overleaf_fig_path, "\n")

