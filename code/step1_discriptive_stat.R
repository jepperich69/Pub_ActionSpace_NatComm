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
dbname <- "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Population synthesis Vianey/TU0624v1.accdb"
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
    share_no_trips = mean(is.na(FirstTrip) | HoursAway == 0),
    avg_distance_km = mean(AvgDist, na.rm = TRUE),
    avg_hours_away = mean(HoursAway, na.rm = TRUE),
    avg_first_trip_h = mean(FirstTrip, na.rm = TRUE) / 60,
    avg_last_trip_h  = mean(LastTrip, na.rm = TRUE) / 60,
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
# 6b. Summarize by 3-year bins (2007–2009, …, 2022–2024)
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
    avg_hours_away  = mean(HoursAway, na.rm = TRUE),
    avg_first_trip_h = mean(FirstTrip, na.rm = TRUE) / 60,
    avg_last_trip_h  = mean(LastTrip,  na.rm = TRUE) / 60,
    .groups = "drop"
  ) %>%
  arrange(YearBin, AgeGroup)

cat("\n=== SUMMARY STATISTICS (3-year bins, 2007–2024) ===\n")
print(summary_stats_3yr, n = Inf)

# -------------------------------------------------------------------
# 7. Save results (extended with 3-year bins)
# -------------------------------------------------------------------
# Update output_dir to be relative to the project root
output_dir <- "../results/baseline/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Calculate 95% CI for share_no_trips
summary_stats_3yr <- summary_stats_3yr %>%
  mutate(
    se = sqrt(share_no_trips * (1 - share_no_trips) / n),
    ci_low  = pmax(0, share_no_trips - 1.96 * se),
    ci_high = pmin(1, share_no_trips + 1.96 * se),
    ci_label = paste0(round(ci_low * 100, 1), "--", round(ci_high * 100, 1))
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
  "\\begin{tabular}{l l r r r r r r r}",
  "\\toprule",
  "\\textbf{Age group} & \\textbf{Year bin} & \\textbf{n} & \\textbf{Share 0 trips (\\%)} & \\textbf{95\\% CI} & \\textbf{Avg. dist. (km)} & \\textbf{Avg. hrs away} & \\textbf{First trip (h)} & \\textbf{Last trip (h)} \\\\",
  "\\midrule"
)

for (i in 1:nrow(summary_stats_3yr)) {
  row <- summary_stats_3yr[i, ]
  line <- paste0(
    row$AgeGroup, " & ", 
    gsub("–", "--", row$YearBin), " & ", 
    format(row$n, big.mark=","), " & ",
    round(row$share_no_trips * 100, 1), " & ",
    row$ci_label, " & ",
    round(row$avg_distance_km, 1), " & ",
    round(row$avg_hours_away, 2), " & ",
    round(row$avg_first_trip_h, 2), " & ",
    round(row$avg_last_trip_h, 2), " \\\\"
  )
  latex_lines <- c(latex_lines, line)
}

latex_lines <- c(
  latex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  "\\item \"Weekdays\" exclude holidays according to survey coding (\\texttt{DiaryDaytype}~$\\in\\{11,12\\}$, \\texttt{DiaryMonth}~$\\neq 7$). Sample restricted to \\texttt{DiaryYear}~$\\ge 2007$. \"Share 0 trips\" denotes the proportion of diary days with no recorded travel. The 95\\% confidence interval is the normal approximation for a binomial proportion using the person-day count in each age-group--period cell. Times are in hours since midnight; distances in kilometers.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(latex_lines, paste0(output_dir, "table_s1_descriptive.tex"))

cat("\n✓ Descriptive summaries and LaTeX Table S1 saved to:\n  ", output_dir, "\n")
