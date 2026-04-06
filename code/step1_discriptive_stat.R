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
output_dir <- "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Generalizing Pseudo Cohorts/"

saveRDS(summary_stats_wd,   paste0(output_dir, "step1_descriptive_summary_2007on.rds"))
write.csv(summary_stats_wd, paste0(output_dir, "step1_descriptive_summary_2007_WD.csv"), row.names = FALSE)

write.csv(summary_stats_all, paste0(output_dir, "step1_descriptive_summary_2007_WD_all.csv"), row.names = FALSE)

saveRDS(summary_stats_3yr,   paste0(output_dir, "step1_descriptive_summary_3yrbins_2007_2024.rds"))
write.csv(summary_stats_3yr, paste0(output_dir, "step1_descriptive_summary_2007_WD_3yrbins.csv"), row.names = FALSE)

cat("\n✓ Descriptive summaries saved to:\n  ", output_dir, "\n")
cat("  Files:\n",
    "  - step1_descriptive_summary_2007on.rds\n",
    "  - step1_descriptive_summary_2007_WD.csv\n",
    "  - step1_descriptive_summary_2007_WD_all.csv\n",
    "  - step1_descriptive_summary_3yrbins_2007_2024.rds\n",
    "  - step1_descriptive_summary_2007_WD_3yrbins.csv\n\n")
