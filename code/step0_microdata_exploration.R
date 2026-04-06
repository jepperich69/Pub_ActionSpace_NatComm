######################################################################
# STEP 0 – MICRODATA EXPLORATION (PROTOTYPE / ARCHIVE)
# Original script: "TU data subset 2007-2024 - Only Kernel estimation V2.R"
# Jeppe Rich, 18.08.2025
#
# Purpose: Exploratory script showing how to load, merge, and inspect
#   the raw TU trip data from the Access database before the structured
#   step1/step2 pipeline was established. Included for reproducibility.
#   DO NOT run as part of the standard pipeline — use step1 and step2.
#
# Database: Population synthesis Vianey/TU0624v1.accdb (not public)
######################################################################

##################
# Import TU data #
##################

install.packages(odbc)

#install.packages("mgcv")
library(mgcv)
library(odbc)
library(dplyr)
library(tidyr)
library(odbc)
library(RODBC)
library(ggplot2)

# Load required libraries
library(stats)

unique(odbc::odbcListDrivers()[[1]])

# path to TU-databasen 
dbname <-"C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Population synthesis Vianey/TU0624v1.accdb"

con <- RODBC::odbcConnectAccess2007(dbname)
# List tables in the database
RODBC::sqlTables(con)

#load the datatable "tur, session and household"
tur<-RODBC::sqlFetch(con,"tur")
session<-RODBC::sqlFetch(con,"session")
household<-RODBC::sqlFetch(con,"household")

#Check summary
summary(session)
head(session)

#################################################
# Merge "Tur" and "Session" and select variables#
#################################################

# Step 1: Clean and enrich the raw trip-level data
tur_enriched <- tur %>%
  dplyr::select(SessionId, TurId, TurNr, PrimMode, PrimModeSumlen, PartyNumAdults, PartyNum1017, PartyNumu10, DepartMSM, ArrivalMSM, DestDwelTime, GISdist, GISdistJourneyStartP, OrigPurp, DestPurp) %>%
  left_join(
    session %>%
      dplyr::select(SessionId, SessionWeight, DiaryYear, RespSex, RespAgeCorrect, HomeAdrCitySize, HousehNumcars),
    by = "SessionId"
  ) %>%
  mutate(
    AggMode = case_when(
      PrimMode == 1 ~ "Walk",
      PrimMode == 2 ~ "Bike",
      PrimMode %in% 11:15 ~ "Car",
      PrimMode %in% 26:37 ~ "Public Transport",
      PrimMode == 51 ~ "Aviation",
      TRUE ~ "Other"
    ),
    AgeGroup = case_when(
      RespAgeCorrect <= 17 ~ "0-17",
      RespAgeCorrect <= 30 ~ "18-30",
      RespAgeCorrect <= 55 ~ "31-55",
      RespAgeCorrect <= 65 ~ "56-65",
      RespAgeCorrect >  65 ~ "66+",
      TRUE ~ NA_character_
    ),
    FromPurpose = case_when(
      OrigPurp == 1                 ~ "Home",
      OrigPurp %in% c(11:15, 51:64) ~ "Work",
      OrigPurp %in% 21:39           ~ "Shop",
      OrigPurp %in% 41:49           ~ "Leisure",
      TRUE ~ NA_character_
    ),
    ToPurpose = case_when(
      DestPurp == 1                 ~ "Home",
      DestPurp %in% c(11:15, 51:64) ~ "Work",
      DestPurp %in% 21:39           ~ "Shop",
      DestPurp %in% 41:49           ~ "Leisure",
      TRUE ~ NA_character_
    ),
    PartySize = PartyNumAdults + PartyNum1017 + PartyNumu10,
    PartySize = if_else(AggMode == "Car", PartySize, NA_real_),
    Male = if_else(RespSex == 1, 1, 0),
    City = if_else(HomeAdrCitySize > 10000, 1, 0, missing = 0)
  ) %>%
  rename(
    TravelDistance = PrimModeSumlen,
    Year = DiaryYear,
    Cars = HousehNumcars
  ) %>%
  filter(Year != 2006) %>%
  dplyr::select(
    -PartyNumAdults, -PartyNum1017, -PartyNumu10,
    -RespSex, -HomeAdrCitySize,
    -PrimMode
  )

# Step 2: Compute weighted persons correctly (per group of interest)
#weighted_persons <- tur_enriched %>%
#  filter(!(AggMode %in% c("Aviation", "Other"))) %>%
#  distinct(Year, AggMode, Cars, Male, AgeGroup, City, SessionId, .keep_all = TRUE) %>%
#  group_by(Year, AggMode, Cars, Male, AgeGroup, City) %>%
#  summarise(
#    WeightedPersons = sum(SessionWeight, na.rm = TRUE),
#    .groups = "drop"
#  )

# Step 3: Compute trip-level aggregates per group
#grouped_metrics <- tur_enriched %>%
#  filter(!(AggMode %in% c("Aviation", "Other"))) %>%
#  group_by(Year, AggMode, Cars, Male, AgeGroup, City) %>%
#  summarise(
#    WeightedTrips = sum(SessionWeight, na.rm = TRUE),
#    WeightedAvgTravelDistance = weighted.mean(TravelDistance, w = SessionWeight, na.rm = TRUE),
#    WeightedAvgPartySize = weighted.mean(PartySize, w = SessionWeight, na.rm = TRUE),
#    .groups = "drop"
#  )

tur_enriched <- tur_enriched %>%
  mutate(Age = RespAgeCorrect) %>%
  dplyr::select(
    SessionId, TurId, TurNr,
    DepartMSM, ArrivalMSM, DestDwelTime,
    GISdist, GISdistJourneyStartP,
    FromPurpose, ToPurpose,
    AgeGroup, Age,
    Cars, Male, Year, City,
    SessionWeight, AggMode
  ) %>%
  arrange(SessionId, TurId, TurNr)

##################################
# Identify SessionIds with trips #
##################################

trippers <- tur_enriched %>%
  distinct(SessionId)

# Step 1: Create zero-trip observations
zero_trip_obs <- session %>%
  filter(!(SessionId %in% trippers$SessionId), DiaryYear != 2006) %>%
  mutate(
    Year = DiaryYear,
    AggMode = "None",
    TravelDistance = 0,
    GISdist = 0,
    GISdistJourneyStartP = 0,
    DepartMSM = NA_real_,
    ArrivalMSM = NA_real_,
    DestDwelTime = NA_real_,
    FromPurpose = NA_character_,
    ToPurpose = NA_character_,
    Age = RespAgeCorrect,
    AgeGroup = case_when(
      RespAgeCorrect <= 17 ~ "0-17",
      RespAgeCorrect <= 30 ~ "18-30",
      RespAgeCorrect <= 55 ~ "31-55",
      RespAgeCorrect <= 65 ~ "56-65",
      RespAgeCorrect >  65 ~ "66+",
      TRUE ~ NA_character_
    ),
    PartySize = NA_real_,
    Male = if_else(RespSex == 1, 1, 0),
    City = if_else(HomeAdrCitySize > 10000, 1, 0, missing = 0),
    Cars = HousehNumcars,
    SessionWeight = SessionWeight,
    TurId = NA_integer_,
    TurNr = NA_integer_
  ) %>%
  dplyr::select(
    SessionId, TurId, TurNr, DepartMSM, ArrivalMSM, DestDwelTime,
    GISdist, GISdistJourneyStartP, FromPurpose, ToPurpose, AgeGroup, Age, 
    Cars, Male, Year, City, SessionWeight, AggMode
  )

######################################################################
# Build the table "tur_enriched" and remove problematic observations #
######################################################################

tur_enriched <- bind_rows(tur_enriched, zero_trip_obs) %>%
  arrange(SessionId, TurId, TurNr)

library(data.table)

# Convert to data.table
tur_enriched <- as.data.table(tur_enriched)

# Identify problematic SessionIds with any missing GISdist or GISdistJourneyStartP
bad_sessions <- tur_enriched[
  is.na(GISdist) | is.na(GISdistJourneyStartP),
  unique(SessionId)
]

# Remove all rows from these sessions
tur_enriched <- tur_enriched[!SessionId %in% bad_sessions]

# Optional: Report how many sessions were removed
cat("Removed", length(bad_sessions), "sessions due to missing GISdist or GISdistJourneyStartP.\n")

# Dump test-sample
write.csv(
  head(tur_enriched, 30),
  file = "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Generalizing Pseudo Cohorts/sample_output.csv",
  row.names = FALSE
)

# 1. Find sessions whose first trip isn't from Home
bad_sessions <- tur_enriched %>%
  filter(AggMode != "None") %>%               # ignore zero-trip rows
  group_by(SessionId) %>%
  slice_min(DepartMSM, with_ties = FALSE) %>% # pick the earliest trip
  filter(FromPurpose != "Home") %>%           # check it
  pull(SessionId) %>%
  unique()

# 2. Report how many sessions will be removed
message("Removing ", length(bad_sessions),
        " sessions because their first trip does not start at Home.")

# 3. Filter them out entirely
tur_enriched <- tur_enriched %>%
  filter(!SessionId %in% bad_sessions)

head(tur_enriched)

# Write new test sample
write.csv(
  head(tur_enriched, 30),
  file = "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Generalizing Pseudo Cohorts/sample_output.csv",
  row.names = FALSE
)

# STEP 1: Identify and extract zero-action SessionIds
zero_ids <- tur_enriched %>%
  group_by(SessionId) %>%
  summarise(all_zero = all(GISdist == 0 & GISdistJourneyStartP == 0), .groups = "drop") %>%
  filter(all_zero) %>%
  pull(SessionId)

Zero <- tur_enriched %>%
  filter(SessionId %in% zero_ids) %>%
  group_by(SessionId) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    TimeMSM = DepartMSM,
    CumDist = 0,
    DistFromHome = 0
  )

# STEP 2: Filter non-zero SessionIds
tur_moving <- tur_enriched %>%
  filter(!(SessionId %in% zero_ids)) %>%
  filter(!is.na(DepartMSM), !is.na(ArrivalMSM), ArrivalMSM > DepartMSM)

write.csv(
  head(tur_moving, 30),
  file = "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Generalizing Pseudo Cohorts/sample_moving.csv",
  row.names = FALSE
)

################# CREATE TABLE WITH DESCRIPTIVE STAT ########################

# --- Inputs (your bins) ---
cohorts <- c("1900–1960","1960–1970","1970–1980","1980–1990","1990–2000","2000–2024")
year_bins <- list(
  `2007–2009` = 2007:2009,
  `2010–2012` = 2010:2012,
  `2013–2015` = 2013:2015,
  `2016–2018` = 2016:2018,
  `2019–2021` = 2019:2021,
  `2022–2024` = 2022:2024
)
year_lookup <- tibble(
  Year = unlist(year_bins),
  year_bin = rep(names(year_bins), lengths(year_bins))
)

# --- Build cohort + year bins from Age & Year ---
tur_binned <- tur_enriched %>%
  mutate(
    birth_year = Year - Age,
    cohort = case_when(
      birth_year >= 1900 & birth_year < 1960 ~ "1900–1960",
      birth_year >= 1960 & birth_year < 1970 ~ "1960–1970",
      birth_year >= 1970 & birth_year < 1980 ~ "1970–1980",
      birth_year >= 1980 & birth_year < 1990 ~ "1980–1990",
      birth_year >= 1990 & birth_year < 2000 ~ "1990–2000",
      birth_year >= 2000 & birth_year <= 2024 ~ "2000–2024",
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(year_lookup, by = "Year") %>%
  mutate(
    cohort  = factor(cohort,  levels = cohorts),
    year_bin = factor(year_bin, levels = names(year_bins))
  )

# --- 6×6 count table (ensures all combos present) ---
table_counts <- tur_binned %>%
  filter(!is.na(cohort), !is.na(year_bin)) %>%
  count(cohort, year_bin, name = "n") %>%
  complete(cohort, year_bin, fill = list(n = 0)) %>%
  arrange(cohort, year_bin)

# Wide matrix-style view
table_wide <- table_counts %>%
  pivot_wider(names_from = year_bin, values_from = n) %>%
  arrange(cohort)

print(table_wide)  # clean 6×6 table for your appendix/diagnostics

# --- Heatmap with cell labels ---
ggplot(table_counts, aes(x = year_bin, y = cohort, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n)) +
  scale_fill_viridis_c(name = "Count") +
  labs(x = "Survey Wave (Year Bin)", y = "Birth Cohort",
       title = "Observations by Cohort and Survey Wave (6×6)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

##########

# ---- 0) tur_moving is already defined ----

# 1) Session‐level static columns
static_cols <- c(
  "SessionId","AgeGroup", "Age", "Cars",
  "City","SessionWeight","Male","Year"
)
sessions_static <- tur_moving %>%
  select(any_of(static_cols)) %>%
  distinct()

# 2) 96 fifteen‐minute bins
bins <- tibble(TimeMSM = seq(0, 1439, by = 15))

# 3) Time‐varying trip attributes (one row per SessionId×bin)
trip_attrs <- tur_moving %>%
  mutate(depart_bin = pmin(ceiling(DepartMSM/15)*15, 1439L)) %>%
  group_by(SessionId, depart_bin) %>%
  slice_max(DepartMSM, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    SessionId,
    depart_bin,
    AggMode,
    FromPurpose,
    ToPurpose
  )

# 4) Departures table for r(t)
trips_dep <- tur_moving %>%
  mutate(depart_bin = pmin(ceiling(DepartMSM/15)*15, 1439L)) %>%
  group_by(SessionId, depart_bin) %>%
  slice_max(DepartMSM, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(SessionId, depart_bin, r_val = GISdistJourneyStartP)

# 5) Arrivals table for d(t)
trips_arr <- tur_moving %>%
  mutate(arrival_bin = pmin(ceiling(ArrivalMSM/15)*15, 1439L)) %>%
  group_by(SessionId, arrival_bin) %>%
  summarise(d_inc = sum(GISdist), .groups = "drop")

# 6) Active‐window per session
session_range <- tur_moving %>%
  mutate(
    depart_bin  = pmin(ceiling(DepartMSM/15)*15, 1439L),
    arrival_bin = pmin(ceiling(ArrivalMSM/15)*15, 1439L)
  ) %>%
  group_by(SessionId) %>%
  summarise(
    first_bin = min(depart_bin),
    last_bin  = max(arrival_bin),
    .groups   = "drop"
  )

# 7) Build the full grid and join everything
grid_mv <- sessions_static %>%
  crossing(bins) %>%
  arrange(SessionId, TimeMSM) %>%
  
  # a) trip_attrs → fill forward
  left_join(trip_attrs,
            by = c("SessionId", "TimeMSM" = "depart_bin")) %>%
  group_by(SessionId) %>%
  fill(AggMode, FromPurpose, ToPurpose, .direction = "down") %>%
  mutate(across(c(AggMode, FromPurpose, ToPurpose),
                ~ replace_na(.x, "NoTrip"))) %>%
  ungroup() %>%
  
  # b) r(t)
  left_join(trips_dep,
            by = c("SessionId", "TimeMSM" = "depart_bin")) %>%
  group_by(SessionId) %>%
  arrange(TimeMSM) %>%
  fill(r_val, .direction = "down") %>%
  mutate(r_rad_km = replace_na(r_val, 0)) %>%
  select(-r_val) %>%
  ungroup() %>%
  
  # c) d(t)
  left_join(trips_arr,
            by = c("SessionId", "TimeMSM" = "arrival_bin")) %>%
  mutate(d_inc = replace_na(d_inc, 0)) %>%
  group_by(SessionId) %>%
  arrange(TimeMSM) %>%
  mutate(d_cum_km = cumsum(d_inc)) %>%
  select(-d_inc) %>%            # only drop the temp increment
  ungroup() %>%
  
  # d) active flag
  left_join(session_range, by = "SessionId") %>%
  mutate(active = as.integer(TimeMSM >= first_bin & TimeMSM <= last_bin)) %>%
  select(-first_bin, -last_bin)

# 8) Final sort and inspection
grid_mv <- grid_mv %>%
  arrange(SessionId, TimeMSM)

# Example view
grid_mv %>%
  filter(SessionId == 59268) %>%
  print(n = Inf) # Use n = Inf to show all rows

#######################################################################################
################### rho(d,t) KERNEL estimation ########################################
#######################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

plot_agegroup_twoyear_unweighted <- function(
    df,
    age_group,               # e.g. "0-17", "18-30", etc.
    time_col     = "TimeMSM",
    radius_col   = "r_rad_km",
    year_col     = "Year",
    sigma_bins   = 2
) {
  # 1) Build Gaussian kernel
  win <- ceiling(6 * sigma_bins)
  if (win %% 2 == 0) win <- win + 1
  kern_x <- seq(-3*sigma_bins, 3*sigma_bins, length.out = win)
  kern   <- dnorm(kern_x, mean = 0, sd = sigma_bins)
  kern   <- kern / sum(kern)
  
  df %>%
    # 2) Filter to the chosen AgeGroup
    filter(AgeGroup == age_group) %>%
    
    # 3) Create two-year bins
    mutate(
      bin0   = floor((.data[[year_col]] - min(.data[[year_col]])) / 2) * 2 + min(.data[[year_col]]),
      YearBin = paste0(bin0, "–", bin0 + 1)
    ) %>%
    
    # 4) Compute unweighted mean radial distance per slot
    group_by(YearBin, TimeMSM = .data[[time_col]]) %>%
    summarise(
      mean_r = mean(.data[[radius_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    
    # 5) Fill in all 96 bins 0–1425
    complete(
      YearBin,
      TimeMSM = seq(0, 95 * 15, by = 15),
      fill = list(mean_r = 0)
    ) %>%
    
    # 6) Smooth within each YearBin
    group_by(YearBin) %>%
    mutate(
      Smoothed = as.numeric(stats::filter(mean_r, kern, sides = 2)),
      Hour      = TimeMSM / 60
    ) %>%
    ungroup() %>%
    
    # 7) Plot
    ggplot(aes(x = Hour, y = Smoothed, color = YearBin)) +
    geom_line(size = 1) +
    scale_color_viridis_d(option = "D", name = "Survey Bin") +
    scale_x_continuous(
      "Time of Day (hours since midnight)",
      breaks = seq(0, 24, by = 4),
      limits = c(0, 24),
      expand = c(0, 0)
    ) +
    labs(
      title = paste0(age_group, " cohort — 2-year action-space (unweighted)"),
      y     = "Radial Distance (km)"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "right")
}

# Examples:
plot_agegroup_twoyear_unweighted(grid_mv, "0-17")
plot_agegroup_twoyear_unweighted(grid_mv, "18-30")
plot_agegroup_twoyear_unweighted(grid_mv, "31-55")

######################## Proper Pseudo Panel with 10-year tracking ##########################

library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
install.packages("zoo")    # if you haven’t already
library(zoo)

# 1) Define the wide birth‐cohort breaks and labels
birth_breaks <- c(1900, 1950, 1960, 1970, 1980, 1990, 2000, 2010, Inf)
birth_labels <- c(
  "1900–1950", "1950–1960", "1960–1970",
  "1970–1980", "1980–1990", "1990–2000",
  "2000–2010", "2010+"
)

# 2) Add BirthYear and BirthCohortWide to grid_mv
grid_mv_bc <- grid_mv %>%
  mutate(
    BirthYear = Year - Age,
    BirthCohortWide = cut(
      BirthYear,
      breaks = birth_breaks,
      labels = birth_labels,
      right  = FALSE
    )
  ) %>%
  filter(!is.na(BirthCohortWide))  # drop those outside our breaks

# 3) Kernel setup (same as before)
sigma_bins <- 2
win <- ceiling(6 * sigma_bins); if(win%%2==0) win <- win+1
kx <- seq(-3*sigma_bins, 3*sigma_bins, length.out = win)
kern <- dnorm(kx,0,sigma_bins); kern <- kern/sum(kern)

# 4) A little helper to return a smoothed df for any cohort slice
get_smoothed_cohort <- function(df, cohort_label) {
  df %>%
    filter(BirthCohortWide == cohort_label) %>%
    
    # two‐year survey bins
    mutate(
      bin0   = floor((Year - min(Year)) / 2) * 2 + min(Year),
      YearBin = paste0(bin0, "–", bin0 + 1)
    ) %>%
    
    # mean per 15-min bin (unweighted)
    group_by(YearBin, TimeMSM) %>%
    summarise(
      mean_r = mean(r_rad_km, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    
    # fill all 96 bins
    complete(
      YearBin,
      TimeMSM = seq(0, 95*15, by = 15),
      fill = list(mean_r = 0)
    ) %>%
    
    # 6) Smooth within each YearBin, then carry edges forward/backward
    group_by(YearBin) %>%
    mutate(
      Smoothed = stats::filter(mean_r, kern, sides = 2) %>%
        as.numeric() %>%
        na.locf(na.rm = FALSE) %>%       # forward‐fill NAs
        na.locf(fromLast = TRUE),        # then back‐fill any leading NAs
      Hour     = TimeMSM / 60
    ) %>%
    ungroup() %>%
    mutate(Cohort = cohort_label)
}

# 5) Build a single dataframe with all wide‐cohorts
all_cohorts <- lapply(birth_labels, function(lbl) {
  get_smoothed_cohort(grid_mv_bc, lbl)
}) %>% bind_rows()

# 6) Plot: faceted by birth‐cohort, colored by survey‐bin
ggplot(all_cohorts, aes(x = Hour, y = Smoothed, color = YearBin)) +
  geom_line(size = 1) +
  facet_wrap(~ Cohort, ncol = 2) +
  scale_color_viridis_d(option = "D", name = "Survey Bin") +
  scale_x_continuous(
    "Hour of Day",
    breaks = seq(0, 24, by = 4),
    limits = c(0, 24),
    expand = c(0,0)
  ) +
  labs(
    title = "Action-Space Curves by Wide Birth-Cohort",
    y     = "Radial Distance (km)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(face = "bold")
  )

################# SWAP TO COHORTS ###################

library(ggplot2)
library(viridis)

# ensure YearBin is ordered chronologically
all_cohorts$YearBin <- factor(
  all_cohorts$YearBin,
  levels = sort(unique(all_cohorts$YearBin))
)

ggplot(all_cohorts, aes(x = Hour, y = Smoothed, color = Cohort)) +
  geom_line(size = 1) +
  facet_wrap(~ YearBin, ncol = 2) +
  scale_color_viridis_d(
    option = "D",
    name   = "Birth Cohort",
    begin  = 0.1, end = 0.9
  ) +
  scale_x_continuous(
    "Hour of Day",
    breaks = seq(0, 24, by = 4),
    limits = c(0, 24),
    expand = c(0, 0)
  ) +
  labs(
    title = "Generational Action‐Space Curves by Survey Bin",
    y     = "Radial Distance (km)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(face = "bold")
  )

##################

install.packages("patchwork")
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(patchwork)

# Pre–compute the discrete YearBin levels and linetypes
yearBins <- sort(unique(all_cohorts$YearBin))
nBins    <- length(yearBins)
# Define a cycle of common dash styles
dashStyles <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
ltys      <- rep(dashStyles, length.out = nBins)
names(ltys) <- yearBins

# Split into two sets of four cohorts
coh_labels <- unique(all_cohorts$Cohort)
set1 <- coh_labels[1:4]
set2 <- coh_labels[5:8]

# Plotting function with both color & linetype mapped to YearBin
plot_chunk <- function(labels) {
  all_cohorts %>%
    filter(Cohort %in% labels) %>%
    ggplot(aes(x = Hour, y = Smoothed, 
               color    = YearBin, 
               linetype = YearBin)) +
    geom_line(size = 1) +
    facet_wrap(~ Cohort, ncol = 2) +
    scale_color_viridis_d(option = "D", name = "Survey Bin") +
    scale_linetype_manual(name = "Survey Bin", values = ltys) +
    scale_x_continuous(
      "Hour of Day",
      breaks = seq(0, 24, by = 4),
      limits = c(0, 24),
      expand = c(0, 0)
    ) +
    labs(y = "Radial Distance (km)") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      strip.text      = element_text(face = "bold"),
      panel.spacing   = unit(1, "lines")
    )
}

# Generate and display the two 2×2 plots
p1 <- plot_chunk(set1)
p2 <- plot_chunk(set2)

p1
p2

################################# ESTIMATION - 2*2 plot ##################################

# 1) Load required libraries
library(MASS)
library(ggplot2)
library(viridis)
library(purrr)

# 2) Make sure BirthCohortWideGrouped exists
grid_mv_bc <- grid_mv_bc %>%
  mutate(
    BirthCohortWideGrouped = case_when(
      BirthCohortWide %in% c("2000–2010", "2010+") ~ "2000–2024",
      TRUE ~ as.character(BirthCohortWide)
    )
  )

# 3) Define cohorts and year bins
cohorts <- c("1960–1970", "2000–2024")
year_bins <- list(
  `2007–2009` = 2007:2009,
  `2022–2024` = 2022:2024
)

# 4) Create combination grid
combos <- tidyr::expand_grid(
  cohort  = cohorts,
  yearbin = names(year_bins)
) %>%
  mutate(years = purrr::map(yearbin, ~ year_bins[[.x]]))

# 5) Loop with KDE estimation
all_kdes <- purrr::pmap_dfr(
  combos,
  function(cohort, yearbin, years) {
    dat2d <- grid_mv_bc %>%
      filter(
        BirthCohortWideGrouped == cohort,
        Year %in% years,
        active == 1
      ) %>%
      mutate(Hour = TimeMSM / 60)
    
    # Diagnostics
    cat("=== ", cohort, " - ", yearbin, " ===\n")
    print(table(dat2d$BirthCohortWideGrouped, useNA = "always"))
    print(table(dat2d$Year, useNA = "always"))
    cat("N observations:", nrow(dat2d), "\n\n")
    
    kde2d_res <- with(dat2d, kde2d(
      x    = Hour,
      y    = r_rad_km,
      n    = c(200, 200),
      lims = c(0, 24, 0, max(r_rad_km, na.rm = TRUE)),
      h    = c(1, 5)
    ))
    
    expand.grid(
      Hour   = kde2d_res$x,
      Radius = kde2d_res$y
    ) %>%
      mutate(
        Density                = as.vector(kde2d_res$z),
        BirthCohortWideGrouped = cohort,
        YearBin                = yearbin
      )
  }
)

# 6) Plot
p <- ggplot(all_kdes, aes(x = Hour, y = Radius)) +
  geom_raster(aes(fill = Density)) +
  geom_contour(aes(z = Density), color = "white", bins = 6, size = 0.2) +
  coord_cartesian(expand = FALSE, ylim = c(0, 30), xlim = c(4, 24)) +
  scale_x_continuous("Hour of Day", breaks = seq(0, 24, by = 4), labels = c("", "4", "8", "12", "16", "20", "")) +
  scale_y_continuous("Radial Distance (km)", breaks = seq(0, 30, by = 5), labels = c("0", "5", "10", "15", "20", "25", "")) +
  scale_fill_viridis_c(
    name = NULL,
    option = "inferno",
    direction = 1,
    trans = "sqrt",
    guide = guide_colorbar(
      title.position = "left",
      title.hjust    = 1,
      label.position = "bottom",
      barwidth       = unit(10, "cm"),
      barheight      = unit(0.4, "cm")
    )
  ) +
  geom_vline(
    xintercept = seq(0, 24, by = 1),
    linetype   = "dotted",
    color      = "white",
    alpha      = 0.25,
    linewidth  = 0.3
  ) +
  facet_grid(BirthCohortWideGrouped ~ YearBin) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title    = element_text(hjust = 1),
    legend.text     = element_text(hjust = 0.5),
    legend.margin   = margin(t = 10),
    panel.spacing   = unit(0.5, "lines"),
    strip.text.y    = element_text(face = "bold", angle = 0),
    strip.text.x    = element_text(face = "bold"),
    axis.title      = element_text(size = 12)
  )

p

# 7) Save to file
ggsave(
  filename = "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Generalizing Pseudo Cohorts/mobility_kde_2x2.png",
  plot     = p,
  width    = 11.69,   # A4 landscape
  height   = 8.27,
  units    = "in",
  dpi      = 300,
  device   = "png"
)

######################### ESTIMATION 6*6 Plot ##################


# 1) Load packages
library(MASS)      # for kde2d()
library(dplyr)     # for data manipulation
library(purrr)     # for pmap_dfr
library(tidyr)     # for expand_grid
library(ggplot2)   # for plotting
library(viridis)   # for color scales

R_MAX <- grid_mv_bc %>%
  filter(active == 1) %>%
  pull(r_rad_km) %>%
  quantile(0.99, na.rm = TRUE) %>%
  as.numeric()
R_MAX <- max(30, floor(R_MAX))   # keep at least 30 km like your plot

N_T <- 200  # # of grid points along Hour (time)
N_D <- 200  # # of grid points along Radius (distance)

# 2) Group cohorts into 6 broader bins
grid_mv_bc <- grid_mv_bc %>%
  mutate(
    BirthCohortGrouped6 = case_when(
      BirthCohortWide %in% c("1900–1950", "1950–1960") ~ "1900–1960",
      BirthCohortWide %in% c("2000–2010", "2010+")     ~ "2000–2024",
      TRUE                                              ~ as.character(BirthCohortWide)
    )
  )

# 3) Define 6×6 cohorts and year bins
cohorts <- c(
  "1900–1960", "1960–1970", "1970–1980", 
  "1980–1990", "1990–2000", "2000–2024"
)

year_bins <- list(
  `2007–2009` = 2007:2009,
  `2010–2012` = 2010:2012,
  `2013–2015` = 2013:2015,
  `2016–2018` = 2016:2018,
  `2019–2021` = 2019:2021,
  `2022–2024` = 2022:2024
)

# 4) Create combination grid
combos <- expand_grid(
  cohort  = cohorts,
  yearbin = names(year_bins)
) %>%
  mutate(years = map(yearbin, ~ year_bins[[.x]]))

# 5) KDE estimation loop with diagnostics
all_kdes <- pmap_dfr(
  combos,
  function(cohort, yearbin, years) {
    dat2d <- grid_mv_bc %>%
      filter(
        BirthCohortGrouped6 == cohort,
        Year %in% years,
        active == 1
      ) %>%
      mutate(Hour = TimeMSM / 60)
    
    # Diagnostics
    cat("=== ", cohort, " - ", yearbin, " ===\n")
    print(table(dat2d$BirthCohortGrouped6, useNA = "always"))
    print(table(dat2d$Year, useNA = "always"))
    cat("N observations:", nrow(dat2d), "\n\n")
    
    kde2d_res <- with(dat2d, kde2d(
      x    = Hour,
      y    = r_rad_km,
      # NEW (uses fixed global grid)
      n    = c(N_T, N_D),
      lims = c(0, 24, 0, R_MAX),
      h    = c(1, 5)
    ))
    
    expand.grid(
      Hour   = kde2d_res$x,
      Radius = kde2d_res$y
    ) %>%
      mutate(
        Density             = as.vector(kde2d_res$z),
        BirthCohortGrouped6 = cohort,
        YearBin             = yearbin
      )
  }
)

# 6) Create the plot
p <- ggplot(all_kdes, aes(x = Hour, y = Radius)) +
  geom_raster(aes(fill = Density)) +
  geom_contour(aes(z = Density), color = "white", bins = 6, size = 0.2) +
  coord_cartesian(expand = FALSE, ylim = c(0, 30), xlim = c(4, 24)) +
  scale_x_continuous("Hour of Day", breaks = seq(0, 24, by = 4),
                     labels = c("", "4", "8", "12", "16", "20", "")) +
  scale_y_continuous("Radial Distance (km)", breaks = seq(0, 30, by = 5),
                     labels = c("0", "5", "10", "15", "20", "25", "")) +
  scale_fill_viridis_c(
    name = NULL,
    option = "inferno",
    direction = 1,
    trans = "sqrt",
    guide = guide_colorbar(
      title.position = "left",
      title.hjust    = 1,
      label.position = "bottom",
      barwidth       = unit(10, "cm"),
      barheight      = unit(0.4, "cm")
    )
  ) +
  geom_vline(
    xintercept = seq(0, 24, by = 1),
    linetype   = "dotted",
    color      = "white",
    alpha      = 0.25,
    linewidth  = 0.3
  ) +
  facet_grid(BirthCohortGrouped6 ~ YearBin) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title    = element_text(hjust = 1),
    legend.text     = element_text(hjust = 0.5),
    legend.margin   = margin(t = 10),
    panel.spacing   = unit(0.5, "lines"),
    strip.text.y    = element_text(face = "bold", angle = 0),
    strip.text.x    = element_text(face = "bold"),
    axis.title      = element_text(size = 12)
  )

p

# 7) Save the plot (optional, adjust size for landscape or full-page portrait)
ggsave(
  filename = "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Generalizing Pseudo Cohorts/mobility_kde_6x6.png",
  plot     = p,
  width    = 16.5,   # Suitable for landscape A3 or large A4
  height   = 11.7,
  units    = "in",
  dpi      = 300,
  device   = "png"
)

###############################################################################
######## From here and down - ONLY rho and kernel data ########################
###############################################################################

# --- Step 1) Build tidy rho(d,t ; cohort, tau) and check the grid ---

install.packages("stringr")
library(stringr)  # if not already loaded
library(dplyr)  

# Map each YearBin to a numeric "calendar time" (mid-year of the 3-year bin)
yearbin_mid <- c(
  "2007–2009" = 2008,
  "2010–2012" = 2011,
  "2013–2015" = 2014,
  "2016–2018" = 2017,
  "2019–2021" = 2020,
  "2022–2024" = 2023
)

# Tidy density frame on the fixed grid from Step 0
rho_df <- all_kdes %>%
  mutate(
    YearBin = factor(YearBin, levels = names(year_bins)),
    tau     = unname(yearbin_mid[as.character(YearBin)]),
    cohort  = factor(BirthCohortGrouped6, levels = cohorts),
    t       = Hour,
    d       = Radius,
    rho     = Density
  ) %>%
  dplyr::select(cohort, YearBin, tau, t, d, rho)

# Sanity checks: ensure *every* panel shares the same grid
gchk <- rho_df %>%
  group_by(cohort, YearBin) %>%
  summarise(
    n_t  = n_distinct(t),
    n_d  = n_distinct(d),
    tmin = min(t), tmax = max(t),
    dmin = min(d), dmax = max(d),
    .groups = "drop"
  )

stopifnot(
  all(gchk$n_t  == N_T),
  all(gchk$n_d  == N_D),
  all(gchk$tmin == 0),
  all(gchk$tmax == 24),
  all(gchk$dmin == 0),
  all(gchk$dmax == R_MAX)
)

# (Optional) Keep an ordered key for later joins/merges
rho_df <- rho_df %>%
  arrange(cohort, tau, t, d)


#######################################################

# --- Step 2) Numerical derivatives: ∂ρ/∂t, ∂ρ/∂d, ∂ρ/∂τ ---

library(dplyr)

# Helper: centered finite differences on a sorted vector
centered_diff <- function(y, x) {
  n  <- length(y)
  out <- rep(NA_real_, n)
  if (n >= 2) {
    # one-sided at ends
    out[1]  <- (y[2] - y[1]) / (x[2] - x[1])
    out[n]  <- (y[n] - y[n-1]) / (x[n] - x[n-1])
  }
  if (n >= 3) {
    # centered interior
    out[2:(n-1)] <- (y[3:n] - y[1:(n-2)]) / (x[3:n] - x[1:(n-2)])
  }
  out
}

# ∂ρ/∂t within each (cohort, τ, d)
grad_t <- rho_df %>%
  group_by(cohort, tau, d) %>%
  arrange(t, .by_group = TRUE) %>%
  mutate(
    drho_dt = centered_diff(rho, t)
  ) %>%
  ungroup() %>%
  dplyr::select(cohort, tau, t, d, drho_dt)

# ∂ρ/∂d within each (cohort, τ, t)
grad_d <- rho_df %>%
  group_by(cohort, tau, t) %>%
  arrange(d, .by_group = TRUE) %>%
  mutate(
    drho_dd = centered_diff(rho, d)
  ) %>%
  ungroup() %>%
  dplyr::select(cohort, tau, t, d, drho_dd)

# ∂ρ/∂τ within each (cohort, t, d)
grad_tau <- rho_df %>%
  group_by(cohort, t, d) %>%
  arrange(tau, .by_group = TRUE) %>%
  mutate(
    drho_dtau = centered_diff(rho, tau)   # tau is numeric mid-year (e.g., 2008, 2011, …)
  ) %>%
  ungroup() %>%
  dplyr::select(cohort, tau, t, d, drho_dtau)

# Join derivatives back to rho_df
grad_df <- rho_df %>%
  left_join(grad_t,  by = c("cohort","tau","t","d")) %>%
  left_join(grad_d,  by = c("cohort","tau","t","d")) %>%
  left_join(grad_tau,by = c("cohort","tau","t","d"))

# Optional: basic sanity checks
stopifnot(all(is.finite(grad_df$t)))
stopifnot(all(is.finite(grad_df$d)))

# Peek
grad_df %>%
  group_by(cohort, tau) %>%
  summarise(
    rho_mean   = mean(rho, na.rm = TRUE),
    d_rho_t_sd = sd(drho_dt,   na.rm = TRUE),
    d_rho_d_sd = sd(drho_dd,   na.rm = TRUE),
    d_rho_tau_sd = sd(drho_dtau, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print(n = 12)


###########################################################

# --- Step 3) Baseline drift estimation (global v, u per cohort × tau) ---
library(dplyr)
library(purrr)
library(tidyr)

# Hyperparameters (tune if needed)
LAMBDA     <- 1e-3      # ridge penalty (small, stabilizes near-zero gradients)
RHO_QCUT   <- 0.20      # keep cells with rho >= 20th percentile (per cohort×tau)
WEIGHT_POW <- 1.0       # weights w = rho^WEIGHT_POW

# Helper: ridge WLS closed-form solver (no intercept)
ridge_wls <- function(X, y, w, lambda) {
  # X: n×2 (drho_dd, drho_dt), y: n, w: n nonnegative
  W12 <- sqrt(pmax(w, 0))
  Xw  <- X * W12
  yw  <- y * W12
  # beta = (X'X + λI)^(-1) X'y
  p   <- ncol(X)
  XtX <- crossprod(Xw) + lambda * diag(p)
  Xty <- crossprod(Xw, yw)
  beta <- solve(XtX, Xty)
  as.numeric(beta)
}

# Prepare fitting data and weights
fit_df <- grad_df %>%
  # Keep finite derivatives
  filter(is.finite(drho_dtau), is.finite(drho_dd), is.finite(drho_dt), is.finite(rho)) %>%
  group_by(cohort, tau) %>%
  # Drop very low-density cells (noisy gradients) using panel-specific cutoff
  mutate(rho_cut = quantile(rho, RHO_QCUT, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(rho >= rho_cut) %>%
  mutate(w = rho^WEIGHT_POW)

# Estimate v,u per (cohort, tau)
vu_est <- fit_df %>%
  dplyr::group_by(cohort, tau) %>%
  dplyr::group_modify(~{
    d <- .x  # data for this (cohort, tau)
    X <- as.matrix(d[, c("drho_dd", "drho_dt")])
    y <- d$drho_dtau
    w <- d$w
    ok <- is.finite(rowSums(X)) & is.finite(y) & is.finite(w)
    X <- X[ok, , drop = FALSE]; y <- y[ok]; w <- w[ok]
    if (nrow(X) < 10) return(tibble::tibble(v = NA_real_, u = NA_real_, n = nrow(X)))
    beta <- ridge_wls(X, y, w, LAMBDA)
    tibble::tibble(v = beta[1], u = beta[2], n = nrow(X))
  }) %>%
  dplyr::ungroup()

# Attach v,u back to the full grid and compute S_hat
est_df <- grad_df %>%
  left_join(vu_est, by = c("cohort", "tau")) %>%
  mutate(
    S_hat = drho_dtau - v * drho_dd - u * drho_dt
  )

# Diagnostics: fit quality per panel
diag_panel <- est_df %>%
  filter(!is.na(v), is.finite(S_hat)) %>%
  group_by(cohort, tau) %>%
  summarise(
    n_cells = n(),
    rmse    = sqrt(mean(S_hat^2, na.rm = TRUE)),
    r2      = 1 - var(S_hat, na.rm = TRUE) / var(drho_dtau, na.rm = TRUE),
    v_hat   = first(v),
    u_hat   = first(u),
    .groups = "drop"
  )

print(diag_panel, n = 18)

####################### DIAGNOSE #########################

########################################################
# DIAGNOSTIC PANEL for panel-constant drift estimates
# INPUTS required from your code:
#   1) fit_df   = panel-ready derivative & weight data before fitting
#   2) vu_est   = your ridge results (v, u per cohort × tau)
#   3) est_df   = joined drift + S_hat residuals
########################################################

library(dplyr)
library(ggplot2)

# ---- 1) Condition number per panel ----
# Replace 'fit_df' with your actual pre-fit data object
cond_df <- fit_df %>%
  group_by(cohort, tau) %>%
  summarise(
    cond_num = {
      X <- as.matrix(cbind(drho_dd, drho_dt))
      ok <- is.finite(rowSums(X))
      X <- X[ok, , drop = FALSE]
      if (nrow(X) > 2) kappa(crossprod(X)) else NA_real_
    },
    .groups = "drop"
  )

# ---- 2) Residual distribution plots ----
# Replace 'est_df' with your actual object with S_hat
p_resid_hist <- ggplot(est_df, aes(S_hat)) +
  geom_histogram(bins = 40, fill = "steelblue", alpha = 0.7) +
  facet_grid(cohort ~ tau) +
  labs(title = "Residual source term distribution",
       x = expression(hat(S)), y = "Count") +
  theme_minimal()

# ---- 3) RMSE vs. drift magnitude ----
# Replace 'diag_panel' with your existing RMSE/R2 summary if you already made it
diag_panel <- est_df %>%
  filter(!is.na(v), is.finite(S_hat)) %>%
  group_by(cohort, tau) %>%
  summarise(
    n_cells = n(),
    rmse    = sqrt(mean(S_hat^2, na.rm = TRUE)),
    drift_mag = sqrt(first(v)^2 + first(u)^2),
    .groups = "drop"
  )

p_rmse_vs_drift <- ggplot(diag_panel, aes(drift_mag, rmse)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "RMSE vs. drift magnitude",
       x = "Drift magnitude |(v,u)|", y = "RMSE") +
  theme_minimal()

# ---- 4) Time series of v and u per cohort ----
p_v_time <- ggplot(vu_est, aes(x = tau, y = v, color = cohort)) +
  geom_line() + geom_point() +
  labs(title = "Radial drift v_c over time",
       x = "Mid-year of panel", y = "v_c (km/year)") +
  theme_minimal()

p_u_time <- ggplot(vu_est, aes(x = tau, y = u, color = cohort)) +
  geom_line() + geom_point() +
  labs(title = "Temporal drift u_c over time",
       x = "Mid-year of panel", y = "u_c (h/year)") +
  theme_minimal()

# ---- 5) OPTIONAL: λ sensitivity test ----
# Adjust lambdas as needed
lambda_seq <- 10^seq(-4, 1, length.out = 8)

ridge_sensitivity <- function(df_panel, lambda_seq) {
  X <- as.matrix(df_panel[, c("drho_dd", "drho_dt")])
  y <- df_panel$drho_dtau
  w <- df_panel$w
  W12 <- sqrt(pmax(w, 0))
  Xw  <- X * W12
  yw  <- y * W12
  results <- lapply(lambda_seq, function(lam) {
    beta <- solve(crossprod(Xw) + lam * diag(2), crossprod(Xw, yw))
    rmse <- sqrt(mean((y - X %*% beta)^2))
    data.frame(lambda = lam, v = beta[1], u = beta[2], rmse = rmse)
  })
  do.call(rbind, results)
}

# Example: run for first panel in your data
test_panel <- fit_df %>% filter(cohort == first(cohort), tau == first(tau))
sens_df <- ridge_sensitivity(test_panel, lambda_seq)

p_lambda <- ggplot(sens_df, aes(lambda, rmse)) +
  geom_line() + geom_point() +
  scale_x_log10() +
  labs(title = "λ sensitivity (example panel)",
       x = expression(lambda), y = "RMSE") +
  theme_minimal()

########################################################
# Display or save diagnostics
print(cond_df)
print(p_resid_hist)
print(p_rmse_vs_drift)
print(p_v_time)
print(p_u_time)
print(p_lambda)
########################################################

#NEW START

# --- Minimal diagnostics (time on x, distance on y) -----------------------
# Needs:
#   fit_df : columns cohort, tau, d, t, w, drho_dd, drho_dt, drho_dtau
#   vu_est : panel-level (cohort, tau, v, u [, lambda])

library(dplyr)
library(ggplot2)
library(scales)
library(tibble)
library(MASS)   # for ginv if needed

# ---------- Tidy inputs ----------
vu_tbl <- tibble::as_tibble(vu_est)
stopifnot(all(c("cohort","tau") %in% names(vu_tbl)))
if (!"lambda" %in% names(vu_tbl)) {
  vu_tbl <- vu_tbl %>% dplyr::mutate(lambda = 1e-3)
}

fit_df <- fit_df %>%
  dplyr::filter(
    is.finite(drho_dd), is.finite(drho_dt), is.finite(drho_dtau),
    is.finite(w), is.finite(d), is.finite(t)
  )

# ---------- Per-panel metrics (ridge hat-trace & condition) ----------
panel_metrics <- function(df_panel, lambda_val) {
  if (nrow(df_panel) < 3) return(tibble(traceH = NA_real_, cond = NA_real_))
  X   <- as.matrix(df_panel[, c("drho_dd","drho_dt")])
  w   <- pmax(df_panel$w, 0)
  W12 <- sqrt(w)
  Xw  <- X * W12
  XtWX_lam     <- crossprod(Xw) + lambda_val * diag(2)
  XtWX_lam_inv <- tryCatch(solve(XtWX_lam), error = function(e) MASS::ginv(XtWX_lam))
  h <- rowSums((Xw %*% XtWX_lam_inv) * Xw)  # diag of ridge/WLS hat
  tibble(traceH = sum(h), cond = kappa(XtWX_lam))
}

met_df <- fit_df %>%
  dplyr::left_join(vu_tbl %>% dplyr::select(cohort, tau, lambda),
                   by = c("cohort","tau")) %>%
  dplyr::mutate(lambda = ifelse(is.na(lambda), 1e-3, lambda)) %>%
  dplyr::group_by(cohort, tau) %>%
  dplyr::group_modify(~ tibble::as_tibble(panel_metrics(.x, lambda_val = unique(.x$lambda)))) %>%
  dplyr::ungroup()

# ---------- Residual diagnostics ----------
# Join v,u and compute residuals
resid_df <- fit_df %>%
  dplyr::left_join(vu_tbl %>% dplyr::select(cohort, tau, v, u),
                   by = c("cohort","tau")) %>%
  dplyr::mutate(
    fitted = -(v * drho_dd + u * drho_dt),
    resid  = drho_dtau - fitted
  ) %>%
  dplyr::filter(is.finite(resid), is.finite(fitted))

# Panel RMSE + means
resid_summ <- resid_df %>%
  dplyr::group_by(cohort, tau) %>%
  dplyr::summarise(
    rmse   = sqrt(mean(resid^2, na.rm = TRUE)),
    mean   = mean(resid, na.rm = TRUE),
    median = median(resid, na.rm = TRUE),
    .groups = "drop"
  )

# Standardize residuals by each panel’s RMSE
resid_std <- resid_df %>%
  dplyr::left_join(resid_summ, by = c("cohort","tau")) %>%
  dplyr::mutate(z = resid / rmse)

# (A) Standardized residuals (z): same scale across panels
p_resid_std <- ggplot(resid_std, aes(z)) +
  geom_histogram(bins = 40, fill = "steelblue", alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = c(-2, 2), linetype = "dotted") +
  facet_grid(cohort ~ tau, scales = "fixed") +
  labs(title = "Residuals standardized by panel RMSE",
       x = "Standardized residual (z = residual / RMSE)",
       y = "Count") +
  theme_minimal()

# (B) Raw residuals per panel (scientific labels; free x)
p_resid_hist <- ggplot(resid_df, aes(resid)) +
  geom_histogram(bins = 40, fill = "steelblue", alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(cohort ~ tau, scales = "free_x") +
  scale_x_continuous(labels = scales::label_scientific(digits = 2)) +
  labs(title = "Residual source term distribution by panel",
       x = expression(hat(S)~"(residual, density/year)"),
       y = "Count") +
  theme_minimal()

# (C) Residuals vs fitted (binned) with FEWER ticks
tick_x <- scales::breaks_extended(n = 4)
tick_y <- scales::breaks_extended(n = 5)
lab_sc <- scales::label_scientific(digits = 1)

p_resid_fit <- ggplot(resid_df, aes(fitted, resid)) +
  geom_bin2d(bins = 50) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(cohort ~ tau, scales = "free") +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = tick_x, labels = lab_sc) +
  scale_y_continuous(breaks = tick_y, labels = lab_sc) +
  labs(title = "Residuals vs fitted (binned)",
       x = expression("Fitted  ("*-v %.% partialdiff(rho, d) - u %.% partialdiff(rho, t)*")"),
       y = expression(hat(S))) +
  theme_minimal()

# Quick stats: % within ±2*RMSE and corr(resid, fitted)
fit_resid_stats <- resid_df %>%
  dplyr::left_join(resid_summ, by = c("cohort","tau")) %>%
  dplyr::summarise(
    n          = dplyr::n(),
    pct_in_2rm = mean(abs(resid) <= 2*rmse, na.rm = TRUE),
    corr       = cor(fitted, resid, use = "complete.obs"),
    .by        = c(cohort, tau)
  ) %>%
  dplyr::arrange(cohort, tau)

# ±2*RMSE banded version (also with fewer ticks)
band_df <- resid_summ %>%
  dplyr::transmute(cohort, tau, xmin = -Inf, xmax = Inf,
                   ymin = -2*rmse, ymax = 2*rmse)

p_resid_fit_banded <- ggplot(resid_df, aes(fitted, resid)) +
  geom_rect(data = band_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "grey92") +
  geom_bin2d(bins = 50) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(cohort ~ tau, scales = "free") +
  scale_fill_viridis_c(name = "count") +
  scale_x_continuous(breaks = tick_x, labels = lab_sc) +
  scale_y_continuous(breaks = tick_y, labels = lab_sc) +
  labs(title = "Residuals vs fitted with ±2·RMSE band",
       x = expression("Fitted  ("*-v %.% partialdiff(rho, d) - u %.% partialdiff(rho, t)*")"),
       y = expression(hat(S))) +
  theme_minimal()

# Optional: clip extreme y by facet-specific quantiles (ticks simplified)
clip_df <- resid_df %>%
  dplyr::summarise(
    ylo = quantile(resid, 0.01, na.rm = TRUE),
    yhi = quantile(resid, 0.99, na.rm = TRUE),
    .by = c(cohort, tau)
  )
resid_df2 <- resid_df %>%
  dplyr::left_join(clip_df, by = c("cohort","tau")) %>%
  dplyr::mutate(resid_clip = pmax(pmin(resid, yhi), ylo))

p_resid_fit_clipped <- ggplot(resid_df2, aes(fitted, resid_clip)) +
  geom_bin2d(bins = 50) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(cohort ~ tau, scales = "free") +
  scale_fill_viridis_c(name = "count") +
  scale_x_continuous(breaks = tick_x, labels = lab_sc) +
  scale_y_continuous(breaks = tick_y, labels = lab_sc) +
  labs(title = "Residuals vs fitted (99% clipped, per facet)",
       x = expression("Fitted  ("*-v %.% partialdiff(rho, d) - u %.% partialdiff(rho, t)*")"),
       y = expression(hat(S))) +
  theme_minimal()

# ---------- Complexity & stability ----------
p_edf <- ggplot(met_df, aes(tau, traceH, color = cohort)) +
  geom_line() + geom_point(size = 2) +
  labs(title = "Effective degrees of freedom (ridge hat trace)",
       x = "Mid-year", y = "trace(H)") +
  theme_minimal()

p_cond <- ggplot(met_df, aes(tau, cond, color = cohort)) +
  geom_line() + geom_point(size = 2) +
  labs(title = "Condition number of X'WX + λI by panel",
       x = "Mid-year", y = "cond(X'WX + λI)") +
  theme_minimal()

# ---------- Identification strength |∇ρ| (time on x, distance on y) ----------
grad_df <- fit_df %>%
  dplyr::mutate(
    grad_mag = sqrt(drho_dd^2 + drho_dt^2),
    tau_f = factor(tau, levels = sort(unique(tau))),
    cohort = factor(cohort)
  )
vmax <- quantile(grad_df$grad_mag, 0.995, na.rm = TRUE)
d_range <- diff(range(grad_df$d, na.rm = TRUE))

p_grad <- ggplot(grad_df, aes(t, d, fill = grad_mag)) +
  geom_tile() +
  facet_grid(cohort ~ tau_f) +
  scale_fill_viridis_c(option = "magma", trans = "sqrt",
                       limits = c(0, vmax), oob = squish,
                       name = expression("|∇ρ|")) +
  labs(title = "Gradient magnitude of density (identification strength)",
       x = "Time of day t (h)", y = "Distance d (km)") +
  coord_fixed(ratio = d_range / 24) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

# ---------- Show the key plots & tables ----------
print(p_resid_std)
print(p_resid_hist)
print(p_resid_fit_banded)  # preferred residuals vs fitted

ggsave(
  filename = "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Generalizing Pseudo Cohorts/residuals.png",
  plot     = p_resid_fit_banded,
  width    = 16.5,   # Suitable for landscape A3 or large A4
  height   = 11.7,
  units    = "in",
  dpi      = 300,
  device   = "png"
)


print(p_edf)
print(p_cond)
print(p_grad)

print(resid_summ)
print(fit_resid_stats)

# ---- Headline numbers for the paper ----
headline <- fit_resid_stats %>%
  summarise(
    panels      = dplyr::n(),
    pct2rm_min  = min(pct_in_2rm),
    pct2rm_med  = median(pct_in_2rm),
    pct2rm_max  = max(pct_in_2rm),
    corr_min    = min(corr, na.rm = TRUE),
    corr_med    = median(corr, na.rm = TRUE),
    corr_max    = max(corr, na.rm = TRUE)
  ) %>%
  mutate(
    pct2rm_min = scales::percent(pct2rm_min, accuracy = 0.1),
    pct2rm_med = scales::percent(pct2rm_med, accuracy = 0.1),
    pct2rm_max = scales::percent(pct2rm_max, accuracy = 0.1),
    across(starts_with("corr"), ~round(.x, 3))
  )

print(headline)


#NEW END

#########################

library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(42)

#--- helper: bootstrap over panels (here: by tau) ---
boot_cohort <- function(df, B = 1000) {
  taus <- unique(df$tau)
  nT  <- length(taus)
  reps <- replicate(B, {
    samp_tau <- sample(taus, nT, replace = TRUE)
    df %>% filter(tau %in% samp_tau) %>%
      summarise(v = mean(v, na.rm=TRUE),
                u = mean(u, na.rm=TRUE)) %>% unlist()
  })
  tibble(
    v_mean = mean(reps["v",]),
    v_lo   = quantile(reps["v",], 0.025),
    v_hi   = quantile(reps["v",], 0.975),
    u_mean = mean(reps["u",]),
    u_lo   = quantile(reps["u",], 0.025),
    u_hi   = quantile(reps["u",], 0.975)
  )
}

# pooled table by cohort
coef_tab <- vu_est %>%
  group_by(cohort) %>%
  group_modify(~boot_cohort(.x, B = 1000)) %>%
  ungroup()

# (optional) tidy for dot–whisker plot
coef_long <- coef_tab %>%
  transmute(cohort,
            param = "v", mean = v_mean, lo = v_lo, hi = v_hi) %>%
  bind_rows(
    coef_tab %>% transmute(cohort,
                           param = "u", mean = u_mean, lo = u_lo, hi = u_hi)
  )

p_coef <- ggplot(coef_long,
                 aes(x = mean, y = cohort)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.15) +
  facet_wrap(~param, scales = "free_x") +
  labs(x = "Estimate (units of v or u)", y = NULL,
       title = "Ridge-estimated drift parameters by cohort (95% bootstrap CI)") +
  theme_minimal()

##########################################################

library(dplyr)

# Function to get R2 for ridge fit
ridge_fit_stats <- function(df, lambda) {
  X <- as.matrix(df[, c("drho_dd", "drho_dt")])
  y <- df$drho_dtau
  w <- df$w
  W12 <- sqrt(pmax(w, 0))
  Xw <- X * W12
  yw <- y * W12
  
  beta <- solve(crossprod(Xw) + lambda * diag(2), crossprod(Xw, yw))
  y_hat <- X %*% beta
  
  rmse <- sqrt(mean((y - y_hat)^2))
  r2 <- 1 - sum((y - y_hat)^2) / sum((y - mean(y))^2)
  
  data.frame(v = beta[1], u = beta[2], RMSE = rmse, R2 = r2)
}

# Pick your chosen lambda
lambda_chosen <- 1e-4

# Compute per-cohort stats pooled over all tau
coef_stats <- fit_df %>%
  group_by(cohort) %>%
  group_modify(~ridge_fit_stats(.x, lambda_chosen)) %>%
  ungroup()

print(coef_stats)

#############################################################

# --- Inputs expected ---
# vu_est: data.frame with columns cohort, tau, v, u  (ridge estimates per panel tau)
# If you already have bootstrapped CIs, skip to the plotting block and adapt 'coef_long'.

library(dplyr)
library(tidyr)
library(ggplot2)
set.seed(42)

# Bootstrap CIs by resampling panels (tau) within each cohort
boot_cohort <- function(df, B = 1000) {
  taus <- unique(df$tau); nT <- length(taus)
  reps <- replicate(B, {
    samp <- sample(taus, nT, replace = TRUE)
    xs <- df %>% filter(tau %in% samp) %>%
      summarise(v = mean(v, na.rm=TRUE),
                u = mean(u, na.rm=TRUE))
    unlist(xs)
  })
  tibble(
    v_mean = mean(reps["v",]),
    v_lo   = quantile(reps["v",], 0.025),
    v_hi   = quantile(reps["v",], 0.975),
    u_mean = mean(reps["u",]),
    u_lo   = quantile(reps["u",], 0.025),
    u_hi   = quantile(reps["u",], 0.975)
  )
}

coef_tab <- vu_est %>%
  group_by(cohort) %>%
  group_modify(~boot_cohort(.x, B = 1000)) %>%
  ungroup()

# Tidy to long for plotting
coef_long <- bind_rows(
  coef_tab %>%
    transmute(cohort, param = "v_c (km/yr)", mean = v_mean, lo = v_lo, hi = v_hi),
  coef_tab %>%
    transmute(cohort, param = "u_c (h/yr)",  mean = u_mean, lo = u_lo, hi = u_hi)
)

# Order cohorts top-to-bottom as in your tables
coef_long$cohort <- factor(coef_long$cohort,
                           levels = rev(sort(unique(as.character(coef_long$cohort)))))

# --- Dot–whisker plot ---
p_dw <- ggplot(coef_long, aes(x = mean, y = cohort)) +
  geom_point(shape = 21, fill = "grey60", color = "black", size = 2) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.15, color = "black") +
  facet_wrap(~param, scales = "free_x") +
  labs(x = "Estimate (95% CI)", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.3, colour = "grey85"),
    strip.background = element_blank(),
    strip.text = element_text(face = "plain")
  )

p_dw

# Save for Overleaf
ggsave("ridge_coef_dotwhisker.pdf", p_dw, width = 3.2, height = 2.8)



##########################################################


# Optional visuals: quiver for (v,u) and heatmap for S_hat
# (Here v,u are scalars per panel; arrows show direction only.)

library(ggplot2)
# Thin grid for plotting arrows
set.seed(1)
vecs <- est_df %>%
  filter(!is.na(v)) %>%
  group_by(cohort, tau) %>%
  group_modify(~ dplyr::slice_sample(.x, n = min(400, nrow(.x)))) %>%  # <- key change
  ungroup() %>%
  mutate(t_end = t + 0.5 * u, d_end = d + 0.5 * v)

p_vec <- library(ggplot2)
ggplot(vecs, aes(x = t, y = d)) +
  geom_segment(aes(xend = t_end, yend = d_end),
               arrow = arrow(length = unit(0.07, "in"))) +
  facet_grid(cohort ~ tau) +
  labs(x = "Hour of day", y = "Radial distance (km)",
       title = "Baseline drift field (global v, u per panel)") +
  theme_minimal()


p_S <- ggplot(est_df, aes(t, d, fill = S_hat)) +
  geom_raster() +
  facet_grid(cohort ~ tau) +
  scale_fill_viridis_c(option = "magma") +
  labs(x = "Hour of day", y = "Radial distance (km)", fill = expression(hat(S)),
       title = "Implied source/sink term  " %+% expression(hat(S))) +
  theme_minimal()

# Print or save:
# print(p_vec); print(p_S)
# ggsave("figures/drift_quiver_baseline.pdf", p_vec, width = 9, height = 7)
# ggsave("figures/source_residual_baseline.pdf", p_S, width = 9, height = 7)

################################################
# Estimation of drift and speed plots ##########
################################################

install.packages("mgcv")
library(mgcv)

# Fit GAMs for v and u per panel
smooth_df <- grad_df %>%
  filter(!is.na(drho_dt), !is.na(drho_dd), !is.na(drho_dtau)) %>%
  group_by(cohort, tau) %>%
  group_modify(~{
    # Local regression (GAM)
    fit <- gam(drho_dtau ~ s(d, t, k = 20) + 
                 s(d, k = 10) + s(t, k = 10),
               data = .x, weights = rho)
    # Predict partial derivatives (v,u)
    pred_grid <- expand_grid(
      d = seq(0, R_MAX, length.out = 20),
      t = seq(0, 24, length.out = 20)
    )
    pred_grid$v <- predict(fit, newdata = pred_grid, type = "terms")[,"s(d)"] # Example
    pred_grid$u <- predict(fit, newdata = pred_grid, type = "terms")[,"s(t)"] # Example
    pred_grid
  }) %>%
  ungroup()

# Add magnitude and arrow endpoints
smooth_df <- smooth_df %>%
  mutate(speed = sqrt(v^2 + u^2),
         t_end = t + 0.5*u,
         d_end = d + 0.5*v)

# Plot
field <- ggplot(smooth_df, aes(x = t, y = d)) +
  geom_segment(
    aes(xend = t_end, yend = d_end, color = speed),
    arrow = arrow(length = unit(0.07, "in")),
    alpha = 0.7
  ) +
  scale_color_viridis_c(
    name = "Speed",
    guide = guide_colorbar(
      barwidth = unit(0.4, "cm"),  # thin vertical bar
      barheight = unit(5, "cm")
    )
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  ) +
  scale_y_continuous(
    name   = "r (Radial distance from home)",
    breaks = seq(0, 120, by = 30)
  ) +
  scale_x_continuous(
    name = "t (Time of day in hours)",
    breaks = seq(0, 24, by = 6)
  ) +
  facet_grid(cohort ~ tau) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    legend.title     = element_text(face = "bold"),
    strip.text.y     = element_text(face = "bold"),
    strip.text.x     = element_text(face = "bold")
  )

field

# 7) Save the plot (optional, adjust size for landscape or full-page portrait)
ggsave(
  filename = "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Generalizing Pseudo Cohorts/mobility_speed_6x6_fin.png",
  plot     = field,
  width    = 16.5,   # Suitable for landscape A3 or large A4
  height   = 11.7,
  units    = "in",
  dpi      = 300,
  device   = "png"
)


############### NEW SPEED PLOT OVER COHORTS ##########################

# ------------------------------------------------------------
# Flux magnitude fields (constant drift) + speed comparisons
# Requires: grad_df (ρ, ∂ρ/∂t, ∂ρ/∂d, ∂ρ/∂τ), vu_est (v,u per cohort×τ), R_MAX
# ------------------------------------------------------------

# ------------------------------------------------------------
# Flux-style plot: density heatmap + arrows scaled by |J| = ρ·s
# ------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(viridis)
library(grid)

# 1) Panel-level speeds and join back to grid
speed_df <- vu_est %>%
  filter(is.finite(v), is.finite(u)) %>%
  mutate(speed = sqrt(v^2 + u^2))

const_df <- grad_df %>%
  inner_join(speed_df, by = c("cohort","tau")) %>%
  mutate(
    flux  = pmax(rho * speed, 0),
    tau   = factor(tau, levels = sort(unique(tau))),   # keep a stable order
    cohort= factor(cohort, levels = unique(cohort))
  )

# 2) (Optional) very sparse arrows: one arrow per panel center
#    Toggle ON by setting add_arrows <- TRUE
add_arrows <- FALSE
if (add_arrows) {
  arrows_df <- speed_df %>%
    mutate(
      # place an arrow near panel center just for orientation
      t = 12, d = R_MAX/2,
      k = 0.8,                        # visual length
      t_end = t + k * u / pmax(speed, 1e-9),
      d_end = d + k * v / pmax(speed, 1e-9)
    )
}

# 3) Plot: flux magnitude heatmap (+ optional single arrow per panel)
flux_lim <- quantile(const_df$flux, 0.995, na.rm = TRUE)  # clamp to expose greens

p_flux <- ggplot(const_df, aes(x = t, y = d)) +
  geom_raster(aes(fill = pmin(flux, flux_lim))) +
  # light contours of rho for context (can comment out)
  geom_contour(aes(z = rho), color = "white", bins = 6, size = 0.2, alpha = 0.5) +
  # optional arrows
  { if (add_arrows)
    geom_segment(
      data = arrows_df,
      aes(x = t, y = d, xend = t_end, yend = d_end),
      arrow = arrow(type = "closed", length = unit(0.10, "in")),
      inherit.aes = FALSE, color = "black", linewidth = 0.6, alpha = 0.9
    ) else NULL } +
  scale_fill_viridis_c(
    name   = expression("|J| = "~rho~"."~s),
    option = "C", begin = 0.20, end = 0.95, trans = "sqrt",
    limits = c(0, flux_lim), oob = scales::squish
  ) +
  scale_x_continuous(
    "Clock time (hours)",
    breaks = seq(0, 24, 6),        # tick positions
    labels = c("","6","12","18","")  # custom labels, last one blank
  ) +
  scale_y_continuous("Distance from home (km)", breaks = seq(0, R_MAX, by = 15), limits = c(0, R_MAX)) +
  coord_cartesian(expand = FALSE) +
  facet_grid(cohort ~ tau) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text.y     = element_text(face = "bold"),
    strip.text.x     = element_text(face = "bold"),
    legend.position  = "right"
  )

p_flux

# Save if you like
ggsave(
  "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Generalizing Pseudo Cohorts/mobility_flux_simple.png",
  p_flux, width = 16.5, height = 11.7, units = "in", dpi = 300
)

library(dplyr)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(scales)  # for alpha()
library(RColorBrewer)

# Prep
speed_df2 <- speed_df %>%
  mutate(
    tau_num = suppressWarnings(as.numeric(as.character(tau))),
    cohort  = as.factor(cohort)
  )

# latest point per cohort for direct labels
lab_df <- speed_df2 %>%
  group_by(cohort) %>%
  slice_max(tau_num, n = 1, with_ties = FALSE) %>%
  ungroup()

x_breaks <- sort(unique(speed_df2$tau_num))

# Cohort levels and palette/linetypes sized to match
cohorts <- levels(speed_df2$cohort)
n <- length(cohorts)

# Set2 has up to 8 colors; extend if needed
base_cols <- brewer.pal(min(max(n, 3), 8), "Set2")
if (n > length(base_cols)) {
  pal_vec <- colorRampPalette(base_cols)(n)
} else {
  pal_vec <- base_cols[seq_len(n)]
}
names(pal_vec) <- cohorts
fill_pal <- alpha(pal_vec, 0.20)

# Linetypes with correct length & names
lt_seq <- c("solid","dashed","dotdash","twodash","longdash","longdash")
lt_vec <- setNames(rep(lt_seq, length.out = n), cohorts)

# Plot
p_speed <- ggplot(speed_df2, aes(x = tau_num, y = speed, group = cohort)) +
  annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.6) +
  geom_line(aes(color = cohort), linewidth = 0.7, alpha = 0.35) +
  geom_point(aes(color = cohort), shape = 21, fill = "white",
             size = 2.4, stroke = 0.9, alpha = 0.8) +
  geom_smooth(
    aes(color = cohort, fill = cohort, linetype = cohort),
    method    = "lm",
    formula   = y ~ x,
    se        = TRUE,
    level     = 0.90,
    linewidth = 1.2,
    alpha     = 0.25
  ) +
  geom_text_repel(
    data = lab_df,
    aes(label = cohort, color = cohort),
    nudge_x = 1.0, direction = "y", hjust = 0,
    size = 3.8, segment.color = NA, show.legend = FALSE
  ) +
  scale_x_continuous(
    "Survey mid-year (τ)",
    breaks = x_breaks, labels = x_breaks,
    expand = expansion(mult = c(0.02, 0.16)) # a bit more room for labels
  ) +
  scale_y_continuous("Drift speed  s = ||(v,u)||") +
  scale_color_manual(values = pal_vec, breaks = cohorts) +
  scale_fill_manual(values  = fill_pal, breaks = cohorts) +
  scale_linetype_manual(values = lt_vec, breaks = cohorts) +
  guides(color = "none", fill = "none", linetype = guide_legend(title = NULL)) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90"),
    legend.position = "bottom",
    plot.margin = margin(10, 34, 10, 10)
  )

p_speed
# A4 width, reasonable height
ggsave(
  filename = "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Generalizing Pseudo Cohorts/mobility_speed_trends_v3.png",
  plot     = p_speed,
  width    = 8.27, height = 5.5, units = "in", dpi = 300
)


##################### NEW DIRECTIONAL PLOT


library(dplyr)
library(ggplot2)

# Example: one arrow per panel, angle from (u,v)
arrows_clean <- dir_df %>%
  mutate(
    t = 12, d = R_MAX/2,     # place arrow at center
    len = 5,                 # visual scaling
    xend = t + len * u,
    yend = d + len * v
  )

p_dir_clean <- ggplot(arrows_clean, aes(x = t, y = d, xend = xend, yend = yend)) +
  geom_segment(
    arrow = arrow(type = "open", length = unit(0.15, "in")),
    size = 1, lineend = "round", color = "black"
  ) +
  facet_grid(cohort ~ tau) +
  scale_x_continuous("Clock time (hours)",
                     breaks = seq(0, 24, 6),
                     labels = c("0","6","12","18",""),
                     limits = c(0, 24)) +
  scale_y_continuous("Distance from home (km)", limits = c(0,R_MAX)) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(face = "bold"),
    strip.text.y = element_text(face = "bold")
  )

p_dir_clean

###

library(dplyr)
library(ggplot2)
library(grid)      # for arrow()
library(viridis)   # for scale_color_viridis_c()

# 0) Per-panel drift + speed and rescaling
dir_df2 <- dir_df %>%
  mutate(
    speed = sqrt(v^2 + u^2)
  )

s_min <- min(dir_df2$speed, na.rm = TRUE)
s_max <- max(dir_df2$speed, na.rm = TRUE)
s_norm <- function(s) if (s_max > s_min) (s - s_min)/(s_max - s_min) else 0

# 1) Build arrows: center position, unit direction, shaft length scaled by speed
L0 <- 6.0  # min shaft length (plot units)
L1 <- 15.5  # max shaft length (plot units)

arrows_clean <- dir_df2 %>%
  mutate(
    t = 12, d = R_MAX/2,
    u_hat = ifelse(speed > 0, u/speed, 0),
    v_hat = ifelse(speed > 0, v/speed, 0),
    len   = L0 + (L1 - L0) * s_norm(speed),
    xend  = t + len * u_hat,
    yend  = d + len * v_hat,
    # three bins for arrowhead size (since arrow() isn't vectorized)
    head_bin = cut(s_norm(speed),
                   breaks = c(-Inf, 1/3, 2/3, Inf),
                   labels = c("small","medium","large"),
                   right  = TRUE)
  )

# 2) Base plot (axes, facets). We'll add three segment layers for head sizes.
p_dir_clean <- ggplot(arrows_clean, aes(x = t, y = d, xend = xend, yend = yend, color = speed)) +
  scale_color_viridis_c("Drift speed", option = "C") +
  facet_grid(cohort ~ tau) +
  scale_x_continuous("Clock time (hours)",
                     breaks = seq(0, 24, 6),
                     labels = c("0","6","12","18",""),
                     limits = c(0, 24)) +
  scale_y_continuous("Distance from home (km)", limits = c(0, R_MAX)) +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(face = "bold"),
    strip.text.y = element_text(face = "bold")
  )

# 3) Add arrows with different head sizes per bin (small/med/large)
p_dir_clean <-
  p_dir_clean +
  geom_segment(
    data = subset(arrows_clean, head_bin == "small"),
    arrow = arrow(type = "open", length = unit(0.06, "in"), angle = 18),
    linewidth = 1.5, lineend = "round"
  ) +
  geom_segment(
    data = subset(arrows_clean, head_bin == "medium"),
    arrow = arrow(type = "open", length = unit(0.10, "in"), angle = 18),
    linewidth = 1.5, lineend = "round"
  ) +
  geom_segment(
    data = subset(arrows_clean, head_bin == "large"),
    arrow = arrow(type = "open", length = unit(0.14, "in"), angle = 18),
    linewidth = 1.5, lineend = "round"
  )

p_dir_clean

# (Optional) Save
ggsave(
  filename = "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Generalizing Pseudo Cohorts/mobility_dir_trends.png",
  plot     = p_dir_clean,
  width    = 11.7, height = 6.5, units = "in", dpi = 300
)

########################### 2 × 2 (same template, denser grid) ###########################

#library(mgcv)

########################### 2 × 2 — high-resolution version ###########################
# Requires: grad_df, yearbin_mid, smooth_df (for color limits), R_MAX, mgcv loaded

#library(dplyr); library(tidyr); library(ggplot2); library(mgcv)

# Panels to show
cohorts_2x2 <- c("1960–1970", "2000–2024")
bins_2x2    <- c("2007–2009","2022–2024")
taus_2x2    <- unname(yearbin_mid[bins_2x2])   # c(2008, 2023)

# Keep colors consistent with the 6×6
sp_lims <- range(smooth_df$speed, na.rm = TRUE)

# --- Resolution controls ---
pred_d_n    <- 100L   # radial points (0..R_MAX)
pred_t_n    <- 192L   # time points (0..24)  ~ every 0.125 h
arrow_scale <- 0.35   # shorter arrows to reduce overlap at high density
stroke_w    <- 0.2    # line width for arrows

# Fit GAMs and predict ONLY for the four panels on a dense grid
smooth_df_2x2_dense <- grad_df %>%
  filter(cohort %in% cohorts_2x2, tau %in% taus_2x2,
         is.finite(drho_dt), is.finite(drho_dd), is.finite(drho_dtau), is.finite(rho)) %>%
  group_by(cohort, tau) %>%
  group_modify(~{
    fit <- gam(drho_dtau ~ s(d, t, k = 20) + s(d, k = 10) + s(t, k = 10),
               data = .x, weights = rho)
    
    pred_grid <- expand_grid(
      d = seq(0, R_MAX, length.out = pred_d_n),
      t = seq(0, 24,   length.out = pred_t_n)
    )
    
    trm <- predict(fit, newdata = pred_grid, type = "terms")
    tibble(
      t = pred_grid$t,
      d = pred_grid$d,
      v = trm[, "s(d)"],
      u = trm[, "s(t)"]
    )
  }) %>%
  ungroup() %>%
  mutate(
    speed      = sqrt(v^2 + u^2),
    t_end      = t + arrow_scale * u,
    d_end      = d + arrow_scale * v,
    cohort_fac = factor(as.character(cohort), levels = cohorts_2x2),
    yearbin_fac= factor(ifelse(tau == taus_2x2[1], bins_2x2[1], bins_2x2[2]),
                        levels = bins_2x2)
  )

# Plot — same template as your 6×6 field
field_2x2_dense <- ggplot(smooth_df_2x2_dense, aes(x = t, y = d)) +
  geom_segment(
    aes(xend = t_end, yend = d_end, color = speed),
    arrow = grid::arrow(length = grid::unit(0.07, "in")),
    alpha = 0.65, linewidth = stroke_w
  ) +
  scale_color_viridis_c(
    name   = "Speed",
    limits = sp_lims, oob = scales::squish,
    guide  = guide_colorbar(barwidth = grid::unit(0.4, "cm"),
                            barheight = grid::unit(5, "cm"))
  ) +
  scale_y_continuous(
    name   = "r (Radial distance from home)",
    breaks = seq(0, 120, by = 30)
  ) +
  scale_x_continuous(
    name   = "t (Time of day in hours)",
    breaks = seq(0, 24, by = 6)
  ) +
  facet_grid(rows = vars(cohort_fac), cols = vars(yearbin_fac), drop = FALSE) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    legend.title     = element_text(face = "bold"),
    strip.text.y     = element_text(face = "bold"),
    strip.text.x     = element_text(face = "bold")
  )

field_2x2_dense

# Save
ggsave(
  "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Generalizing Pseudo Cohorts/mobility_speed_2x2_fin.png",
  plot = field_2x2_dense, width = 11.69, height = 8.27, units = "in", dpi = 300
)

################################  DRIFT VECTORS ON HEATMAP  ################################
# Inputs expected in the session:
#   grad_df     : derivatives grid with columns cohort, tau (numeric), t, d, rho, drho_dt, drho_dd, drho_dtau
#   yearbin_mid : named numeric vector (e.g. c("2007–2009"=2008, ..., "2022–2024"=2023))

library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(scales)
library(mgcv)

# ---------- helpers ----------
# (Optional) pick a font that has the → glyph
candidates  <- c("DejaVu Sans", "Arial", "Liberation Sans", "Segoe UI Symbol")
arrow_font  <- "sans"
if (requireNamespace("systemfonts", quietly = TRUE)) {
  avail <- systemfonts::system_fonts()$family
  hit   <- intersect(candidates, avail)
  if (length(hit)) arrow_font <- hit[1]
}

labs <- names(yearbin_mid)
mids <- unname(yearbin_mid)
to_num <- function(x) if (is.factor(x)) as.numeric(as.character(x)) else as.numeric(x)
as_yearbin <- function(tau_num) factor(labs[ match(round(tau_num), round(mids)) ], levels = labs)

# Range for radius if not defined
if (!exists("R_MAX")) {
  R_MAX <- grad_df %>% summarise(mx = quantile(d, 0.99, na.rm = TRUE)) %>% pull(mx)
  R_MAX <- max(30, as.numeric(R_MAX))
}

# ---------- 1) Fit local drift (u, v) per panel and predict on a modest grid ----------
pred_D <- 40L   # radial grid
pred_T <- 48L   # time grid
smooth_df <- grad_df %>%
  filter(is.finite(drho_dt), is.finite(drho_dd), is.finite(drho_dtau), is.finite(rho)) %>%
  mutate(tau = to_num(tau)) %>%
  group_by(cohort, tau) %>%
  group_modify(~{
    fit <- gam(drho_dtau ~ s(d, t, k = 20) + s(d, k = 10) + s(t, k = 10),
               data = .x, weights = rho)
    grid <- expand_grid(
      d = seq(0, R_MAX, length.out = pred_D),
      t = seq(0, 24,   length.out = pred_T)
    )
    # use partial terms as smooth estimates for v(d) and u(t)
    trm <- predict(fit, newdata = grid, type = "terms")
    tibble(t = grid$t, d = grid$d, v = trm[, "s(d)"], u = trm[, "s(t)"])
  }) %>%
  ungroup() %>%
  mutate(
    speed   = sqrt(v^2 + u^2),
    tau     = to_num(tau),
    YearBin = as_yearbin(tau)
  )

# ---------- 2) Build the raster: panel-wise relative speed (0..1) ----------
vu_rel <- smooth_df %>%
  filter(!is.na(YearBin)) %>%
  group_by(cohort, YearBin) %>%
  mutate(q99 = quantile(speed, 0.99, na.rm = TRUE),
         speed_rel = ifelse(q99 > 0 & is.finite(q99), pmin(speed / q99, 1), 0)) %>%
  ungroup() %>%
  group_by(cohort, YearBin, t, d) %>%
  summarise(speed_rel = mean(speed_rel, na.rm = TRUE), .groups = "drop")

# ---------- 3) Arrow grid (thin + short segments) ----------
# fewer, larger arrows
thin_dt     <- 2.0   # hours between arrows  (↑ = fewer)
thin_dd     <- 6.0   # km between arrows     (↑ = fewer)
arrow_scale <- 0.60  # segment length factor (↑ = longer)


arrows_df <- smooth_df %>%
  dplyr::filter(!is.na(YearBin)) %>%
  dplyr::transmute(
    cohort, YearBin, u, v,
    t0 = round(t / thin_dt) * thin_dt,
    d0 = round(d / thin_dd) * thin_dd
  ) %>%
  dplyr::group_by(cohort, YearBin, t0, d0) %>%
  dplyr::summarise(u = mean(u, na.rm = TRUE),
                   v = mean(v, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    t1 = t0 + arrow_scale * u,
    d1 = d0 + arrow_scale * v,
    dir_deg = (atan2(v, u) * 180/pi + 360) %% 360
  )


# ---------- 4) 6×6 plot ----------
p_dir <- ggplot() +
  geom_raster(data = vu_rel, aes(x = t, y = d, fill = speed_rel)) +
  scale_fill_viridis_c(
    name = "Rel. speed", option = "plasma", begin = 0.15, end = 1,
    limits = c(0, 1), oob = scales::squish
  ) +
  # ⬇️ replace geom_segment(...) with this:
  geom_text(
    data = arrows_df,
    aes(x = t0, y = d0, angle = dir_deg, color = dir_deg),
    label = "\u2192", size = 7, alpha = 0.85, vjust = 0.5, hjust = 0.5
  ) +
  scale_color_gradientn(
    name    = "Direction",
    colours = c("white", "black", "white"),
    values  = scales::rescale(c(0, 180, 360), to = c(0, 1)),
    limits  = c(0, 360)
  ) +
  scale_x_continuous("Hour of Day", breaks = seq(4, 24, 4), limits = c(0, 24)) +
  scale_y_continuous("Radial Distance (km)", breaks = seq(0, R_MAX, 30)) +
  facet_grid(cohort ~ YearBin) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    strip.text.x     = element_text(face = "bold"),
    strip.text.y     = element_text(face = "bold")
  )

print(p_dir)

# 7) Save the plot (optional, adjust size for landscape or full-page portrait)
ggsave(
  filename = "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Generalizing Pseudo Cohorts/mobility_arrows_6x6_FINAL.png",
  plot     = p_dir,
  width    = 16.5,   # Suitable for landscape A3 or large A4
  height   = 11.7,
  units    = "in",
  dpi      = 300,
  device   = "png"
)

# ---------- 5) 2×2 subset (same style) ----------
cohorts_2x2 <- c("1960–1970", "2000–2024")
bins_2x2    <- c("2007–2009", "2022–2024")

vu_rel_2x2 <- vu_rel %>%
  filter(cohort %in% cohorts_2x2, YearBin %in% bins_2x2) %>%
  mutate(
    cohort  = factor(as.character(cohort),  levels = cohorts_2x2),
    YearBin = factor(as.character(YearBin), levels = bins_2x2)
  )

arrows_2x2 <- arrows_df %>%
  filter(cohort %in% cohorts_2x2, YearBin %in% bins_2x2) %>%
  mutate(
    cohort  = factor(as.character(cohort),  levels = cohorts_2x2),
    YearBin = factor(as.character(YearBin), levels = bins_2x2)
  )

p_dir_2x2 <- ggplot() +
  geom_raster(data = vu_rel_2x2, aes(x = t, y = d, fill = speed_rel)) +
  scale_fill_viridis_c(
    name = "Rel. speed", option = "plasma", begin = 0.15, end = 1,
    limits = c(0, 1), oob = scales::squish
  ) +
  # ⬇️ arrow glyphs here too
  geom_text(
    data   = arrows_2x2,
    aes(x = t0, y = d0, angle = dir_deg, color = dir_deg),
    label  = "\u2192",
    family = arrow_font,
    size   = 5.5, alpha = 0.75, vjust = 0.5, hjust = 0.5
  ) +
  scale_color_gradientn(
    name    = "Direction",
    colours = c("white", "black", "white"),
    values  = scales::rescale(c(0, 180, 360), to = c(0, 1)),
    limits  = c(0, 360)
  ) +
  scale_x_continuous("Hour of Day", breaks = seq(4, 24, 4), limits = c(0, 24)) +
  scale_y_continuous("Radial Distance (km)", breaks = seq(0, R_MAX, 30)) +
  facet_grid(cohort ~ YearBin, drop = FALSE) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    strip.text.x     = element_text(face = "bold"),
    strip.text.y     = element_text(face = "bold")
  )

p_dir_2x2

ggsave(
  filename = "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Generalizing Pseudo Cohorts/mobility_arrows_2x2_FINAL.png",
  plot     = p_dir_2x2,
  width    = 16.5,   # Suitable for landscape A3 or large A4
  height   = 11.7,
  units    = "in",
  dpi      = 300,
  device   = "png"
)
