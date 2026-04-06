######################################################################
# STEP 2 – ACTION-SPACE KDE & DIFFERENCE DISTRIBUTIONS (FINAL)
# What it does
#  - Builds individual action spaces: distance from home over 24h (15-min grid)
#  - Baseline KDE per cohort (AgeGroup)
#  - Year-bin KDE per cohort (2007–2009, …, 2022–2024)
#  - Difference distributions: (year-bin − baseline) per cohort
#  - Uses empirical time-of-day "home spike" (no uniform smearing)
#  - Strict normalization + (t,d) alignment for comparisons
#  - Saves RDS + CSV + PNG outputs to your output_dir
######################################################################

# ---- Libraries (no readr / lubridate) ----
library(odbc)
library(RODBC)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(data.table)
library(MASS)   # keep last to avoid dplyr::select masking

data.table::setDTthreads(percent = 100)
set.seed(42)

cat("\n=== STEP 2: Action-Space KDE & Differences ===\n")

# -------------------------------------------------------------------
# 0) Parameters & output directory
# -------------------------------------------------------------------
# Parameters can be pre-set by a wrapper script (e.g. step2_run_bw3.R)
# before sourcing this file. If not set, defaults are used.

# Default output directory (Baseline scenario):
if (!exists("output_dir")) {
  output_dir <- "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Pub_ActionSpace_NatComm/Paper Results Baseline"
  # Other scenarios (uncomment as needed, or set in wrapper script):
  # output_dir <- ".../Paper Results Sex1"
  # output_dir <- ".../Paper Results Sex2"
  # output_dir <- ".../Paper Results City 10000"
  # output_dir <- ".../Paper Results City 25000"
  # output_dir <- ".../Paper Results City 50000"
  # output_dir <- ".../Paper Results City 100000"
  # output_dir <- ".../Paper Results BW3"
  # output_dir <- ".../Paper Results BW7"
}

# Ensure directory exists and has a trailing slash
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
if (!grepl("/$", output_dir)) output_dir <- paste0(output_dir, "/")

# KDE grid & bandwidths
R_MAX <- 60        # km (radial distance grid max)
N_T   <- 96        # time bins (24h * 4 = 15-min bins)
N_D   <- 96        # distance bins
H_T   <- 1         # hours bandwidth
if (!exists("H_D")) H_D <- 5  # km bandwidth (away only) — override in wrapper scripts
MAX_PER_GROUP <- 120000   # cap for runtime (weighted subsample if larger)
HOME_ZERO_CUTOFF <- 0.02  # km: <= this is "home" (tight to avoid losing short trips)
dt <- 24 / N_T
dd <- R_MAX / N_D

# Year-bins & cohorts
year_bins <- list(
  `2007–2009` = 2007:2009,
  `2010–2012` = 2010:2012,
  `2013–2015` = 2013:2015,
  `2016–2018` = 2016:2018,
  `2019–2021` = 2019:2021,
  `2022–2024` = 2022:2024
)
year_lookup <- tibble(Year = unlist(year_bins),
                      YearBin = rep(names(year_bins), lengths(year_bins)))
age_groups <- c("10-17", "18-30", "31-55", "56-65", "66+")

# -------------------------------------------------------------------
# 1) Load data (keep 2007+ only)
# -------------------------------------------------------------------
dbname <- "C:/Users/rich/OneDrive - Danmarks Tekniske Universitet/JR/Publikationer/Pub_PopulationVianuey_TBA/TU0624v1.accdb"
con <- RODBC::odbcConnectAccess2007(dbname)

tur <- RODBC::sqlFetch(con, "tur")
session <- RODBC::sqlFetch(con, "session")
household <- RODBC::sqlFetch(con, "household")

cat("Loaded:", nrow(tur), "trips,", nrow(session), "sessions\n")

# -------------------------------------------------------------------
# 2) Clean & enrich; keep weekdays only (>=2007, exclude July)
# -------------------------------------------------------------------
tur_enriched <- tur %>%
  dplyr::select(SessionId, TurId, TurNr,
                PrimMode, PrimModeSumlen,
                PartyNumAdults, PartyNum1017, PartyNumu10,
                DepartMSM, ArrivalMSM, DestDwelTime,
                GISdist, GISdistJourneyStartP, OrigPurp, DestPurp) %>%
  left_join(
    session %>%
      dplyr::select(SessionId, SessionWeight, DiaryYear, RespSex,
                    RespAgeCorrect, HomeAdrCitySize, HousehNumcars, DiaryDaytype, DiaryMonth),
    by = "SessionId"
  ) %>%
  # --- Apply all filters here ---
  filter(
    DiaryYear >= 2007,              # Drop 2006 and earlier
    DiaryDaytype %in% c(11, 12),    # Weekdays only
    DiaryMonth != 7,                 # Exclude July
#    HomeAdrCitySize > 100000,
#    RespSex == 2,
    RespAgeCorrect >= 10                 # <— drop younger than 10
  ) %>%
  # --- Enrich ---
  mutate(
    AgeGroup = case_when(
      RespAgeCorrect <= 17 ~ "10-17",
      RespAgeCorrect <= 30 ~ "18-30",
      RespAgeCorrect <= 55 ~ "31-55",
      RespAgeCorrect <= 65 ~ "56-65",
      RespAgeCorrect >  65 ~ "66+",
      TRUE ~ NA_character_
    ),
    Age  = RespAgeCorrect,
    Year = DiaryYear,
    Cars = HousehNumcars,
    Male = if_else(RespSex == 1, 1, 0),
    City = if_else(HomeAdrCitySize > 10000, 1, 0, missing = 0)
  ) %>%
  dplyr::select(SessionId, TurId, TurNr, DepartMSM, ArrivalMSM, DestDwelTime,
                GISdist, GISdistJourneyStartP,
                AgeGroup, Age, Cars, Male, Year, City, SessionWeight) %>%
  as.data.table()

# --- Remove sessions with missing GIS ---
bad_sessions <- tur_enriched[is.na(GISdist) | is.na(GISdistJourneyStartP), unique(SessionId)]
tur_enriched <- tur_enriched[!SessionId %in% bad_sessions]
cat("Removed", length(bad_sessions), "sessions with missing GIS\n")

tur_enriched <- as_tibble(tur_enriched)

# --- Remove sessions not starting at home (relaxed threshold) ---
bad_sessions_home <- tur_enriched %>%
  group_by(SessionId) %>%
  slice_min(DepartMSM, with_ties = FALSE) %>%
  filter(GISdistJourneyStartP > 5.0) %>%  # >5 km at first departure → likely not at home
  pull(SessionId) %>% unique()

tur_enriched <- tur_enriched %>% filter(!SessionId %in% bad_sessions_home)
cat("Removed", length(bad_sessions_home), "sessions not starting at home\n")

# -------------------------------------------------------------------
# 3) 15-min grid + zero-trip days → r_rad_km (radial distance) & d_cum_km
# -------------------------------------------------------------------

# CRITICAL FIX: Track which sessions ORIGINALLY had trips (before filtering)
original_trippers <- tur %>% 
  filter(!is.na(TurId)) %>% 
  distinct(SessionId)

trippers <- tur_enriched %>% distinct(SessionId)

# FIXED: Only include sessions that NEVER had trips originally
# (not sessions whose trips were removed by filtering)
zero_trip_obs <- session %>%
  filter(!(SessionId %in% original_trippers$SessionId),  # Never had trips originally
         DiaryYear >= 2007, 
         DiaryDaytype %in% c(11, 12), 
         DiaryMonth != 7,
         #RespSex == 2,
         HomeAdrCitySize > 100000,
         RespAgeCorrect >= 10) %>%
  mutate(
    Year = DiaryYear,
    Age  = RespAgeCorrect,
    AgeGroup = case_when(
      RespAgeCorrect <= 17 ~ "10-17",
      RespAgeCorrect <= 30 ~ "18-30",
      RespAgeCorrect <= 55 ~ "31-55",
      RespAgeCorrect <= 65 ~ "56-65",
      RespAgeCorrect >  65 ~ "66+",
      TRUE ~ NA_character_
    ),
    Male = if_else(RespSex == 1, 1, 0),
    City = if_else(HomeAdrCitySize > 10000, 1, 0, missing = 0),
    Cars = HousehNumcars,
    SessionWeight = SessionWeight,
    TurId = NA_integer_, TurNr = NA_integer_,
    DepartMSM = NA_real_, ArrivalMSM = NA_real_, DestDwelTime = NA_real_,
    GISdist = 0, GISdistJourneyStartP = 0
  ) %>%
  dplyr::select(SessionId, TurId, TurNr, DepartMSM, ArrivalMSM, DestDwelTime,
                GISdist, GISdistJourneyStartP, AgeGroup, Age,
                Cars, Male, Year, City, SessionWeight)

tur_enriched <- bind_rows(tur_enriched, zero_trip_obs) %>% arrange(SessionId, TurId, TurNr)

bins <- tibble(TimeMSM = seq(0, 1439, by = 15))  # 96 bins of 15 min
sessions_static <- tur_enriched %>% dplyr::select(SessionId, AgeGroup, Age, Cars, City, SessionWeight, Male, Year) %>% distinct()
tur_moving <- tur_enriched %>% filter(!is.na(DepartMSM), !is.na(ArrivalMSM), ArrivalMSM > DepartMSM)

# COMBINED PROCESSING: Calculate both departure and arrival locations together
# This ensures departures use the correct "where am I now" location
trips_with_locations <- tur_moving %>%
  arrange(SessionId, TurNr) %>%
  group_by(SessionId) %>%
  mutate(
    # Where will I be AFTER this trip? (arrival location)
    dest_dist = GISdistJourneyStartP,
    
    # For the LAST trip: if first trip was from home, assume return to home
    first_trip_from_home = first(GISdistJourneyStartP) < 0.1,
    is_last_trip = (TurNr == max(TurNr)),
    arrival_dist = if_else(is_last_trip & first_trip_from_home, 0, dest_dist),
    
    # Where am I NOW when departing? (use previous arrival location, or 0 for first trip)
    depart_from_dist = lag(arrival_dist, default = 0)
  ) %>%
  ungroup()

# Departures - where person IS before leaving
trips_dep <- trips_with_locations %>%
  mutate(depart_bin = pmin(ceiling(DepartMSM/15)*15, 1439L)) %>%
  group_by(SessionId, depart_bin) %>%
  slice_max(DepartMSM, n = 1, with_ties = FALSE) %>% 
  ungroup() %>%
  transmute(SessionId, TimeBin = depart_bin, r_val = depart_from_dist)

# Arrivals - where person ENDS UP after arriving
trips_arr_with_dist <- trips_with_locations %>%
  mutate(arrival_bin = pmin(ceiling(ArrivalMSM/15)*15, 1439L)) %>%
  group_by(SessionId, arrival_bin) %>%
  slice_max(ArrivalMSM, n = 1, with_ties = FALSE) %>% 
  ungroup() %>%
  transmute(SessionId, TimeBin = arrival_bin, r_val = arrival_dist)

# Cumulative distance increments
trips_arr_cumulative <- tur_moving %>%
  mutate(arrival_bin = pmin(ceiling(ArrivalMSM/15)*15, 1439L)) %>%
  group_by(SessionId, arrival_bin) %>%
  summarise(d_inc = sum(GISdist), .groups = "drop")

# Build full time grid with forward-filled r and cumulative distance
all_location_events <- bind_rows(trips_dep, trips_arr_with_dist) %>% arrange(SessionId, TimeBin)

grid_mv <- sessions_static %>%
  crossing(bins) %>%
  rename(TimeBin = TimeMSM) %>%
  arrange(SessionId, TimeBin) %>%
  left_join(all_location_events, by = c("SessionId","TimeBin")) %>%
  group_by(SessionId) %>%
  arrange(TimeBin) %>%
  tidyr::fill(r_val, .direction = "down") %>%
  mutate(
    r_rad_km = replace_na(r_val, 0),
    r_rad_km = if_else(r_rad_km < HOME_ZERO_CUTOFF, 0, r_rad_km)
  ) %>%
  dplyr::select(-r_val) %>% ungroup() %>%
  left_join(trips_arr_cumulative, by = c("SessionId","TimeBin" = "arrival_bin")) %>%
  mutate(d_inc = replace_na(d_inc, 0)) %>%
  group_by(SessionId) %>%
  arrange(TimeBin) %>%
  mutate(d_cum_km = cumsum(d_inc)) %>%
  dplyr::select(-d_inc) %>%
  ungroup() %>%
  rename(TimeMSM = TimeBin)

#############



#############

# (Optional) active window flags
session_range <- tur_moving %>%
  mutate(depart_bin = pmin(ceiling(DepartMSM/15)*15, 1439L),
         arrival_bin = pmin(ceiling(ArrivalMSM/15)*15, 1439L)) %>%
  group_by(SessionId) %>%
  summarise(first_bin = min(depart_bin), last_bin = max(arrival_bin), .groups = "drop")
grid_mv <- grid_mv %>%
  left_join(session_range, by = "SessionId") %>%
  mutate(active = as.integer(TimeMSM >= first_bin & TimeMSM <= last_bin),
         active = if_else(is.na(active), 0L, active)) %>%
  dplyr::select(-first_bin, -last_bin) %>%
  arrange(SessionId, TimeMSM)

cat("Grid created:", nrow(grid_mv), "rows\n")

# -------------------------------------------------------------------
# 4) Analysis frame + sessions with trips (for KDE)
# -------------------------------------------------------------------
analysis_data <- grid_mv %>%
  left_join(year_lookup, by = "Year") %>%
  filter(!is.na(YearBin), !is.na(AgeGroup)) %>%
  mutate(
    AgeGroup = factor(AgeGroup, levels = age_groups),
    YearBin  = factor(YearBin, levels = names(year_bins))
  ) %>%
  dplyr::select(SessionId, TimeMSM, r_rad_km, SessionWeight, AgeGroup, YearBin)

sessions_with_trips <- analysis_data %>%
  group_by(SessionId) %>% summarise(has_trips = any(r_rad_km > HOME_ZERO_CUTOFF), .groups="drop") %>%
  filter(has_trips) %>% pull(SessionId)

kde_input <- analysis_data %>%
  dplyr::select(SessionId, TimeMSM, r_rad_km, SessionWeight, AgeGroup, YearBin)

# ---- RAW GRID sanity check (compare to Step 1) ----
raw_hours_away <- grid_mv %>%
  left_join(year_lookup, by = "Year") %>%
  filter(!is.na(YearBin), !is.na(AgeGroup)) %>%
  group_by(AgeGroup, YearBin) %>%
  summarise(
    mean_hours_away_grid = mean(r_rad_km > HOME_ZERO_CUTOFF) * 24,
    share_no_travel_grid = mean(all(r_rad_km <= HOME_ZERO_CUTOFF)),
    .groups = "drop"
  )
cat("\n=== RAW GRID CHECK (before KDE) ===\n"); print(raw_hours_away, n = Inf)

# -------------------------------------------------------------------
# 5) KDE function with empirical home-time spike + normalization
# -------------------------------------------------------------------
estimate_kde <- function(data, n_t = N_T, n_d = N_D, r_max = R_MAX,
                         h_t = H_T, h_d = H_D, max_n = MAX_PER_GROUP,
                         home_zero = HOME_ZERO_CUTOFF) {
  
  data <- data %>%
    dplyr::select(TimeMSM, r_rad_km, SessionWeight) %>%
    tidyr::drop_na(TimeMSM, r_rad_km) %>%
    mutate(
      SessionWeight = as.numeric(replace_na(SessionWeight, 0)),
      SessionWeight = if_else(!is.finite(SessionWeight) | SessionWeight < 0, 0, SessionWeight)
    )
  
  if (nrow(data) < 200 || sum(data$SessionWeight) <= 0) {
    warning("Too few observations/weight for KDE")
    return(tibble(t = numeric(0), d = numeric(0), density = numeric(0)))
  }
  
  total_weight <- sum(data$SessionWeight)
  home_weight  <- sum(data$SessionWeight[data$r_rad_km <= home_zero])
  fraction_home <- pmax(pmin(home_weight / total_weight, 1), 0)
  
  data_away <- data %>% filter(r_rad_km > home_zero)
  n_away <- nrow(data_away)
  
  # Grid
  grid_t <- seq(0, 24, length.out = n_t)
  grid_d <- seq(0, r_max, length.out = n_d)
  density_away <- matrix(0, nrow = n_t, ncol = n_d)
  
  if (n_away >= 50) {
    if (n_away > max_n) {
      # Clean weights before sampling
      valid_weights <- !is.na(data_away$SessionWeight) & 
        is.finite(data_away$SessionWeight) & 
        data_away$SessionWeight > 0
      
      n_valid <- sum(valid_weights)  # <-- Calculate count first
      
      if (n_valid > 0 && sum(data_away$SessionWeight[valid_weights]) > 0) {
        data_away <- data_away %>%
          filter(valid_weights) %>%
          dplyr::slice_sample(n = min(max_n, n_valid), weight_by = SessionWeight)
      } else {
        data_away <- dplyr::slice_sample(data_away, n = max_n)
      }
    }
    
    kde_away <- with(data_away, MASS::kde2d(
      x = TimeMSM / 60, y = r_rad_km,
      n = c(n_t, n_d),
      lims = c(0, 24, 0, r_max),
      h = c(h_t, h_d)
    ))
    grid_t <- kde_away$x
    grid_d <- kde_away$y
    density_away <- kde_away$z
  }
  # Normalize away to unit mass on grid
  s_away <- sum(density_away) * dt * dd
  if (s_away > 0) density_away <- density_away / s_away
  
  # Mix with home mass
  d_idx_zero <- which.min(abs(grid_d - 0))
  density_combined <- density_away * (1 - fraction_home)
  
  # Empirical time-of-day home profile (replaces uniform spike)
  home_profile <- data %>%
    mutate(hour = floor(TimeMSM / 60)) %>%
    group_by(hour) %>%
    summarise(w_home = sum(SessionWeight[r_rad_km <= home_zero]), .groups = "drop")
  
  if (sum(home_profile$w_home) > 0) {
    home_profile <- home_profile %>% mutate(p_home = w_home / sum(w_home))
  } else {
    home_profile <- tibble(hour = 0:23, p_home = 1/24)
  }
  
  home_density_time <- approx(home_profile$hour, home_profile$p_home, xout = grid_t, rule = 2)$y
  home_density_time <- home_density_time / sum(home_density_time * dt)
  
  for (i in seq_along(grid_t)) {
    density_combined[i, d_idx_zero] <- density_combined[i, d_idx_zero] +
      (fraction_home * home_density_time[i]) / dd
  }
  
  # Final renorm
  s_total <- sum(density_combined) * dt * dd
  if (s_total > 0) density_combined <- density_combined / s_total
  
  tibble::tibble(
    t = rep(grid_t, times = length(grid_d)),
    d = rep(grid_d, each  = length(grid_t)),
    density = as.vector(density_combined)
  )
}

# -------------------------------------------------------------------
# 6) Baseline KDE per cohort & Year-bin KDE per cohort
# -------------------------------------------------------------------
cat("\nEstimating baseline KDEs...\n")
baseline_kde <- kde_input %>%
  group_by(AgeGroup) %>%
  group_modify(~{
    cat("  ", as.character(.y$AgeGroup), "N=", nrow(.x), "\n")
    out <- estimate_kde(.x)
    if (nrow(out) == 0) out <- tibble(t = numeric(0), d = numeric(0), density = numeric(0))
    out
  }) %>% ungroup() %>% arrange(AgeGroup, t, d)

cat("\nEstimating year-bin KDEs...\n")
yearbin_kde <- kde_input %>%
  group_by(AgeGroup, YearBin) %>%
  group_modify(~{
    cat("  ", as.character(.y$AgeGroup), "-", as.character(.y$YearBin),
        " N=", nrow(.x), "\n")
    out <- estimate_kde(.x)
    if (nrow(out) == 0) out <- tibble(t = numeric(0), d = numeric(0), density = numeric(0))
    out
  }) %>% ungroup() %>% arrange(AgeGroup, YearBin, t, d)

# -------------------------------------------------------------------
# 7) Mass audits (must be ~1.000). Uses SAME cutoff as grid/KDE.
# -------------------------------------------------------------------
CUTOFF <- HOME_ZERO_CUTOFF

audit_baseline <- baseline_kde %>%
  group_by(AgeGroup) %>%
  summarise(
    total_mass      = sum(density) * dt * dd,
    share_away_time = sum(density[d > CUTOFF]) * dt * dd,
    share_home_time = 1 - share_away_time,
    mean_dist       = sum(d * density) * dt * dd,
    .groups = "drop"
  )
cat("\n=== BASELINE MASS AUDIT ===\n"); print(audit_baseline, n = Inf)

audit_yearbin <- yearbin_kde %>%
  group_by(AgeGroup, YearBin) %>%
  summarise(
    total_mass      = sum(density) * dt * dd,
    share_away_time = sum(density[d > CUTOFF]) * dt * dd,
    share_home_time = 1 - share_away_time,
    mean_dist       = sum(d * density) * dt * dd,
    .groups = "drop"
  ) %>% arrange(AgeGroup, YearBin)
cat("\n=== YEAR-BIN MASS AUDIT ===\n"); print(audit_yearbin, n = Inf)

# -------------------------------------------------------------------
# 8) Difference distributions: (year-bin − baseline) per cohort
# -------------------------------------------------------------------
diff_kde <- yearbin_kde %>%
  inner_join(
    baseline_kde %>% rename(density_baseline = density),
    by = c("AgeGroup","t","d")
  ) %>%
  mutate(density_diff = density - density_baseline) %>%
  dplyr::select(AgeGroup, YearBin, t, d, density_diff) %>%
  arrange(AgeGroup, YearBin, t, d)

cat("\nWriting outputs to: ", output_dir, "\n")

# -------------------------------------------------------------------
# 9) Save core outputs (RDS + CSV via base write.csv)
# -------------------------------------------------------------------
saveRDS(baseline_kde, file = paste0(output_dir, "kde_baseline_by_cohort.rds"))
saveRDS(yearbin_kde,  file = paste0(output_dir, "kde_yearbin_by_cohort.rds"))
saveRDS(diff_kde,     file = paste0(output_dir, "kde_difference_by_cohort.rds"))
saveRDS(grid_mv,      file = paste0(output_dir, "grid_mv_step2.rds"))

write.csv(audit_baseline, file = paste0(output_dir, "audit_baseline.csv"), row.names = FALSE)
write.csv(audit_yearbin,  file = paste0(output_dir, "audit_yearbin.csv"),  row.names = FALSE)
write.csv(raw_hours_away, file = paste0(output_dir, "raw_grid_check.csv"),  row.names = FALSE)

cat("\n✓ Saved KDEs & audits to:\n  ", output_dir, "\n")

# -------------------------------------------------------------------
# 10) Plots — auto-exposed & readable
# -------------------------------------------------------------------
library(scales)
library(dplyr)
library(ggplot2)
library(viridisLite)

# Ensure we have probability-per-cell columns
baseline_kde <- baseline_kde %>% mutate(mass = density * dt * dd)
yearbin_kde  <- yearbin_kde  %>% mutate(mass = density * dt * dd)
diff_kde     <- diff_kde     %>% mutate(mass_diff = density_diff * dt * dd)

# ---------- Diagnostic: print quantiles so we know the range ----------
q_print <- function(x, name) {
  qs <- quantile(x, c(0, .5, .9, .95, .99, .995, .999, 1), na.rm = TRUE)
  cat("\n", name, "quantiles (as % of day per cell):\n")
  print(round(100*qs, 4))
}
q_print(baseline_kde$mass, "Baseline mass")
q_print(yearbin_kde$mass,  "Yearbin mass")
q_print(abs(diff_kde$mass_diff), "Abs diff mass")

# Helper to cap values to robust limits
cap_val <- function(x, lo = 0, hi = 0.999) {
  qs <- quantile(x, probs = c(lo, hi), na.rm = TRUE)
  list(lo = as.numeric(qs[1]), hi = as.numeric(qs[2]))
}

# ==============================
# A) GLOBAL, COMPARABLE SCALES
# ==============================
# Use aggressive upper cap @ 99.9% to brighten mid-tones
caps_base <- cap_val(baseline_kde$mass, hi = 0.999)     # baseline
caps_year <- cap_val(yearbin_kde$mass,  hi = 0.999)     # yearbin
caps_diff <- cap_val(abs(diff_kde$mass_diff), hi = 0.999)
L_global  <- caps_diff$hi

# --- Baseline (GLOBAL) ---
p_kde_baseline_GLOBAL <- ggplot(baseline_kde, aes(t, d, fill = mass)) +
  geom_raster() +
  facet_wrap(~ AgeGroup, ncol = 3) +
  scale_fill_viridis_c(option = "plasma",
                       limits = c(0, caps_base$hi),
                       oob = squish,
                       trans = "sqrt",
                       labels = percent_format(accuracy = 0.01),
                       name = "Prob. per cell") +
  labs(title = "Action-Space Probability (Baseline, Global Exposure)",
       x = "Hour of day", y = "Distance from home (km)") +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA)) +
  coord_cartesian(expand = FALSE)

ggsave(paste0(output_dir, "kde_baseline_by_cohort_PROB_GLOBAL.png"),
       p_kde_baseline_GLOBAL, width = 12, height = 8, dpi = 300)

# --- Year-bin (GLOBAL) ---
p_kde_yearbin_GLOBAL <- ggplot(yearbin_kde, aes(t, d, fill = mass)) +
  geom_raster() +
  facet_grid(AgeGroup ~ YearBin) +
  scale_fill_viridis_c(option = "plasma",
                       limits = c(0, caps_year$hi),
                       oob = squish,
                       trans = "sqrt",
                       labels = percent_format(accuracy = 0.01),
                       name = "Prob. per cell") +
  labs(title = "Action-Space Probability by Cohort & Year-bin (Global Exposure)",
       x = "Hour of day", y = "Distance from home (km)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 9),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA)) +
  coord_cartesian(expand = FALSE)

ggsave(paste0(output_dir, "kde_yearbin_by_cohort_PROB_GLOBAL.png"),
       p_kde_yearbin_GLOBAL, width = 14, height = 10, dpi = 300)

# --- Difference (GLOBAL, symmetric) ---
p_kde_diff_GLOBAL <- ggplot(diff_kde, aes(t, d, fill = mass_diff)) +
  geom_raster() +
  facet_grid(AgeGroup ~ YearBin) +
  scale_fill_gradient2(low = "#2c7bb6", mid = "white", high = "#d7191c",
                       limits = c(-L_global, L_global),
                       oob = squish,
                       labels = function(x) percent(x, accuracy = 0.001),
                       name = "\u0394 Prob. per cell") +
  labs(title = "Difference (Year-bin − Baseline): Prob. per cell (Global Exposure)",
       x = "Hour of day", y = "Distance from home (km)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 9),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA)) +
  coord_cartesian(expand = FALSE)

ggsave(paste0(output_dir, "kde_difference_by_cohort_PROB_GLOBAL.png"),
       p_kde_diff_GLOBAL, width = 14, height = 10, dpi = 300)

# ============================================
# B) AUTO-EXPOSED (PER-FACET) — SUPER BRIGHT
# ============================================
# NOTE: NOT comparable across facets; tuned for readability.

# per-cohort exposure for baseline
baseline_exposed <- baseline_kde %>%
  group_by(AgeGroup) %>%
  mutate(cap = quantile(mass, 0.999, na.rm = TRUE),
         mass_vis = pmin(mass, cap)) %>%
  ungroup()

p_kde_baseline_EXPOSED <- ggplot(baseline_exposed, aes(t, d, fill = mass_vis)) +
  geom_raster() +
  facet_wrap(~ AgeGroup, ncol = 3) +
  scale_fill_viridis_c(option = "inferno",
                       trans = "sqrt",
                       labels = percent_format(accuracy = 0.01),
                       name = "Prob. per cell\n(per facet cap)") +
  labs(title = "Action-Space Probability (Baseline, Auto-Exposed per Cohort)",
       x = "Hour of day", y = "Distance from home (km)") +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA)) +
  coord_cartesian(expand = FALSE)

ggsave(paste0(output_dir, "kde_baseline_by_cohort_PROB_EXPOSED.png"),
       p_kde_baseline_EXPOSED, width = 12, height = 8, dpi = 300)

# per (AgeGroup, YearBin) exposure for year-bin & difference
yearbin_exposed <- yearbin_kde %>%
  group_by(AgeGroup, YearBin) %>%
  mutate(cap = quantile(mass, 0.999, na.rm = TRUE),
         mass_vis = pmin(mass, cap)) %>%
  ungroup()

p_kde_yearbin_EXPOSED <- ggplot(yearbin_exposed, aes(t, d, fill = mass_vis)) +
  geom_raster() +
  facet_grid(AgeGroup ~ YearBin) +
  scale_fill_viridis_c(option = "inferno",
                       trans = "sqrt",
                       labels = percent_format(accuracy = 0.01),
                       name = "Prob. per cell\n(per facet cap)") +
  labs(title = "Action-Space Probability by Cohort & Year-bin (Auto-Exposed)",
       x = "Hour of day", y = "Distance from home (km)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 9),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA)) +
  coord_cartesian(expand = FALSE)

ggsave(paste0(output_dir, "kde_yearbin_by_cohort_PROB_EXPOSED.png"),
       p_kde_yearbin_EXPOSED, width = 14, height = 10, dpi = 300)

diff_exposed <- diff_kde %>%
  group_by(AgeGroup, YearBin) %>%
  mutate(cap = quantile(abs(mass_diff), 0.999, na.rm = TRUE),
         md_vis = pmax(pmin(mass_diff, cap), -cap)) %>%
  ungroup()

p_kde_diff_EXPOSED <- ggplot(diff_exposed, aes(t, d, fill = md_vis)) +
  geom_raster() +
  facet_grid(AgeGroup ~ YearBin) +
  scale_fill_gradient2(low = "#2c7bb6", mid = "white", high = "#d7191c",
                       labels = function(x) percent(x, accuracy = 0.001),
                       name = "\u0394 Prob. per cell\n(per facet cap)") +
  labs(title = "Difference (Year-bin − Baseline): Auto-Exposed per Facet",
       x = "Hour of day", y = "Distance from home (km)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold", size = 9),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA)) +
  coord_cartesian(expand = FALSE)

ggsave(paste0(output_dir, "kde_difference_by_cohort_PROB_EXPOSED.png"),
       p_kde_diff_EXPOSED, width = 14, height = 10, dpi = 300)

cat("\n================================================================================\n")
cat("✓ STEP 2 COMPLETE\n")
cat("  - RDS: kde_baseline_by_cohort.rds, kde_yearbin_by_cohort.rds, kde_difference_by_cohort.rds, grid_mv_step2.rds\n")
cat("  - CSV: audit_baseline.csv, audit_yearbin.csv, raw_grid_check.csv\n")
cat("  - PNG: kde_baseline_by_cohort.png, kde_yearbin_by_cohort.png, kde_difference_by_cohort.png\n")
cat("================================================================================\n\n")

# ============================================================================
# CREATE TEMPORAL PROFILES FOR STEP 3
# ============================================================================

cat("\n=== Creating temporal profiles for Step 3 ===\n")

HOME_CUTOFF_TEMPORAL <- 0.5  # km - for "away" analysis

# --- Check column names in yearbin_kde only (baseline doesn't have year bins!) ---
cat("  Checking column names...\n")
cat("    Baseline columns: ", paste(names(baseline_kde), collapse=", "), "\n")
cat("    Year-bin columns: ", paste(names(yearbin_kde), collapse=", "), "\n")

# Standardize year-bin column name
if ("year_bin" %in% names(yearbin_kde)) {
  yearbin_kde <- yearbin_kde %>% rename(YearBin = year_bin)
  cat("    Renamed 'year_bin' to 'YearBin' in yearbin_kde\n")
} else if ("YearBin" %in% names(yearbin_kde)) {
  cat("    Using 'YearBin' column in yearbin_kde\n")
} else {
  stop("Error: Cannot find year_bin or YearBin column in yearbin_kde!")
}

# --- Baseline temporal marginals (away-time only) ---
cat("  Computing baseline temporal marginals...\n")
temporal_baseline_away <- baseline_kde %>%
  filter(d > HOME_CUTOFF_TEMPORAL) %>%
  group_by(AgeGroup, t) %>%
  summarise(
    density_t = sum(density) * dd,  # integrate over distance
    .groups = "drop"
  ) %>%
  # Normalize to sum to 1 over time
  group_by(AgeGroup) %>%
  mutate(
    total_mass = sum(density_t * dt),
    density_t = if_else(total_mass > 0, density_t / sum(density_t * dt), 0)
  ) %>%
  ungroup() %>%
  dplyr::select(AgeGroup, t, density_t)

cat("    Baseline: ", nrow(temporal_baseline_away), " rows, ", 
    length(unique(temporal_baseline_away$AgeGroup)), " age groups\n")

# --- Year-bin temporal marginals (away-time only) ---
cat("  Computing year-bin temporal marginals...\n")
temporal_yearbin_away <- yearbin_kde %>%
  filter(d > HOME_CUTOFF_TEMPORAL) %>%
  group_by(AgeGroup, YearBin, t) %>%
  summarise(
    density_t = sum(density) * dd,
    .groups = "drop"
  ) %>%
  group_by(AgeGroup, YearBin) %>%
  mutate(
    total_mass = sum(density_t * dt),
    density_t = if_else(total_mass > 0, density_t / sum(density_t * dt), 0)
  ) %>%
  ungroup() %>%
  dplyr::select(AgeGroup, YearBin, t, density_t)

cat("    Year-bin: ", nrow(temporal_yearbin_away), " rows, ", 
    length(unique(temporal_yearbin_away$AgeGroup)), " age groups, ",
    length(unique(temporal_yearbin_away$YearBin)), " year bins\n")

# --- Apply temporal smoothing ---
cat("  Applying temporal smoothing...\n")
smooth_temporal <- function(df, sigma_hours = 0.5) {
  sigma_bins <- sigma_hours / dt
  win <- ceiling(6 * sigma_bins)
  if (win %% 2 == 0) win <- win + 1
  kern_x <- seq(-3 * sigma_bins, 3 * sigma_bins, length.out = win)
  kern <- dnorm(kern_x, mean = 0, sd = sigma_bins)
  kern <- kern / sum(kern)
  
  # NOTE: This function receives data WITHOUT the grouping variables
  df %>%
    arrange(t) %>%
    mutate(
      density_smooth = as.numeric(stats::filter(density_t, kern, sides = 2))
    )
}

temporal_baseline_away <- temporal_baseline_away %>%
  group_by(AgeGroup) %>%
  group_modify(~ smooth_temporal(.x)) %>%
  ungroup()

cat("    ✓ Baseline smoothed\n")

temporal_yearbin_away <- temporal_yearbin_away %>%
  group_by(AgeGroup, YearBin) %>%
  group_modify(~ smooth_temporal(.x)) %>%
  ungroup()

cat("    ✓ Year-bin smoothed\n")

# --- Save temporal profiles ---
cat("  Saving temporal profiles...\n")
saveRDS(temporal_baseline_away, paste0(output_dir, "temporal_baseline_away.rds"))
saveRDS(temporal_yearbin_away, paste0(output_dir, "temporal_yearbin_away.rds"))

cat("  ✓ temporal_baseline_away.rds\n")
cat("  ✓ temporal_yearbin_away.rds\n\n")
cat("  These files are now ready for Step 3!\n\n")

############## DEBUG #############

# 1. Check raw_grid_check - should now show ~6-8 hours
raw_grid_check <- read.csv(paste0(output_dir, "raw_grid_check.csv"))
print(raw_grid_check[1:10,])

# 2. Check session 435086 - should now show ~6.5 hours away
check_435086 <- grid_mv %>%
  filter(SessionId == 435086, TimeMSM >= 450, TimeMSM <= 900) %>%
  dplyr::select(SessionId, TimeMSM, r_rad_km, d_cum_km) %>%
  mutate(is_away = as.integer(r_rad_km > 0.02))

cat("\nSession 435086 time away:\n")
cat("  Total away minutes:", sum(check_435086$is_away) * 15, "\n")
cat("  Total away hours:", sum(check_435086$is_away) * 15 / 60, "\n")

print(check_435086, n = 31)

# 3. Check overall unweighted averages
overall_check <- grid_mv %>%
  group_by(SessionId, AgeGroup) %>%
  summarise(hrs_away = sum(r_rad_km > 0.02) * 15 / 60, .groups = "drop") %>%
  group_by(AgeGroup) %>%
  summarise(avg_hrs_away = mean(hrs_away))

cat("\nOverall average hours away by age group:\n")
print(overall_check)


############


# Check raw_grid_check to see if it improved
raw_grid_check <- read.csv(paste0(output_dir, "raw_grid_check.csv"))
cat("\nRaw grid hours away (should match overall_check):\n")
print(raw_grid_check[raw_grid_check$YearBin == "2007–2009",])