######################################################################
# STEP 11 - DEPARTURE FROM THE PRE-PANDEMIC TREND (Supplementary Table 4)
#
# PURPOSE
# -------
# Answers Reviewer 2's question about the pandemic without replaying the
# drift-vector figure over a shorter window.
#
# The drift vectors compare an endpoint against a period-averaged
# baseline, so they cannot say whether the pandemic bent the path. This
# does: fit the pre-pandemic trend on the four bins ending in 2016-2018,
# extrapolate it to 2019-2021 and 2022-2024, and report how far the
# observed values fall from that extrapolation.
#
# Two quantities are reported so the contrast is visible:
#   fraction of day away from home  - the temporal axis of Figure 6a
#   mean distance when away (km)    - the spatial axis of Figure 6a
#
# The departure is DESCRIPTIVE. A repeated cross-section with a single
# affected period cannot separate a pandemic effect from any other
# contemporaneous change, and nothing here should be read as causal.
#
# Uncertainty comes from the same session-level bootstrap used in step 8:
# each replicate refits the pre-pandemic trend and recomputes the
# departure, so the interval covers both sampling error in the observed
# value and in the extrapolation.
#
# INPUTS
# ------
# results/<SCENARIO>/grid_mv_step2.rds
#
# OUTPUTS
# -------
# results/<SCENARIO>/covid_departure.csv
# results/<SCENARIO>/table_s4_covid_departure.tex
#
# RUN
# ---
# Rscript code/step11_covid_departure.R
######################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(Matrix)
})

source("code/utils_io.R")
source("code/bootstrap_kde.R")
source("code/eq7_weighting_lib.R")

# Bandwidths for the KDE refit; must match step2.
BOOT_H_T <- 1
BOOT_H_D <- 5

cat("\n=== STEP 11: DEPARTURE FROM PRE-PANDEMIC TREND ===\n\n")

SCENARIO    <- if (exists("scenario")) scenario else "baseline"
B           <- 500
SEED        <- 42
HOME_CUTOFF <- 0.02   # km - must match step 2 / step 4 / step 8
AGE_LEVELS  <- c("10-17", "18-30", "31-55", "56-65", "66+")

PERIODS     <- c("2007-2009", "2010-2012", "2013-2015",
                 "2016-2018", "2019-2021", "2022-2024")
PRE_PERIODS <- PERIODS[1:4]   # trend is fitted on these only

set.seed(SEED)
dirs <- get_dirs(SCENARIO)

# Read the grid from the kernel folder, which is where step2 writes it and
# what 00_validate_inputs.R checks. This read used dirs$out_dir (results/),
# contradicting the header above: after step2 was rerun, the kernels updated
# but results/ kept a stale copy, so the bootstrap silently resampled the old
# grid while the point estimates came from the new one.
grid_path <- file.path(dirs$in_dir, "grid_mv_step2.rds")
if (!file.exists(grid_path)) stop("grid_mv_step2.rds not found at: ", grid_path)

grid_mv <- readRDS(grid_path)

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

# Table S4's distance column used to be a per-session mean over away bins,
# averaged across sessions with the survey weights. That is a DIFFERENT
# estimand from the Eq. (7) KDE functional the drift figures report, which was
# the second location of the referee's M4: the pandemic table and Figure 6a
# disagreed about what "mean distance when away" means. It now runs the same
# fixed-weight Eq. (7) through the same code, so the two are comparable.
grid_mv <- grid_mv[, c("SessionId", "AgeGroup", "SessionWeight",
                       "Year", "r_rad_km", "TimeMSM")]
invisible(gc(verbose = FALSE))

grid_mv <- grid_mv %>%
  left_join(year_bin_map, by = "Year") %>%
  filter(!is.na(YearBin), !is.na(AgeGroup))

cat("  Sessions:", format(n_distinct(grid_mv$SessionId), big.mark = ","), "\n\n")

CELLS <- expand.grid(AgeGroup = AGE_LEVELS, YearBin = PERIODS,
                     stringsAsFactors = FALSE)

cat("Preparing", nrow(CELLS), "age-period cells...\n")
PREP <- vector("list", nrow(CELLS))
for (k in seq_len(nrow(CELLS))) {
  rows <- grid_mv[grid_mv$AgeGroup == CELLS$AgeGroup[k] &
                  grid_mv$YearBin  == CELLS$YearBin[k], ]
  PREP[[k]] <- bk_prepare(rows, h_t = BOOT_H_T, h_d = BOOT_H_D)
}
names(PREP) <- paste(CELLS$AgeGroup, CELLS$YearBin)
rm(grid_mv); invisible(gc(verbose = FALSE))
cat("  ", sum(!vapply(PREP, is.null, logical(1))), "of", length(PREP),
    "cells prepared\n\n")

# Cell means for one draw; resample = FALSE gives the point estimate. The
# fixed-weight reference profile is rebuilt from the same draw, exactly as in
# step8. frac_away is scaled to percentage points, the units Table S4 reports.
cell_means <- function(resample = FALSE) {
  SL <- lapply(PREP, function(P) {
    if (is.null(P)) return(NULL)
    bk_slices(P, if (resample) bk_draw(P$n_sess) else NULL)
  })
  eq7_cell_means(SL, AGE_LEVELS, PERIODS) %>%
    mutate(frac_away = 100 * frac_away)
}

# Fit the pre-pandemic trend and read off the two extrapolated departures.
departures <- function(cm) {
  map_dfr(AGE_LEVELS, function(a) {
    d <- cm %>% filter(AgeGroup == a)
    d <- d[match(PERIODS, d$YearBin), ]
    idx <- seq_along(PERIODS)

    one <- function(v, metric) {
      fit  <- lm(v[1:4] ~ idx[1:4])
      pred <- coef(fit)[1] + coef(fit)[2] * idx
      tibble(AgeGroup = a, metric = metric,
             slope_per_period = unname(coef(fit)[2]),
             obs_1921 = v[5], pred_1921 = pred[5], dep_1921 = v[5] - pred[5],
             obs_2224 = v[6], pred_2224 = pred[6], dep_2224 = v[6] - pred[6])
    }
    bind_rows(one(d$frac_away, "frac_away"), one(d$mean_dist, "mean_dist"))
  })
}

point <- departures(cell_means(resample = FALSE))

cat("Running", B, "bootstrap replicates...\n")
boot <- map_dfr(seq_len(B), function(b) {
  if (b %% 50 == 0) cat("    replicate", b, "/", B, "\n")
  departures(cell_means(resample = TRUE)) %>% mutate(replicate = b)
})
cat("  Bootstrap complete\n\n")

ci <- boot %>%
  group_by(AgeGroup, metric) %>%
  summarise(
    slope_lo    = quantile(slope_per_period, 0.025, na.rm = TRUE),
    slope_hi    = quantile(slope_per_period, 0.975, na.rm = TRUE),
    dep_1921_lo = quantile(dep_1921, 0.025, na.rm = TRUE),
    dep_1921_hi = quantile(dep_1921, 0.975, na.rm = TRUE),
    dep_2224_lo = quantile(dep_2224, 0.025, na.rm = TRUE),
    dep_2224_hi = quantile(dep_2224, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

res <- point %>%
  left_join(ci, by = c("AgeGroup", "metric")) %>%
  mutate(sig_slope = (slope_lo > 0) | (slope_hi < 0),
         sig_1921  = (dep_1921_lo > 0) | (dep_1921_hi < 0),
         sig_2224  = (dep_2224_lo > 0) | (dep_2224_hi < 0),
         AgeGroup  = factor(AgeGroup, levels = AGE_LEVELS)) %>%
  arrange(metric, AgeGroup)

write.csv(res, file.path(dirs$out_dir, "covid_departure.csv"), row.names = FALSE)
cat("  covid_departure.csv saved\n")

print(as.data.frame(res %>%
  select(AgeGroup, metric, dep_1921, dep_1921_lo, dep_1921_hi, sig_1921,
         dep_2224, dep_2224_lo, dep_2224_hi, sig_2224) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))), row.names = FALSE)

# ---------------------------------------------------------------- LaTeX
fmt <- function(est, lo, hi, dp) {
  sprintf(paste0("$%+.", dp, "f$ [$%+.", dp, "f$, $%+.", dp, "f$]"), est, lo, hi)
}

blk <- function(metric, dp, label, unit) {
  r <- res %>% filter(metric == !!metric)
  c(sprintf("\\multicolumn{4}{l}{\\textit{%s (%s)}} \\\\", label, unit),
    "\\addlinespace[2pt]",
    vapply(seq_len(nrow(r)), function(i) sprintf(
      "\\quad %s & %s & %s & %s \\\\",
      r$AgeGroup[i],
      fmt(r$slope_per_period[i], r$slope_lo[i], r$slope_hi[i], dp),
      fmt(r$dep_1921[i], r$dep_1921_lo[i], r$dep_1921_hi[i], dp),
      fmt(r$dep_2224[i], r$dep_2224_lo[i], r$dep_2224_hi[i], dp)), character(1)))
}

tex <- c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Age group & Pre-pandemic & \\multicolumn{2}{c}{Departure from extrapolated trend} \\\\",
  "\\cmidrule(lr){3-4}",
  " & trend, per 3-yr period & 2019--2021 & 2022--2024 \\\\",
  "\\midrule",
  blk("frac_away", 2, "Fraction of day away from home", "percentage points"),
  "\\addlinespace[4pt]",
  blk("mean_dist", 2, "Mean distance when away", "km"),
  "\\bottomrule",
  "\\end{tabular}"
)

tex_path <- file.path(dirs$out_dir, "table_s4_covid_departure.tex")
writeLines(tex, tex_path)
cat("  table_s4_covid_departure.tex saved\n\n")
cat("=== STEP 11 COMPLETE ===\n\n")
