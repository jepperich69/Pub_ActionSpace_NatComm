######################################################################
# sweep_eq7_reference.R -- is fixed-weight Eq. (7) sensitive to the
#                          reference profile?
#
# WHY THIS EXISTS
# ---------------
# sweep_eq7_weighting.R settled two things: the occupancy-threshold route
# fails its stability precondition (the age ordering moves with tau), and
# fixed-weight standardisation cuts the mean 95% interval from 33.2 to 10.8
# percentage points with no cutoff to choose.
#
# But "no cutoff" is not "no choice". Fixed weighting replaces tau with a
# reference occupancy profile, and if the answer moves with that profile we
# have swapped one researcher degree of freedom for another. This script
# measures whether it does.
#
# The variants are the standard index-number ones. If a Laspeyres reference
# (weights from the first period) and a Paasche reference (weights from the
# last) give the same drift, the standardisation is robust in the sense that
# matters: the result is not an artefact of which day-shape was frozen.
#
#   pooled6  the age group's mean of its six normalised period profiles
#            (what sweep_eq7_weighting.R used)
#   base5    the same, over the five baseline periods only -- drops
#            2019-2021, so the pandemic day-shape cannot enter the weights
#   first    the 2007-2009 profile alone                        (Laspeyres)
#   last     the 2022-2024 profile alone                        (Paasche)
#   allages  one profile shared by every age group: the mean over all 30
#            cells. Tests whether the age-specific day shape does any work.
#
# `all` (current Eq. 7) and `marginal` (t integrated out with each period's
# own profile) are carried through as anchors.
#
# WHAT IT WRITES  (results/sensitivity/eq7_reference/)
# --------------
# point_estimates.csv    per age x variant: baseline, endpoint, drift, % drift
# bootstrap_summary.csv  per age x variant: % drift, 95% CI, width, rank
# reference_profiles.csv the five profiles, per age group, per 15-min slice
# spread.csv             per age: max spread in % drift across the references
# reference_report.txt   console transcript
#
# RUN  (from code/)
# ---
#   Rscript code/sweep_eq7_reference.R
######################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(Matrix)
})

source("code/utils_io.R")
source("code/bootstrap_kde.R")
source("code/eq7_weighting_lib.R")

SCENARIO   <- if (exists("scenario")) scenario else "baseline"
B          <- as.integer(Sys.getenv("SWEEP_B", "500"))
SEED       <- 42
AGE_LEVELS <- c("10-17", "18-30", "31-55", "56-65", "66+")
H_T        <- 1
H_D        <- 5

REFS     <- c("pooled6", "base5", "first", "last", "allages")
VARIANTS <- c("all", paste0("fixed:", REFS), "marginal")

OUT_DIR <- file.path("results", "sensitivity", "eq7_reference")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

con <- file(file.path(OUT_DIR, "reference_report.txt"), open = "wt")
say <- function(...) { msg <- paste0(...); cat(msg); cat(msg, file = con) }

say("\n=== Eq. (7) REFERENCE-PROFILE SENSITIVITY ===\n\n")
say("Scenario   : ", SCENARIO, "\n")
say("Replicates : ", B, "\n")
say("References : ", paste(REFS, collapse = ", "), "\n\n")

set.seed(SEED)
dirs <- get_dirs(SCENARIO)

# ============================================================================
# 1. LOAD SESSION GRID AND PREPARE CELLS
# ============================================================================

say("Loading grid_mv_step2.rds...\n")
grid_path <- file.path(dirs$in_dir, "grid_mv_step2.rds")
if (!file.exists(grid_path)) stop("grid_mv_step2.rds not found at: ", grid_path)

grid_mv <- readRDS(grid_path)
say("  loaded ", format(nrow(grid_mv), big.mark = ","), " rows, ",
    format(n_distinct(grid_mv$SessionId), big.mark = ","), " sessions\n")

grid_mv <- grid_mv[, c("SessionId", "AgeGroup", "SessionWeight",
                       "Year", "r_rad_km", "TimeMSM")]
invisible(gc(verbose = FALSE))

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

grid_mv <- grid_mv %>%
  left_join(year_bin_map, by = "Year") %>%
  filter(!is.na(YearBin), !is.na(AgeGroup))

PERIODS  <- sort(unique(grid_mv$YearBin))
FIRST    <- PERIODS[1]
ENDPOINT <- tail(PERIODS, 1)
CELLS    <- expand.grid(AgeGroup = AGE_LEVELS, YearBin = PERIODS,
                        stringsAsFactors = FALSE)

say("Preparing ", nrow(CELLS), " age-period cells...\n")
PREP <- vector("list", nrow(CELLS))
for (k in seq_len(nrow(CELLS))) {
  rows <- grid_mv[grid_mv$AgeGroup == CELLS$AgeGroup[k] &
                  grid_mv$YearBin  == CELLS$YearBin[k], ]
  PREP[[k]] <- bk_prepare(rows, h_t = H_T, h_d = H_D)
}
names(PREP) <- paste(CELLS$AgeGroup, CELLS$YearBin)
rm(grid_mv); invisible(gc(verbose = FALSE))
say("  ", sum(!vapply(PREP, is.null, logical(1))), " of ", length(PREP),
    " cells prepared\n\n")

cell_key <- function(a, p) paste(a, p)

# ============================================================================
# 2. REFERENCE PROFILES
# ============================================================================
# Built inside every pass, so the bootstrap propagates uncertainty in the
# weights as well as in the density.

build_refs <- function(SL) {
  ok_all <- Filter(Negate(is.null), SL)
  shared <- Reduce(`+`, lapply(ok_all, `[[`, "occ")) / length(ok_all)

  out <- list()
  for (a in AGE_LEVELS) {
    sl <- SL[cell_key(a, PERIODS)]
    if (any(vapply(sl, is.null, logical(1)))) next
    occ <- lapply(sl, `[[`, "occ")
    names(occ) <- PERIODS
    base_p <- PERIODS[in_baseline(PERIODS)]
    out[[a]] <- list(
      pooled6 = Reduce(`+`, occ) / length(occ),
      base5   = Reduce(`+`, occ[base_p]) / length(base_p),
      first   = occ[[FIRST]],
      last    = occ[[ENDPOINT]],
      allages = shared
    )
  }
  out
}

# ============================================================================
# 3. ONE PASS
# ============================================================================

one_pass <- function(resample = FALSE) {
  SL <- lapply(PREP, function(P) {
    if (is.null(P)) return(NULL)
    bk_slices(P, if (resample) bk_draw(P$n_sess) else NULL)
  })
  REFP <- build_refs(SL)

  out <- list()
  for (a in names(REFP)) {
    sl <- SL[cell_key(a, PERIODS)]

    vals <- vapply(VARIANTS, function(v) {
      if (v == "all" || v == "marginal") {
        rule <- v; ref <- REFP[[a]]$pooled6
      } else {
        rule <- "fixed"; ref <- REFP[[a]][[sub("^fixed:", "", v)]]
      }
      per <- vapply(sl, function(s) apply_rule(rule, s$m, s$occ, ref), numeric(1))
      names(per) <- PERIODS
      base <- mean(per[in_baseline(PERIODS)], na.rm = TRUE)
      end  <- per[[ENDPOINT]]
      c(base = base, end = end, drift = end - base,
        pct = 100 * (end - base) / base)
    }, numeric(4))

    out[[a]] <- tibble(
      AgeGroup = a, variant = VARIANTS,
      base = vals["base", ], end = vals["end", ],
      drift = vals["drift", ], pct = vals["pct", ]
    )
  }
  bind_rows(out)
}

say("Point estimates...\n")
point <- one_pass(resample = FALSE)
write.csv(point, file.path(OUT_DIR, "point_estimates.csv"), row.names = FALSE)

# The profiles themselves, for inspection and a possible SI panel.
SL0   <- lapply(PREP, function(P) if (is.null(P)) NULL else bk_slices(P, NULL))
REFP0 <- build_refs(SL0)
prof <- bind_rows(lapply(names(REFP0), function(a) {
  bind_rows(lapply(REFS, function(r) tibble(
    AgeGroup = a, reference = r,
    slice = seq_along(REFP0[[a]][[r]]),
    hour  = (seq_along(REFP0[[a]][[r]]) - 1) * 0.25,
    weight = REFP0[[a]][[r]]
  )))
}))
write.csv(prof, file.path(OUT_DIR, "reference_profiles.csv"), row.names = FALSE)

# ============================================================================
# 4. BOOTSTRAP
# ============================================================================

say("Bootstrapping ", B, " replicates ")
t0 <- Sys.time()
reps <- vector("list", B)
for (b in seq_len(B)) {
  reps[[b]] <- one_pass(resample = TRUE) %>%
    select(AgeGroup, variant, pct) %>% mutate(rep = b)
  if (b %% 25 == 0) say(".")
}
say(" done in ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1),
    " min\n\n")

boot <- bind_rows(reps)

summ <- boot %>%
  group_by(AgeGroup, variant) %>%
  summarise(lo = quantile(pct, 0.025, na.rm = TRUE),
            hi = quantile(pct, 0.975, na.rm = TRUE),
            sd = sd(pct, na.rm = TRUE), .groups = "drop") %>%
  inner_join(point %>% select(AgeGroup, variant, pct),
             by = c("AgeGroup", "variant")) %>%
  mutate(width = hi - lo, excludes_zero = (lo > 0) | (hi < 0)) %>%
  group_by(variant) %>% mutate(rank = rank(-pct, ties.method = "min")) %>%
  ungroup() %>%
  select(AgeGroup, variant, pct, lo, hi, width, sd, excludes_zero, rank) %>%
  arrange(match(variant, VARIANTS), match(AgeGroup, AGE_LEVELS))

write.csv(summ, file.path(OUT_DIR, "bootstrap_summary.csv"), row.names = FALSE)

# Spread across the five references, against the bootstrap sd of one of them.
spread <- point %>%
  filter(grepl("^fixed:", variant)) %>%
  group_by(AgeGroup) %>%
  summarise(min_pct = min(pct), max_pct = max(pct),
            spread_pp = max(pct) - min(pct), .groups = "drop") %>%
  inner_join(summ %>% filter(variant == "fixed:pooled6") %>%
               select(AgeGroup, pooled6_pct = pct, boot_sd = sd,
                      ci_width = width),
             by = "AgeGroup") %>%
  mutate(spread_over_sd = spread_pp / boot_sd,
         spread_over_ci = spread_pp / ci_width)
write.csv(spread, file.path(OUT_DIR, "spread.csv"), row.names = FALSE)

# ============================================================================
# 5. REPORT
# ============================================================================

fmt <- function(x, d = 2) formatC(x, format = "f", digits = d, width = 6)

say("--- % DRIFT IN MEAN DISTANCE WHEN AWAY, five-period baseline -> ",
    ENDPOINT, " ---\n\n")
for (v in VARIANTS) {
  s <- summ %>% filter(variant == v) %>% arrange(desc(pct))
  say(sprintf("%-16s\n", v))
  for (i in seq_len(nrow(s)))
    say(sprintf("   %-6s %s%%  [%s, %s]  width %s %s\n",
                s$AgeGroup[i], fmt(s$pct[i]), fmt(s$lo[i]), fmt(s$hi[i]),
                fmt(s$width[i]), ifelse(s$excludes_zero[i], "*", " ")))
  say("   ordering: ", paste(s$AgeGroup, collapse = " > "), "\n\n")
}

say("--- POINT ESTIMATE BY REFERENCE (% drift) ---\n\n")
ptab <- point %>% select(AgeGroup, variant, pct) %>%
  pivot_wider(names_from = variant, values_from = pct)
say(paste(capture.output(print(as.data.frame(ptab), row.names = FALSE)),
          collapse = "\n"), "\n\n")

say("--- SPREAD ACROSS REFERENCES vs SAMPLING NOISE ---\n")
say("    spread_over_sd < 1 means the choice of reference matters less than\n")
say("    one bootstrap standard error.\n\n")
say(paste(capture.output(print(as.data.frame(spread), row.names = FALSE)),
          collapse = "\n"), "\n\n")

say("--- ORDERING BY REFERENCE (rank 1 = largest expansion) ---\n\n")
otab <- summ %>% select(AgeGroup, variant, rank) %>%
  pivot_wider(names_from = variant, values_from = rank)
say(paste(capture.output(print(as.data.frame(otab), row.names = FALSE)),
          collapse = "\n"), "\n\n")

say("--- LASPEYRES vs PAASCHE (first-period vs last-period weights) ---\n\n")
lp <- point %>% filter(variant %in% c("fixed:first", "fixed:last")) %>%
  select(AgeGroup, variant, pct) %>%
  pivot_wider(names_from = variant, values_from = pct) %>%
  mutate(gap_pp = `fixed:last` - `fixed:first`)
say(paste(capture.output(print(as.data.frame(lp), row.names = FALSE)),
          collapse = "\n"), "\n\n")

say("Written to ", OUT_DIR, "\n")
close(con)
