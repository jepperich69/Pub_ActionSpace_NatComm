######################################################################
# sweep_eq7_weighting.R -- how Eq. (7) should weight time slices
#
# WHY THIS EXISTS
# ---------------
# Eq. (7) averages the per-slice conditional mean distance over "occupied"
# slices. The code implements occupancy as "any away mass", and after
# smoothing that is every slice, so 03:00 -- where almost nobody is out --
# counts as heavily as noon. Bootstrap CV of the slice mean is 49% over
# 00-05h against 2.2% over 07-19h, which is why the distance axis of
# Figure 6a is unresolved while the away axis is tight.
#
# Two families of repair are on the table, and this script measures both on
# an identical footing:
#
#   (a) OCCUPANCY THRESHOLD -- keep equal weights, but only over slices
#       carrying at least tau of the day's peak away mass. Crude, and tau is
#       a researcher degree of freedom that moves the age ordering, so it
#       must be swept and disclosed.
#
#   (b) FIXED-WEIGHT STANDARDISATION -- weight slices by an occupancy
#       profile, but hold that profile FIXED across periods at a common
#       age-group reference. Empty hours get near-zero weight (the precision
#       of an integrated-out marginal) while the weights cannot move between
#       baseline and endpoint (so a change in WHEN people are out cannot leak
#       into the distance axis). No cutoff to choose.
#
# The naive alternative -- integrating t out with each period's OWN occupancy
# profile -- is computed too, as MARGINAL. It is not a candidate estimator:
# because r and t are strongly dependent, its drift mixes a change in
# distance with a change in timing. It is here to size that leak, since
#
#   drift(MARGINAL) = drift(FIXED) + compositional term
#
# and the compositional term is itself reportable.
#
# WHAT IT WRITES  (results/sensitivity/eq7_weighting/)
# --------------
# point_estimates.csv   per age x rule: baseline, endpoint, drift, % drift
# bootstrap_summary.csv per age x rule: % drift, 95% CI, width, ordering rank
# slice_weights.csv     the reference profile and what each rule keeps
# sweep_report.txt      console transcript
#
# RUN  (from code/)
# ---
#   Rscript code/sweep_eq7_weighting.R
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

# Occupancy thresholds, as a fraction of the day's PEAK slice away mass.
TAUS <- c(0.01, 0.02, 0.05, 0.10, 0.20)

OUT_DIR <- file.path("results", "sensitivity", "eq7_weighting")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

con <- file(file.path(OUT_DIR, "sweep_report.txt"), open = "wt")
say <- function(...) { msg <- paste0(...); cat(msg); cat(msg, file = con) }

say("\n=== Eq. (7) SLICE-WEIGHTING SWEEP ===\n\n")
say("Scenario   : ", SCENARIO, "\n")
say("Replicates : ", B, "\n")
say("Thresholds : ", paste0(TAUS * 100, "%", collapse = ", "), "\n\n")

set.seed(SEED)
dirs <- get_dirs(SCENARIO)

# ============================================================================
# 1-2. SLICE EVALUATION AND WEIGHTING RULES
# ============================================================================
# bk_slices(), apply_rule() and rule_n_eff() live in eq7_weighting_lib.R so
# that this sweep and the reference-profile sweep share one definition.

RULES <- c("all", paste0("thr", sprintf("%02d", TAUS * 100)), "fixed", "marginal")

# ============================================================================
# 3. LOAD SESSION GRID AND PREPARE CELLS
# ============================================================================

say("Loading grid_mv_step2.rds...\n")
grid_path <- file.path(dirs$in_dir, "grid_mv_step2.rds")
if (!file.exists(grid_path)) stop("grid_mv_step2.rds not found at: ", grid_path)

grid_mv <- readRDS(grid_path)
say("  loaded ", format(nrow(grid_mv), big.mark = ","), " rows, ",
    format(n_distinct(grid_mv$SessionId), big.mark = ","), " sessions\n")

# Column subset: the full frame is ~11M x 12 and the join below copies it.
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
# 4. ONE PASS: all cells -> all rules -> drift per age group
# ============================================================================
# The reference profile is re-estimated inside every pass, so the bootstrap
# propagates uncertainty in the weights as well as in the density. It is the
# unweighted mean of the age group's six normalised period profiles: a
# "typical day shape" for that life stage, symmetric between the ends of the
# comparison by construction.

one_pass <- function(resample = FALSE) {
  SL <- lapply(PREP, function(P) {
    if (is.null(P)) return(NULL)
    bk_slices(P, if (resample) bk_draw(P$n_sess) else NULL)
  })

  out <- list()
  for (a in AGE_LEVELS) {
    keys <- cell_key(a, PERIODS)
    sl   <- SL[keys]
    ok   <- !vapply(sl, is.null, logical(1))
    if (!all(ok)) next

    ref <- Reduce(`+`, lapply(sl, `[[`, "occ")) / length(sl)

    vals <- vapply(RULES, function(rule) {
      per <- vapply(sl, function(s) apply_rule(rule, s$m, s$occ, ref), numeric(1))
      names(per) <- PERIODS
      base <- mean(per[in_baseline(PERIODS)], na.rm = TRUE)
      end  <- per[[ENDPOINT]]
      c(base = base, end = end, drift = end - base,
        pct = 100 * (end - base) / base)
    }, numeric(4))

    n_eff <- vapply(RULES, function(rule) {
      mean(vapply(sl, function(s) rule_n_eff(rule, s$m, s$occ, ref), numeric(1)))
    }, numeric(1))

    frac <- vapply(sl, `[[`, numeric(1), "frac")
    names(frac) <- PERIODS
    fb   <- mean(frac[in_baseline(PERIODS)], na.rm = TRUE)

    out[[a]] <- tibble(
      AgeGroup = a, rule = RULES,
      base = vals["base", ], end = vals["end", ],
      drift = vals["drift", ], pct = vals["pct", ],
      n_eff_slices = n_eff,
      frac_base = fb, frac_end = frac[[ENDPOINT]],
      frac_pct = 100 * (frac[[ENDPOINT]] - fb) / fb
    )
  }
  bind_rows(out)
}

say("Point estimates...\n")
point <- one_pass(resample = FALSE)
write.csv(point, file.path(OUT_DIR, "point_estimates.csv"), row.names = FALSE)

# Reference profile and slice retention, for the SI figure.
SL0 <- lapply(PREP, function(P) if (is.null(P)) NULL else bk_slices(P, NULL))
slice_rows <- list()
for (a in AGE_LEVELS) {
  sl  <- SL0[cell_key(a, PERIODS)]
  if (any(vapply(sl, is.null, logical(1)))) next
  ref <- Reduce(`+`, lapply(sl, `[[`, "occ")) / length(sl)
  ep  <- sl[[cell_key(a, ENDPOINT)]]
  slice_rows[[a]] <- tibble(
    AgeGroup = a,
    slice    = seq_along(ref),
    hour     = (seq_along(ref) - 1) * 0.25,
    ref_weight = ref,
    occ_endpoint = ep$occ,
    m_endpoint   = ep$m,
    !!!setNames(lapply(TAUS, function(tau) ep$occ >= tau * max(ep$occ)),
                paste0("keep_thr", sprintf("%02d", TAUS * 100)))
  )
}
write.csv(bind_rows(slice_rows), file.path(OUT_DIR, "slice_weights.csv"),
          row.names = FALSE)

# ============================================================================
# 5. BOOTSTRAP
# ============================================================================

say("Bootstrapping ", B, " replicates ")
t0 <- Sys.time()
reps <- vector("list", B)
for (b in seq_len(B)) {
  reps[[b]] <- one_pass(resample = TRUE) %>%
    select(AgeGroup, rule, pct, drift, frac_pct) %>%
    mutate(rep = b)
  if (b %% 25 == 0) say(".")
}
say(" done in ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1),
    " min\n\n")

boot <- bind_rows(reps)

summ <- boot %>%
  group_by(AgeGroup, rule) %>%
  summarise(
    lo    = quantile(pct, 0.025, na.rm = TRUE),
    hi    = quantile(pct, 0.975, na.rm = TRUE),
    sd    = sd(pct, na.rm = TRUE),
    n_ok  = sum(is.finite(pct)),
    .groups = "drop"
  ) %>%
  inner_join(point %>% select(AgeGroup, rule, pct, n_eff_slices),
             by = c("AgeGroup", "rule")) %>%
  mutate(width = hi - lo,
         excludes_zero = (lo > 0) | (hi < 0)) %>%
  group_by(rule) %>%
  mutate(rank = rank(-pct, ties.method = "min")) %>%
  ungroup() %>%
  select(AgeGroup, rule, pct, lo, hi, width, sd, excludes_zero, rank,
         n_eff_slices, n_ok) %>%
  arrange(match(rule, RULES), match(AgeGroup, AGE_LEVELS))

write.csv(summ, file.path(OUT_DIR, "bootstrap_summary.csv"), row.names = FALSE)

# ============================================================================
# 6. REPORT
# ============================================================================

fmt <- function(x, d = 2) formatC(x, format = "f", digits = d, width = 6)

say("--- % DRIFT IN MEAN DISTANCE WHEN AWAY, five-period baseline -> ",
    ENDPOINT, " ---\n\n")
for (r in RULES) {
  s <- summ %>% filter(rule == r) %>% arrange(desc(pct))
  say(sprintf("%-9s  mean eff. slices %.1f\n", r, mean(s$n_eff_slices)))
  for (i in seq_len(nrow(s))) {
    say(sprintf("   %-6s %s%%  [%s, %s]  width %s %s\n",
                s$AgeGroup[i], fmt(s$pct[i]), fmt(s$lo[i]), fmt(s$hi[i]),
                fmt(s$width[i]), ifelse(s$excludes_zero[i], "*", " ")))
  }
  say("   ordering: ", paste(s$AgeGroup, collapse = " > "), "\n\n")
}

say("--- CI WIDTH BY RULE (percentage points) ---\n\n")
wtab <- summ %>% select(AgeGroup, rule, width) %>%
  pivot_wider(names_from = rule, values_from = width)
say(paste(capture.output(print(as.data.frame(wtab), row.names = FALSE)),
          collapse = "\n"), "\n\n")

say("--- RESOLVED (CI excludes zero) ---\n\n")
rtab <- summ %>% group_by(rule) %>%
  summarise(resolved = sum(excludes_zero), of = n(),
            mean_width = mean(width), .groups = "drop") %>%
  arrange(match(rule, RULES))
say(paste(capture.output(print(as.data.frame(rtab), row.names = FALSE)),
          collapse = "\n"), "\n\n")

say("--- ORDERING STABILITY ACROSS RULES (rank 1 = largest expansion) ---\n\n")
otab <- summ %>% select(AgeGroup, rule, rank) %>%
  pivot_wider(names_from = rule, values_from = rank)
say(paste(capture.output(print(as.data.frame(otab), row.names = FALSE)),
          collapse = "\n"), "\n\n")

say("--- COMPOSITIONAL LEAK: MARGINAL minus FIXED (pp of % drift) ---\n")
say("    How much of a naively integrated-out drift is retiming, not distance.\n\n")
leak <- point %>% filter(rule %in% c("fixed", "marginal")) %>%
  select(AgeGroup, rule, pct) %>%
  pivot_wider(names_from = rule, values_from = pct) %>%
  mutate(leak_pp = marginal - fixed)
say(paste(capture.output(print(as.data.frame(leak), row.names = FALSE)),
          collapse = "\n"), "\n\n")

say("Written to ", OUT_DIR, "\n")
close(con)
