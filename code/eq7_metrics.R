######################################################################
# eq7_metrics.R -- the Eq. (7) point estimate, defined once
#
# WHY THIS FILE EXISTS
# --------------------
# `step4_integral.R` and `step4_integral_R2.R` each carried a byte-identical
# copy of this computation, and the second overwrites the first's Figure 6b
# and Figure S3. Editing one and not the other changed nothing and raised no
# error. Eq. (7) now has one definition, here.
#
# WHAT CHANGED (2026-09-09)
# -------------------------
# Eq. (7) averages the per-slice conditional mean distance over "occupied"
# slices. The old code implemented occupancy as `mean_d_active > 0`, but the
# KDE is positive everywhere after smoothing, so all 96 slices qualified and
# 03:00 counted as heavily as noon. Bootstrap CV of the slice mean was 49%
# over 00-05h against 2.2% over 07-19h, which is why the distance axis of
# Figure 6a was unresolved while the away axis was tight.
#
# The replacement is a fixed-weight (standardised) mean: slices are weighted
# by an occupancy profile, but that profile is held CONSTANT across periods
# at each age group's own day shape averaged over the five baseline periods.
#
#   - Weighting by occupancy gives the precision of an integrated-out
#     marginal: empty hours carry almost no weight.
#   - Freezing the weights keeps the quantity conditional. Because r and t
#     are dependent, weighting each period by its OWN occupancy would let a
#     change in WHEN people are out register as a change in HOW FAR they go,
#     and the two axes of the drift would stop measuring separate things.
#     Fixed weights make that leak exactly zero rather than merely small.
#   - Averaging the reference over the five baseline periods (not six) keeps
#     the 2019-2021 day shape out of the weights, matching BASELINE_EXCLUDE.
#
# Two sweeps back this (see FINDINGS.md, 2026-09-09):
#   - The occupancy-threshold alternative fails its stability precondition:
#     the age ordering moves with tau and the 10-17 estimate spans 10.2 pp
#     across the sweep. Rejected.
#   - The reference profile is not a live degree of freedom. Across five
#     choices (pooled six periods, baseline five, first period alone, last
#     period alone, one profile shared by all age groups) the spread is at
#     most 0.43 bootstrap standard errors and 11% of the interval width.
#
# The bootstrap in `step8_uncertainty_R2.R` applies the SAME rule through
# `eq7_weighting_lib.R::apply_rule("fixed", ...)`. If you change the rule
# here, change it there, or Figure 6a's arrow and its fan go back to
# measuring different things.
#
# Requires utils_io.R (in_baseline).
######################################################################

# Reference profile for the fixed weights. "base5" = each age group's mean
# normalised occupancy over the five baseline periods. Alternatives measured
# in results/sensitivity/eq7_reference/; all agree to well within one
# standard error.
EQ7_REFERENCE <- "base5"

#' Per-cell action-space metrics from the step 2 year-bin KDE.
#'
#' @param yearbin_densities long frame with AgeGroup, YearBin, t, d, density
#' @param home_cutoff km; must match step 2
#' @return one row per (AgeGroup, Period) with the columns step 5, step 7 and
#'   step 8 read. `mean_distance_active` is Eq. (7).
compute_cell_metrics <- function(yearbin_densities, home_cutoff) {
  dt_val <- 24 / 96   # hours per time slice
  dd_val <- 60 / 96   # km per distance cell

  # --- per-slice profiles for every age-period cell ------------------------
  slice_profiles <- yearbin_densities %>%
    dplyr::group_by(AgeGroup, YearBin, t) %>%
    dplyr::summarise(
      mean_d  = stats::weighted.mean(d, w = density, na.rm = TRUE),
      occ_raw = sum(density[d > home_cutoff]),
      mean_d_active = {
        active   <- density[d > home_cutoff]
        d_active <- d[d > home_cutoff]
        if (sum(active) > 0) {
          stats::weighted.mean(d_active, w = active, na.rm = TRUE)
        } else {
          0
        }
      },
      .groups = "drop"
    ) %>%
    dplyr::group_by(AgeGroup, YearBin) %>%
    dplyr::mutate(occ = occ_raw / sum(occ_raw)) %>%
    dplyr::ungroup()

  # --- the fixed reference profile, one per age group ----------------------
  # Averaged over the five baseline periods, then applied unchanged to every
  # period including the endpoint. This is what makes the weights fixed.
  if (EQ7_REFERENCE != "base5")
    stop("Unsupported EQ7_REFERENCE: ", EQ7_REFERENCE)

  ref_profiles <- slice_profiles %>%
    dplyr::filter(in_baseline(YearBin)) %>%
    dplyr::group_by(AgeGroup, t) %>%
    dplyr::summarise(ref = mean(occ), .groups = "drop")

  n_ref_periods <- slice_profiles %>%
    dplyr::filter(in_baseline(YearBin)) %>%
    dplyr::distinct(YearBin) %>%
    nrow()
  cat("  Eq. (7) reference: '", EQ7_REFERENCE, "' over ", n_ref_periods,
      " periods, per age group\n", sep = "")

  # A slice carrying no away mass would enter the weighted mean as a zero
  # distance. After smoothing this does not happen; warn rather than silently
  # bias the estimate if it ever does.
  n_empty <- sum(slice_profiles$mean_d_active <= 0)
  if (n_empty > 0)
    warning("Eq. (7): ", n_empty, " slice(s) carry no away mass and enter ",
            "the fixed-weight mean as zero distance.")

  # --- collapse to one row per cell ---------------------------------------
  slice_profiles %>%
    dplyr::left_join(ref_profiles, by = c("AgeGroup", "t")) %>%
    dplyr::group_by(AgeGroup, YearBin) %>%
    dplyr::summarise(
      fraction_away = sum(occ_raw) * dt_val * dd_val,
      mean_distance = mean(mean_d),
      # Eq. (7): fixed-weight mean over the day.
      mean_distance_active = sum(ref * mean_d_active) / sum(ref),
      volume        = sum(mean_d) * dt_val,   # km-hours per day
      # Descriptive only, no downstream consumer; left on equal slice weights
      # so it keeps its previous meaning.
      dist_p90      = stats::quantile(mean_d_active[mean_d_active > 0], 0.90,
                                      na.rm = TRUE),
      var_distance  = stats::var(mean_d),
      .groups = "drop"
    ) %>%
    dplyr::rename(Period = YearBin)
}
