######################################################################
# eq7_weighting_lib.R -- shared slice evaluator for the Eq. (7) sweeps
#
# `bootstrap_kde.R::bk_metrics()` collapses the away density straight to the
# two scalars the figure plots. Every candidate weighting rule for Eq. (7) is
# a different average of the SAME per-slice vectors, so this file exposes
# those vectors instead and applies the rules on top. Arithmetic is identical
# to bk_metrics() up to the final average, which is what makes the rules
# comparable to each other and to the published estimates.
#
# Used by sweep_eq7_weighting.R (threshold sweep) and sweep_eq7_reference.R
# (reference-profile sensitivity). Not on the manuscript path.
#
# Requires bootstrap_kde.R to have been sourced (BK_* constants, bk_prepare).
######################################################################

#' Per-slice conditional mean distance and occupancy for one age-period cell.
#'
#' @param P output of bk_prepare()
#' @param cnt session draw counts, or NULL for the point estimate
#' @return list(m, occ, frac): m[t] is the mass-weighted mean distance within
#'   slice t, occ[t] the slice's share of the day's away mass (sums to 1),
#'   frac the fraction of the day spent away.
bk_slices <- function(P, cnt = NULL) {
  if (is.null(P)) return(NULL)
  if (is.null(cnt)) cnt <- rep(1, P$n_sess)

  H <- matrix(as.vector(P$S %*% cnt), BK_N_T, P$M)
  D <- P$Kt %*% H %*% t(P$Kr)

  tot <- sum(D)
  if (!is.finite(tot) || tot <= 0) return(NULL)

  w_tot  <- sum(P$tot_w * cnt)
  p_home <- if (w_tot > 0) sum(P$home_w * cnt) / w_tot else NA_real_

  ac   <- P$away_cols
  frac <- (1 - p_home) * sum(D[, ac]) / tot

  num <- as.vector(D[, ac] %*% P$grid_d[ac])   # per-slice sum of r * mass
  den <- rowSums(D[, ac])                      # per-slice away mass
  m   <- ifelse(den > 0, num / den, 0)

  list(m = m, occ = den / sum(den), frac = frac)
}

#' Apply one weighting rule to a cell's slice vectors.
#'
#' @param rule "all" (equal weight over occupied slices, the current Eq. 7),
#'   "thrNN" (equal weight above NN% of the day's peak away mass),
#'   "fixed" (weight by `ref`, held constant across periods),
#'   "marginal" (weight by the cell's own occupancy, i.e. t integrated out).
#' @param occ the cell's OWN occupancy profile; `ref` the fixed reference.
apply_rule <- function(rule, m, occ, ref) {
  if (rule == "all") {
    v <- m[m > 0]
    return(if (length(v)) mean(v) else NA_real_)
  }
  if (rule == "fixed")    return(sum(ref * m) / sum(ref))
  if (rule == "marginal") return(sum(occ * m) / sum(occ))
  tau <- as.numeric(sub("^thr", "", rule)) / 100
  keep <- occ >= tau * max(occ)
  if (!any(keep)) return(NA_real_)
  mean(m[keep])
}

#' Effective number of slices a rule averages over (Kish for weighted rules,
#' a plain count for the equal-weight ones).
rule_n_eff <- function(rule, m, occ, ref) {
  w <- switch(rule,
    all      = as.numeric(m > 0),
    fixed    = ref,
    marginal = occ,
    as.numeric(occ >= (as.numeric(sub("^thr", "", rule)) / 100) * max(occ))
  )
  if (sum(w) <= 0) return(NA_real_)
  sum(w)^2 / sum(w^2)
}

# ---------------------------------------------------------------------------
# Production Eq. (7): fixed-weight mean, reference = "base5"
# ---------------------------------------------------------------------------
# These two are what step8_uncertainty_R2.R uses, so that the bootstrap
# interval and the step4 point estimate in eq7_metrics.R are the same
# functional. Changing the rule in one place without the other puts Figure
# 6a's arrow and its fan back on different estimands, which was the referee's
# M4. Requires utils_io.R (in_baseline).

#' An age group's fixed reference occupancy profile: the mean of its
#' normalised per-slice away mass over the five baseline periods. Built from
#' whatever data it is handed, so a bootstrap replicate rebuilds it too and
#' the interval carries the uncertainty in the weights as well.
#'
#' @param sl list of bk_slices() outputs for one age group, named by period
#' @param periods period labels, in order
eq7_reference <- function(sl, periods) {
  base_p <- periods[in_baseline(periods)]
  occ <- lapply(sl[base_p], `[[`, "occ")
  Reduce(`+`, occ) / length(occ)
}

#' Fixed-weight Eq. (7) and away fraction for every age-period cell.
#'
#' @param SL list of bk_slices() outputs, named `paste(AgeGroup, YearBin)`
#' @return tibble(AgeGroup, YearBin, frac_away, mean_dist)
eq7_cell_means <- function(SL, age_levels, periods) {
  dplyr::bind_rows(lapply(age_levels, function(a) {
    sl <- SL[paste(a, periods)]
    if (any(vapply(sl, is.null, logical(1))))
      return(dplyr::tibble(AgeGroup = a, YearBin = periods,
                           frac_away = NA_real_, mean_dist = NA_real_))
    names(sl) <- periods
    ref <- eq7_reference(sl, periods)
    dplyr::tibble(
      AgeGroup  = a,
      YearBin   = periods,
      frac_away = vapply(sl, `[[`, numeric(1), "frac"),
      mean_dist = vapply(sl, function(s) apply_rule("fixed", s$m, s$occ, ref),
                         numeric(1))
    )
  }))
}
