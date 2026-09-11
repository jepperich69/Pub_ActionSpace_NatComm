######################################################################
# bootstrap_kde.R -- session bootstrap of the ESTIMAND IN EQ. (7)
#
# Why this exists
# ---------------
# Methods states that the bootstrap "recomputes the full drift vector from
# the resampled KDE surfaces". It did not. step8 collapsed each diary day to
# one per-person mean distance and resampled those, which is a different
# estimand from Eq. (7): a per-slice conditional mean, taken on the KDE
# surface and averaged over occupied time slices. The two were then reconciled
# by re-centring the replicate cloud on the KDE point estimate, which forces
# agreement in location while leaving the spread that of the wrong quantity.
#
# With the corrected sample that broke visibly: the 31-55 point estimate
# (+14.80%) fell outside its own 95% interval (-1.68 to 6.27).
#
# This module resamples diary sessions, rebuilds the away density from the
# resampled rows exactly as step2 does, and evaluates exactly the functional
# step4 applies. No re-centring: the interval is the sampling distribution of
# the quantity actually plotted.
#
# Speed
# -----
# Refitting a 96x96 kde2d per replicate per age-period cell is ~15,000 fits.
# A product-Gaussian KDE is a convolution, so binning the observations once
# turns each refit into two small dense matrix products:
#
#     density_away = Kt %*% H %*% t(Kr)
#
# with H the weighted 2-D histogram of the resampled away rows. Kt and Kr are
# precomputed. Only H changes per replicate, and it is a sparse matrix-vector
# product against the session draw counts. Bin widths (t exact on the 15-min
# grid, r at 0.1 km against a 1.25 km kernel sd) make the binning error
# negligible relative to the smoothing.
######################################################################

suppressPackageStartupMessages({
  library(Matrix)
})

# Grid and kernel constants -- must match step2_kernel_generation.R
BK_R_MAX  <- 60
BK_N_T    <- 96
BK_N_D    <- 96
BK_CUTOFF <- 0.02
BK_R_BIN  <- 0.1    # km, binning resolution for the r axis
BK_R_TOP  <- 80     # km, beyond which kernel mass at the grid is nil

#' Precompute everything that does not change across bootstrap replicates.
#'
#' @param df data.frame for ONE age-period cell with columns
#'   SessionId, TimeMSM, r_rad_km, SessionWeight (all rows, not just away).
#' @param h_t,h_d bandwidths in the kde2d convention (Gaussian sd = h/4).
bk_prepare <- function(df, h_t = 1, h_d = 5) {
  df <- df[is.finite(df$SessionWeight) & df$SessionWeight > 0 &
             !is.na(df$TimeMSM) & !is.na(df$r_rad_km), ]
  if (!nrow(df)) return(NULL)

  sess      <- factor(df$SessionId)
  n_sess    <- nlevels(sess)
  sess_idx  <- as.integer(sess)

  # Per-session total and home weight (each row of a session carries the
  # same SessionWeight, so these are just weighted row counts).
  is_home  <- df$r_rad_km <= BK_CUTOFF
  tot_w    <- as.vector(rowsum(df$SessionWeight, sess_idx, reorder = TRUE))
  home_w   <- as.vector(rowsum(df$SessionWeight * is_home, sess_idx, reorder = TRUE))

  # Away rows -> sparse (cell x session) weight matrix
  aw <- !is_home
  if (sum(aw) < 50) return(NULL)

  t_bin <- as.integer(round(df$TimeMSM[aw] / 15)) + 1L      # 1..96
  t_bin <- pmin(pmax(t_bin, 1L), BK_N_T)
  r_bin <- as.integer(pmin(df$r_rad_km[aw], BK_R_TOP) / BK_R_BIN) + 1L
  M     <- as.integer(BK_R_TOP / BK_R_BIN) + 1L
  r_bin <- pmin(pmax(r_bin, 1L), M)

  # Column-major: H[t, r] must sit at (r - 1) * N_T + t so that
  # matrix(vec, N_T, M) below lays the histogram out as (time x distance).
  cell_idx <- (r_bin - 1L) * BK_N_T + t_bin
  S <- sparseMatrix(i = cell_idx, j = sess_idx[aw], x = df$SessionWeight[aw],
                    dims = c(BK_N_T * M, n_sess))

  # Kernel matrices (evaluation grid x bin centre)
  grid_t <- seq(0, 24, length.out = BK_N_T)
  grid_d <- seq(0, BK_R_MAX, length.out = BK_N_D)
  t_ctr  <- (seq_len(BK_N_T) - 1) * 0.25          # hours, exact 15-min grid
  r_ctr  <- (seq_len(M) - 1) * BK_R_BIN

  Kt <- matrix(dnorm(outer(grid_t, t_ctr, "-") / (h_t / 4)), BK_N_T)
  Kr <- matrix(dnorm(outer(grid_d, r_ctr, "-") / (h_d / 4)), BK_N_D)

  list(S = S, M = M, n_sess = n_sess, tot_w = tot_w, home_w = home_w,
       Kt = Kt, Kr = Kr, grid_d = grid_d,
       away_cols = which(grid_d > BK_CUTOFF))
}

#' Evaluate Eq. (7) and the away fraction for a given multiset of sessions.
#'
#' @param P output of bk_prepare
#' @param cnt integer vector of length P$n_sess: how many times each session
#'   was drawn. Pass NULL for the point estimate (every session once).
bk_metrics <- function(P, cnt = NULL) {
  if (is.null(P)) return(c(frac_away = NA_real_, mean_dist_active = NA_real_))
  if (is.null(cnt)) cnt <- rep(1, P$n_sess)

  H <- matrix(as.vector(P$S %*% cnt), BK_N_T, P$M)
  D <- P$Kt %*% H %*% t(P$Kr)          # 96 x 96, unnormalised away density

  tot <- sum(D)
  if (!is.finite(tot) || tot <= 0)
    return(c(frac_away = NA_real_, mean_dist_active = NA_real_))

  # Home share, weighted, on the same resample
  w_tot  <- sum(P$tot_w  * cnt)
  p_home <- if (w_tot > 0) sum(P$home_w * cnt) / w_tot else NA_real_

  # step4: fraction_away integrates the combined density over d > cutoff.
  # Normalisation and the (1 - p_home) scaling are applied exactly as step2
  # builds the surface; the home spike sits on the d = 0 grid row alone.
  ac   <- P$away_cols
  frac <- (1 - p_home) * sum(D[, ac]) / tot

  # step4: per-time-slice conditional mean, averaged over occupied slices.
  # The (1 - p_home) factor and the normalisation cancel in a weighted mean.
  num <- as.vector(D[, ac] %*% P$grid_d[ac])
  den <- rowSums(D[, ac])
  m   <- ifelse(den > 0, num / den, 0)
  mda <- mean(m[m > 0], na.rm = TRUE)

  c(frac_away = frac, mean_dist_active = mda)
}

#' Draw session counts for one bootstrap replicate (with replacement).
bk_draw <- function(n_sess) tabulate(sample.int(n_sess, n_sess, replace = TRUE),
                                     nbins = n_sess)
