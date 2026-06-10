#10.06.26 20:21 NZST
# ---------------------------------------------------------------------------
# ring_review.R
# Classify detected rings as confirmed (latewood present) or provisional
# (no latewood signal), identify the inner juvenile zone where density carries
# no boundary information, estimate the ring count there from ring spacing, and
# render a dual display that shows measured detection and inferred infill
# distinctly.
#
# Requires: densitometry.R
# ---------------------------------------------------------------------------

source("densitometry.R")


# ---------------------------------------------------------------------------
# classify_and_infill
# Split a detected ring set into confirmed and provisional rings, locate the
# inner juvenile zone, and estimate the rings it contains from the width of the
# innermost confirmed rings.
#
# Returns a list:
#   stats        ring-statistics data frame with a ring_class column
#   confirmed    boundary channels of confirmed (latewood) rings
#   provisional  boundary channels detected but without latewood
#   estimated    additional boundary channels inferred by spacing in the zone
#   zone_end_ch  last channel of the juvenile zone (0 if none)
#   n_confirmed, n_provisional, n_estimated
# ---------------------------------------------------------------------------
classify_and_infill <- function(density, boundaries,
                                step_mm         = 0.3,
                                ew_lw_threshold = 500L) {

  stats <- ring_statistics(density, boundaries, step_mm = step_mm,
                           ew_lw_threshold = ew_lw_threshold)
  n_rings     <- nrow(stats)
  ring_starts <- c(1L, boundaries)
  ring_ends   <- c(boundaries - 1L, length(density))

  confirmed_ring <- !stats$ew_only
  stats$ring_class <- ifelse(confirmed_ring, "confirmed", "provisional")

  first_conf <- which(confirmed_ring)[1L]

  zone_end_ch <- 0L
  estimated   <- integer(0)
  if (!is.na(first_conf) && first_conf > 1L) {
    zone_end_ch <- ring_ends[first_conf - 1L]

    conf_idx     <- which(confirmed_ring)
    inner_conf   <- conf_idx[seq_len(min(3L, length(conf_idx)))]
    w_conf_ch    <- (ring_ends[inner_conf] - ring_starts[inner_conf] + 1L)
    w_juv_ch     <- stats::median(w_conf_ch)

    n_est_total  <- max(1L, round(zone_end_ch / w_juv_ch))
    n_detected   <- first_conf - 1L
    n_add        <- n_est_total - n_detected
    if (n_add > 0L) {
      edges <- round(seq(0L, zone_end_ch, length.out = n_est_total + 1L))
      cand  <- edges[-c(1L, length(edges))]
      existing <- boundaries[boundaries <= zone_end_ch]
      for (c0 in cand) {
        if (all(abs(c0 - c(existing, estimated)) >= w_juv_ch / 2))
          estimated <- c(estimated, as.integer(c0))
      }
    }
  }

  provisional <- boundaries[boundaries <= zone_end_ch]
  confirmed   <- boundaries[boundaries >  zone_end_ch]

  list(stats        = stats,
       confirmed    = confirmed,
       provisional  = provisional,
       estimated    = sort(estimated),
       zone_end_ch  = zone_end_ch,
       n_confirmed  = sum(stats$ring_class == "confirmed"),
       n_provisional = sum(stats$ring_class == "provisional"),
       n_estimated  = length(estimated))
}


# ---------------------------------------------------------------------------
# review_signals: reference-free per-core signals for the review-confidence
# score. rhythm = roughness of the ring-width sequence against its own smooth
# trend; hf = high-frequency density energy (unresolved narrow rings); edge =
# median boundary prominence; n_suspect = flagged rings.
# ---------------------------------------------------------------------------
review_signals <- function(density, boundaries, step_mm = 0.3, ew_lw_threshold = 500L) {
  st <- ring_statistics(density, boundaries, step_mm = step_mm, ew_lw_threshold = ew_lw_threshold)
  w  <- st$ring_width_mm
  sw <- if (length(w) > 4) as.numeric(stats::filter(w, rep(1/5, 5), sides = 2)) else w
  sw[is.na(sw)] <- w[is.na(sw)]
  data.frame(
    n_R     = nrow(st),
    len_mm  = round(length(density) * step_mm, 1),
    rhythm  = round(mean(abs(w - sw)) / stats::median(w), 3),
    hf      = round(mean(abs(diff(density))), 1),
    edge    = round(stats::median(st$prominence, na.rm = TRUE), 1),
    n_susp  = sum(st$suspect),
    stringsAsFactors = FALSE)
}


# ---------------------------------------------------------------------------
# score_review: review-confidence score for the cores of one scan file. Two
# signals use the file context (same-age stand): a core's ring-count deviation
# and length ratio against its file siblings. Coefficients were fitted on the
# reference set (held-out AUC 0.85). A higher score means more likely off, so
# inspect first.
# ---------------------------------------------------------------------------
score_review <- function(df) {
  count_dev <- abs(df$n_R - stats::median(df$n_R))
  len_ratio <- df$len_mm / stats::median(df$len_mm)
  lp <- -2.9437 + 0.7106 * count_dev + 0.0103 * len_ratio +
         2.4150 * df$rhythm + 0.0297 * df$hf - 0.0030 * df$edge + 0.0231 * df$n_susp
  df$review_score <- round(1 / (1 + exp(-lp)), 3)
  df$review_flag  <- df$review_score >= 0.5
  df
}

# ---------------------------------------------------------------------------
# estimate_artifact_gaps: find interior low-density stretches (scan dropouts,
# piece joins) and place evenly-spaced predicted ring positions within them.
# Ring width is estimated from the median spacing of detected boundaries on
# each side of the gap. Returns channel indices of predicted positions.
# ---------------------------------------------------------------------------
estimate_artifact_gaps <- function(density, boundaries, step_mm = 0.3,
                                   gap_threshold = 200L, gap_min_mm = 5) {
  gap_min_ch <- ceiling(gap_min_mm / step_mm)
  is_gap     <- density < gap_threshold
  rle_res    <- rle(is_gap)
  ends       <- cumsum(rle_res$lengths)
  starts     <- c(1L, ends[-length(ends)] + 1L)
  estimated  <- integer(0)

  for (k in seq_along(rle_res$values)) {
    if (!rle_res$values[k] || rle_res$lengths[k] < gap_min_ch) next
    g_start <- starts[k]
    g_end   <- ends[k]
    if (g_start <= 1L || g_end >= length(density)) next

    before_b <- boundaries[boundaries < g_start]
    after_b  <- boundaries[boundaries > g_end]
    w_before <- if (length(before_b) >= 2L) stats::median(diff(before_b)) else NA_real_
    w_after  <- if (length(after_b)  >= 2L) stats::median(diff(after_b))  else NA_real_
    w_est    <- stats::median(c(w_before, w_after), na.rm = TRUE)
    if (is.na(w_est) || w_est <= 0) next

    n_est <- max(1L, round((g_end - g_start) / w_est))
    pos   <- round(seq(g_start, g_end, length.out = n_est + 1L))
    pos   <- pos[-c(1L, length(pos))]
    all_b <- c(boundaries, estimated)
    for (p in pos)
      if (!length(all_b) || min(abs(p - all_b)) >= w_est / 2)
        estimated <- c(estimated, as.integer(p))
  }
  sort(estimated)
}


# ---------------------------------------------------------------------------
# apply_clicks: toggle ring boundaries from operator click positions (channels).
# A click within tol of an existing boundary removes it; otherwise it adds one.
# ---------------------------------------------------------------------------
apply_clicks <- function(boundaries, clicks_ch, tol = 6L) {
  b <- as.integer(boundaries)
  for (ch in as.integer(clicks_ch)) {
    near <- which(abs(b - ch) <= tol)
    if (length(near)) b <- b[-near[1L]] else b <- c(b, ch)
  }
  sort(unique(b[b >= 2L]))
}


# ---------------------------------------------------------------------------
# edit_core: interactive base-R editor. Plots the core with current boundaries
# (suspect rings in firebrick, predicted proposals in tomato dashed), then takes
# operator clicks: click a line to remove it, click a predicted line to accept
# it, click a gap to add a boundary. Click DONE to finish; click EXIT to stop the
# session early — the caller saves state before checking the exit attribute.
# Returns the corrected boundary vector; attr(result, "exit") == TRUE when EXIT
# was clicked. Runs on a machine with an interactive screen.
# ---------------------------------------------------------------------------
edit_core <- function(density, boundaries, step_mm = 0.3,
                      ew_lw_threshold = 500L, tol_mm = 1.5, title = "",
                      estimated = integer(0)) {
  b    <- as.integer(boundaries)
  est  <- as.integer(estimated)
  tol  <- ceiling(tol_mm / step_mm)
  x    <- seq_along(density) * step_mm
  ymax <- max(density) + 80L
  bx1  <- max(x) * 0.89; bx2 <- max(x)
  ex1  <- max(x) * 0.76; ex2 <- max(x) * 0.88
  by1  <- ymax * 0.90;   by2 <- ymax
  repeat {
    nb <- detect_ring_boundaries(density, step_mm, ew_lw_threshold, manual_boundaries = b)
    st <- ring_statistics(density, nb, step_mm = step_mm, ew_lw_threshold = ew_lw_threshold)
    plot(x, density, type = "l", col = "grey30", lwd = 1.6, las = 1,
         ylim = c(0, ymax), xlab = "mm", ylab = "density",
         main = sprintf("%s   rings=%d   (click line=remove, red=accept, gap=add)", title, nrow(st)))
    abline(h = ew_lw_threshold, col = "orange", lty = 2, lwd = 3)
    susp <- c(FALSE, st$suspect[-1])
    abline(v = b   * step_mm, col = ifelse(susp, "firebrick", "steelblue3"), lwd = 4)
    if (length(est))
      abline(v = est * step_mm, col = "tomato", lty = 2, lwd = 3)
    rect(bx1, by1, bx2, by2, col = "palegreen3",  border = "black", lwd = 2)
    text((bx1 + bx2) / 2, (by1 + by2) / 2, "DONE", font = 2, cex = 1.2)
    rect(ex1, by1, ex2, by2, col = "lightsalmon", border = "black", lwd = 2)
    text((ex1 + ex2) / 2, (by1 + by2) / 2, "EXIT", font = 2, cex = 1.2)
    cl <- locator(1)
    if (is.null(cl)) break
    if (cl$x >= bx1 && cl$x <= bx2 && cl$y >= by1 && cl$y <= by2) break
    if (cl$x >= ex1 && cl$x <= ex2 && cl$y >= by1 && cl$y <= by2) {
      result <- detect_ring_boundaries(density, step_mm, ew_lw_threshold, manual_boundaries = b)
      attr(result, "exit") <- TRUE
      return(result)
    }
    ch       <- round(cl$x / step_mm)
    near_est <- which(abs(est - ch) <= tol)
    if (length(near_est)) {
      b   <- sort(unique(c(b, est[near_est[1L]])))
      est <- est[-near_est[1L]]
    } else {
      b <- apply_clicks(b, ch, tol)
    }
  }
  detect_ring_boundaries(density, step_mm, ew_lw_threshold, manual_boundaries = b)
}

# Dual display: confirmed boundaries as solid lines, provisional boundaries as
# dashed lines, spacing-estimated boundaries as dotted lines, and the juvenile
# review zone shaded. Writes a PNG when file is supplied.
# ---------------------------------------------------------------------------
plot_review <- function(density, cls,
                        step_mm         = 0.3,
                        ew_lw_threshold = 500L,
                        core_id         = "",
                        join_channels   = integer(0),
                        operator_added  = integer(0),
                        file            = NULL) {

  if (!is.null(file)) {
    png(filename = file, width = 1400, height = 600, res = 110)
    on.exit(dev.off(), add = TRUE)
  }

  x_mm  <- seq_along(density) * step_mm
  y_max <- max(density) + 80L
  op <- par(mar = c(4.2, 4.5, 3, 1)); on.exit(par(op), add = TRUE)

  edited <- length(operator_added) > 0L
  main_label <- if (edited)
    paste0("Core ", core_id, ": operator-corrected")
  else
    paste0("Core ", core_id, ": measured detection and juvenile-zone estimate")

  plot(x_mm, density, type = "n", las = 1, ylim = c(0, y_max), yaxt = "n",
       xlab = "Distance from inner edge (mm)",
       ylab = expression("Density (kg m"^{-3}*")"),
       main = main_label)
  .density_yaxis(y_max)

  if (cls$zone_end_ch > 0L)
    rect(0, 0, cls$zone_end_ch * step_mm, y_max, col = "#eef0f4", border = NA)

  abline(h = ew_lw_threshold, col = "firebrick", lty = 2, lwd = 3)
  lines(x_mm, density, lwd = 1.6, col = "grey25")

  abline(v = cls$confirmed   * step_mm, col = "steelblue4",     lty = 1, lwd = 4)
  abline(v = cls$provisional * step_mm, col = "darkorange2",    lty = 2, lwd = 4)
  abline(v = cls$estimated   * step_mm, col = "goldenrod3",     lty = 3, lwd = 3.5)
  abline(v = join_channels   * step_mm, col = "purple3",        lty = 1, lwd = 3)
  abline(v = operator_added  * step_mm, col = "mediumseagreen", lty = 1, lwd = 4)

  leg_labels <- c("Density", sprintf("Latewood threshold (%d)", ew_lw_threshold),
                  "Juvenile review zone", "Confirmed boundary",
                  "Provisional boundary", "Spacing estimate", "Piece join")
  leg_cols   <- c("grey25", "firebrick", "#eef0f4", "steelblue4",
                  "darkorange2", "goldenrod3", "purple3")
  leg_lty    <- c(1, 2, NA, 1, 2, 3, 1)
  leg_pch    <- c(NA, NA, 15, NA, NA, NA, NA)
  leg_lwd    <- c(1.6, 3, NA, 4, 4, 3.5, 3)
  if (edited) {
    leg_labels <- c(leg_labels, "Operator added")
    leg_cols   <- c(leg_cols,   "mediumseagreen")
    leg_lty    <- c(leg_lty,    1)
    leg_pch    <- c(leg_pch,    NA)
    leg_lwd    <- c(leg_lwd,    4)
  }

  legend("bottomright", legend = leg_labels, col = leg_cols,
         lty = leg_lty, pch = leg_pch, pt.cex = 2, lwd = leg_lwd,
         bty = "o", bg = "white", box.col = "grey70", cex = 0.75)

  invisible(NULL)
}
