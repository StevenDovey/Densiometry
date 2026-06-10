#10.06.26 17:15 NZST
# ---------------------------------------------------------------------------
# FRI Direct-Scanning X-Ray Densitometer data processing.
# Reimplements the EDITOR program from Cown and Clement (1983), Wood Science
# and Technology 17:91-99 (system software upgraded in the 1990s).
#
# Pipeline: parse_scn() -> trim_air_channels() -> detect_ring_boundaries()
#           -> ring_statistics(). Top-level wrapper: process_scn().
#
# Ring detection is prominence based so that weak juvenile-wood latewood is
# still found, and ambiguous boundaries are flagged as suspect (possible false
# or intra-annual rings) for operator review.
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# parse_scn
# Read a raw .SCN file and return a named list of core objects. Multi-piece
# scans (suffixes a, b, c) are concatenated in suffix order, with the join
# channel positions recorded.
#
# SCN header field positions after "####":
#   [1] core_id  [2] pith_offset_estimate  [3] critical_count
#   [4] attenuation_factor  [5] step_mm  [6] air_count  [7] scan_code  [8] aux
#
# The pith offset estimate and scan code are acquisition metadata. Calendar
# year and final pith index are assigned during editing and are not in the
# .SCN, so rings are labelled by position.
# ---------------------------------------------------------------------------
parse_scn <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)

  header_idx <- grep("^####", lines)
  term_idx   <- grep("^\\*{4}$", lines)
  stopifnot(length(header_idx) == length(term_idx))

  cores <- vector("list", length(header_idx))

  for (i in seq_along(header_idx)) {
    h     <- sub("^####\\s*", "", lines[header_idx[i]])
    parts <- strsplit(trimws(h), "\\s+")[[1]]

    data_lines <- lines[(header_idx[i] + 1L):(term_idx[i] - 1L)]
    density    <- as.integer(unlist(strsplit(trimws(data_lines), "\\s+")))
    density    <- density[!is.na(density)]

    cores[[i]] <- list(
      core_id              = parts[1],
      pith_offset_estimate = as.integer(parts[2]),
      critical_count       = as.integer(parts[3]),
      attenuation_factor   = as.numeric(parts[4]),
      step_mm              = as.numeric(parts[5]),
      air_count            = as.numeric(parts[6]),
      scan_code            = as.integer(parts[7]),
      density              = density,
      n_channels           = length(density),
      join_channels        = integer(0)
    )
    names(cores)[i] <- parts[1]
  }

  base_ids <- sub("[abc]$", "", names(cores))
  for (base in unique(base_ids[duplicated(base_ids)])) {
    pieces <- cores[base_ids == base]
    pieces <- pieces[order(names(pieces))]

    dens  <- integer(0)
    joins <- integer(0)
    for (p in pieces) {
      if (length(dens) > 0L) joins <- c(joins, length(dens) + 1L)
      dens <- c(dens, p$density)
    }

    merged <- pieces[[1L]]
    merged$core_id       <- base
    merged$density       <- dens
    merged$n_channels    <- length(dens)
    merged$scan_code     <- pieces[[length(pieces)]]$scan_code
    merged$air_count     <- mean(vapply(pieces, function(p) p$air_count, numeric(1)))
    merged$join_channels <- joins

    cores[names(pieces)] <- NULL
    cores[[base]] <- merged
  }

  cores
}


# ---------------------------------------------------------------------------
# trim_air_channels
# Remove leading low-density channels produced as the beam enters the wood.
# Only the leading edge is affected; the trailing edge stops scanning at air.
# ---------------------------------------------------------------------------
trim_air_channels <- function(density, threshold = 200L) {
  start <- which(density >= threshold)[1L]
  density[start:length(density)]
}


# ---------------------------------------------------------------------------
# .running_mean: centred running mean.
# ---------------------------------------------------------------------------
.running_mean <- function(x, n = 5L) {
  if (n <= 1L) return(as.numeric(x))
  sm <- as.numeric(stats::filter(x, rep(1 / n, n), sides = 2))
  sm[is.na(sm)] <- x[is.na(sm)]
  sm
}


# ---------------------------------------------------------------------------
# .local_extrema: indices of local maxima (peaks) and minima (troughs).
# ---------------------------------------------------------------------------
.local_extrema <- function(x) {
  d <- sign(diff(x))
  for (i in which(d == 0)) d[i] <- if (i > 1L) d[i - 1L] else 0
  list(peaks   = which(d[-length(d)] > 0 & d[-1] < 0) + 1L,
       troughs = which(d[-length(d)] < 0 & d[-1] > 0) + 1L)
}


# ---------------------------------------------------------------------------
# .no_ring_result: boundary object for a core with no detectable ring.
# ---------------------------------------------------------------------------
.no_ring_result <- function(density, ew_lw_threshold) {
  b <- integer(0)
  attr(b, "ew_only_flags")  <- !any(density >= ew_lw_threshold)
  attr(b, "peak_channel")   <- integer(0)
  attr(b, "peak_density")   <- NA_integer_
  attr(b, "prominence")     <- NA_integer_
  attr(b, "suspect")        <- TRUE
  attr(b, "suspect_reason") <- "no ring detected"
  b
}


# ---------------------------------------------------------------------------
# detect_ring_boundaries
# Locate ring boundaries (latewood to earlywood transitions) from the density
# trace. Returns the channel positions marking the first channel of each new
# ring (rings 2..N); ring 1 starts at channel 1.
#
# Peaks are kept when their prominence, the height above the higher adjacent
# trough, exceeds prominence_frac times the trace range. Each boundary is
# placed at the steepest density drop after a kept latewood peak.
#
# Attributes (length n_rings): ew_only_flags, peak_channel, peak_density,
# prominence, suspect, suspect_reason. manual_boundaries bypasses detection.
# ---------------------------------------------------------------------------
detect_ring_boundaries <- function(density,
                                   step_mm           = 0.3,
                                   ew_lw_threshold   = 500L,
                                   min_ring_mm       = 2,
                                   smooth_n          = 5L,
                                   prominence_frac   = 0.08,
                                   manual_boundaries = NULL) {

  n      <- length(density)
  min_ch <- max(1L, ceiling(min_ring_mm / step_mm))

  if (!is.null(manual_boundaries)) {
    mb <- sort(unique(as.integer(manual_boundaries)))
    mb <- mb[mb >= 2L & mb <= n]
    n_rings     <- length(mb) + 1L
    ring_starts <- c(1L, mb)
    ring_ends   <- c(mb - 1L, n)
    ew_only <- vapply(seq_len(n_rings), function(k)
      !any(density[ring_starts[k]:ring_ends[k]] >= ew_lw_threshold), logical(1))
    attr(mb, "ew_only_flags")  <- ew_only
    attr(mb, "peak_channel")   <- integer(0)
    attr(mb, "peak_density")   <- rep(NA_integer_, n_rings)
    attr(mb, "prominence")     <- rep(NA_integer_, n_rings)
    attr(mb, "suspect")        <- rep(FALSE, n_rings)
    attr(mb, "suspect_reason") <- rep("", n_rings)
    return(mb)
  }

  sm  <- .running_mean(density, smooth_n)
  ext <- .local_extrema(sm)
  peaks   <- ext$peaks
  troughs <- ext$troughs
  if (length(peaks) == 0L) return(.no_ring_result(density, ew_lw_threshold))

  span <- max(sm) - min(sm)
  prom <- numeric(length(peaks))
  for (j in seq_along(peaks)) {
    p  <- peaks[j]
    lt <- troughs[troughs < p]; rt <- troughs[troughs > p]
    left_min  <- if (length(lt)) sm[max(lt)] else min(sm[1:p])
    right_min <- if (length(rt)) sm[min(rt)] else min(sm[p:n])
    prom[j]   <- sm[p] - max(left_min, right_min)
  }

  keep  <- prom >= prominence_frac * span
  peaks <- peaks[keep]
  prom  <- prom[keep]
  if (length(peaks) == 0L) return(.no_ring_result(density, ew_lw_threshold))

  ord <- order(peaks); peaks <- peaks[ord]; prom <- prom[ord]
  repeat {
    if (length(peaks) < 2L) break
    too_close <- which(diff(peaks) < min_ch)
    if (length(too_close) == 0L) break
    k    <- too_close[1L]
    drop <- if (prom[k] >= prom[k + 1L]) k + 1L else k
    peaks <- peaks[-drop]; prom <- prom[-drop]
  }

  n_rings  <- length(peaks)
  peak_den <- sm[peaks]

  boundaries <- integer(0)
  if (n_rings >= 2L) {
    for (k in seq_len(n_rings - 1L)) {
      p1 <- peaks[k]; p2 <- peaks[k + 1L]
      seg_trough <- p1 + which.min(sm[p1:p2]) - 1L
      if (seg_trough <= p1 + 1L) {
        b <- p1 + 1L
      } else {
        b <- p1 + which.min(diff(sm[p1:seg_trough]))
      }
      boundaries <- c(boundaries, b)
    }
  }
  boundaries <- sort(unique(pmin(pmax(boundaries, 2L), n)))

  ring_starts <- c(1L, boundaries)
  ring_ends   <- c(boundaries - 1L, n)
  n_rings     <- length(ring_starts)

  ew_only <- logical(n_rings)
  for (k in seq_len(n_rings))
    ew_only[k] <- !any(density[ring_starts[k]:ring_ends[k]] >= ew_lw_threshold)

  widths_mm <- (ring_ends - ring_starts + 1L) * step_mm
  med_w     <- stats::median(widths_mm)
  thr       <- prominence_frac * span
  suspect        <- logical(n_rings)
  suspect_reason <- character(n_rings)
  for (k in seq_len(n_rings)) {
    reasons <- character(0)
    if (peak_den[k] < ew_lw_threshold) reasons <- c(reasons, "weak latewood")
    if (prom[k] < 1.5 * thr)           reasons <- c(reasons, "low prominence")
    if (widths_mm[k] < 0.45 * med_w)   reasons <- c(reasons, "narrow ring")
    if (length(reasons)) { suspect[k] <- TRUE; suspect_reason[k] <- paste(reasons, collapse = "; ") }
  }

  attr(boundaries, "ew_only_flags")  <- ew_only
  attr(boundaries, "peak_channel")   <- peaks
  attr(boundaries, "peak_density")   <- round(peak_den)
  attr(boundaries, "prominence")     <- round(prom)
  attr(boundaries, "suspect")        <- suspect
  attr(boundaries, "suspect_reason") <- suspect_reason
  boundaries
}


# ---------------------------------------------------------------------------
# ring_statistics
# Per-ring statistics matching the EDITOR output, plus cross-sectional areas
# in square centimetres and suspect flags. Rings are labelled by position;
# ring_no = rings_offset + 1, 2, ... Year is assigned downstream.
# ---------------------------------------------------------------------------
ring_statistics <- function(density,
                            boundaries,
                            step_mm         = 0.3,
                            ew_lw_threshold = 500L,
                            rings_offset    = 0L) {

  ew_only_flags  <- attr(boundaries, "ew_only_flags")
  suspect_flags  <- attr(boundaries, "suspect")
  suspect_reason <- attr(boundaries, "suspect_reason")
  peak_density   <- attr(boundaries, "peak_density")
  prominence     <- attr(boundaries, "prominence")

  ring_starts <- c(1L, boundaries)
  ring_ends   <- c(boundaries - 1L, length(density))
  n_rings     <- length(ring_starts)

  outer_radius_mm <- round(ring_ends * step_mm, 1)
  inner_radius_mm <- round((ring_starts - 1L) * step_mm, 1)

  out <- data.frame(
    ring_no         = rings_offset + seq_len(n_rings),
    inner_radius_mm = inner_radius_mm,
    outer_radius_mm = outer_radius_mm,
    ring_width_mm   = round((ring_ends - ring_starts + 1L) * step_mm, 1),
    ew_width_mm     = NA_real_,
    lw_width_mm     = NA_real_,
    pct_latewood    = NA_real_,
    incr_area_cm2   = NA_real_,
    total_area_cm2  = NA_real_,
    ring_mean       = NA_integer_,
    ew_density      = NA_integer_,
    lw_density      = NA_integer_,
    min_density     = NA_integer_,
    max_density     = NA_integer_,
    uniformity      = NA_integer_,
    range_density   = NA_integer_,
    lw_peak_density = peak_density,
    prominence      = prominence,
    partial_ring    = FALSE,
    ew_only         = ew_only_flags,
    suspect         = suspect_flags,
    suspect_reason  = suspect_reason,
    stringsAsFactors = FALSE
  )

  if (rings_offset > 0L) out$partial_ring[1L] <- TRUE

  for (k in seq_len(n_rings)) {
    d  <- density[ring_starts[k]:ring_ends[k]]
    ew <- d[d <  ew_lw_threshold]
    lw <- d[d >= ew_lw_threshold]

    r_out <- outer_radius_mm[k] / 10
    r_in  <- inner_radius_mm[k] / 10

    out$ew_width_mm[k]   <- round(length(ew) * step_mm, 1)
    out$lw_width_mm[k]   <- round(length(lw) * step_mm, 1)
    out$pct_latewood[k]  <- round(100 * length(lw) / length(d))
    out$incr_area_cm2[k] <- round(pi * (r_out^2 - r_in^2), 1)
    out$ring_mean[k]     <- round(mean(d))
    out$ew_density[k]    <- if (length(ew) > 0L) round(mean(ew)) else NA_integer_
    out$lw_density[k]    <- if (length(lw) > 0L) round(mean(lw)) else NA_integer_
    out$min_density[k]   <- min(d)
    out$max_density[k]   <- max(d)
    out$uniformity[k]    <- if (length(ew) > 0L && length(lw) > 0L)
                              round(mean(lw) - mean(ew)) else NA_integer_
    out$range_density[k] <- max(d) - min(d)
  }
  out$total_area_cm2 <- round(cumsum(out$incr_area_cm2), 1)

  out
}


# ---------------------------------------------------------------------------
# plot_density_profile
# Annotated density trace: latewood shaded, ring boundaries drawn, ring
# numbers labelled along the top, latewood peaks marked, suspect rings in red.
# Writes a PNG when file is supplied.
# ---------------------------------------------------------------------------
plot_density_profile <- function(density,
                                 boundaries      = NULL,
                                 stats           = NULL,
                                 step_mm         = 0.3,
                                 ew_lw_threshold = 500L,
                                 core_id         = "",
                                 file            = NULL) {

  if (!is.null(file)) {
    png(filename = file, width = 1400, height = 600, res = 110)
    on.exit(dev.off(), add = TRUE)
  }

  x_mm  <- seq_along(density) * step_mm
  y_max <- max(density) + 80L

  op <- par(mar = c(4.2, 4.5, 3, 1)); on.exit(par(op), add = TRUE)
  plot(x_mm, density, type = "n",
       xlab = "Distance from inner edge (mm)",
       ylab = expression("Density (kg m"^{-3}*")"),
       main = paste0("Core ", core_id, ": density profile and ring boundaries"),
       ylim = c(0, y_max), las = 1)

  lw_mask <- density >= ew_lw_threshold
  if (any(lw_mask)) {
    rl <- rle(lw_mask); ends <- cumsum(rl$lengths); starts <- ends - rl$lengths + 1L
    for (j in which(rl$values))
      rect(x_mm[starts[j]] - step_mm / 2, 0, x_mm[ends[j]] + step_mm / 2, y_max,
           col = "#ffe9b0", border = NA)
  }

  abline(h = ew_lw_threshold, col = "firebrick", lty = 2, lwd = 1)
  lines(x_mm, density, lwd = 0.8, col = "grey25")

  ring_starts <- c(1L, boundaries)
  ring_ends   <- c(boundaries - 1L, length(density))
  if (length(boundaries) > 0L) {
    susp_b <- if (!is.null(stats)) stats$suspect[-1] else rep(FALSE, length(boundaries))
    abline(v = boundaries * step_mm,
           col = ifelse(susp_b, "red", "steelblue3"),
           lty = 3, lwd = ifelse(susp_b, 1.4, 0.9))
  }

  pk <- attr(boundaries, "peak_channel")
  if (length(pk) > 0L) points(pk * step_mm, density[pk], pch = 25, col = "darkgreen",
                              bg = "darkgreen", cex = 0.6)

  if (!is.null(stats)) {
    mids <- ((ring_starts + ring_ends) / 2) * step_mm
    text(mids, y_max * 0.97, labels = stats$ring_no, cex = 0.6,
         col = ifelse(stats$suspect, "red", "grey20"), srt = 90)
  }

  legend("bottomright",
         legend = c("Density", sprintf("Latewood threshold (%d)", ew_lw_threshold),
                    "Latewood", "Ring boundary", "Suspect ring", "Latewood peak"),
         col = c("grey25", "firebrick", "#ffe9b0", "steelblue3", "red", "darkgreen"),
         lty = c(1, 2, NA, 3, 3, NA), pch = c(NA, NA, 15, NA, NA, 25),
         pt.bg = c(NA, NA, NA, NA, NA, "darkgreen"),
         lwd = c(0.8, 1, NA, 0.9, 1.4, NA),
         bty = "o", bg = "white", box.col = "grey70", cex = 0.75)

  invisible(NULL)
}


# ---------------------------------------------------------------------------
# process_scn
# Parse a .SCN file, run detection and statistics for every core, optionally
# write annotated PNGs, and return the results list.
# ---------------------------------------------------------------------------
process_scn <- function(filepath,
                        ew_lw_threshold   = 500L,
                        min_ring_mm       = 2,
                        smooth_n          = 5L,
                        air_threshold     = 200L,
                        prominence_frac   = 0.08,
                        manual_boundaries = NULL,
                        rings_offset      = NULL,
                        plot_dir          = NULL) {

  cores   <- parse_scn(filepath)
  results <- vector("list", length(cores))
  names(results) <- names(cores)

  if (!is.null(plot_dir)) dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

  for (core_id in names(cores)) {
    core <- cores[[core_id]]
    d    <- trim_air_channels(core$density, threshold = air_threshold)

    bounds <- detect_ring_boundaries(
      density           = d,
      step_mm           = core$step_mm,
      ew_lw_threshold   = ew_lw_threshold,
      min_ring_mm       = min_ring_mm,
      smooth_n          = smooth_n,
      prominence_frac   = prominence_frac,
      manual_boundaries = manual_boundaries[[core_id]]
    )

    off <- if (!is.null(rings_offset) && !is.null(rings_offset[[core_id]]))
             as.integer(rings_offset[[core_id]]) else 0L

    stats <- ring_statistics(
      density         = d,
      boundaries      = bounds,
      step_mm         = core$step_mm,
      ew_lw_threshold = ew_lw_threshold,
      rings_offset    = off
    )

    if (!is.null(plot_dir)) {
      safe <- gsub("[^A-Za-z0-9_-]", "_", core_id)
      plot_density_profile(
        density         = d,
        boundaries      = bounds,
        stats           = stats,
        step_mm         = core$step_mm,
        ew_lw_threshold = ew_lw_threshold,
        core_id         = core_id,
        file            = file.path(plot_dir, paste0("core_", safe, ".png"))
      )
    }

    results[[core_id]] <- list(core = core, density = d,
                               boundaries = bounds, stats = stats)
  }

  results
}


# ===========================================================================
# Operator tools: parity checking, per-core tuning, suspect-ring review
# ===========================================================================

# ---------------------------------------------------------------------------
# parse_dat
# Read an operator-edited .DAT file into a named list of per-core data frames,
# one row per ring. Used as the reference for parity checks and tuning.
# ---------------------------------------------------------------------------
parse_dat <- function(filepath) {
  lines    <- readLines(filepath, warn = FALSE)
  core_hdr <- grep("^Core : ", lines)
  ids      <- trimws(sub("^Core : ", "", lines[core_hdr]))

  cols <- c("ring", "year", "outer_radius_mm", "ring_width_mm", "ew_width_mm",
            "lw_width_mm", "pct_latewood", "incr_area_cm2", "total_area_cm2",
            "ring_mean", "ew_density", "lw_density", "uniformity",
            "min_density", "max_density", "range_density")

  res <- list()
  for (i in seq_along(core_hdr)) {
    start <- core_hdr[i]
    end   <- if (i < length(core_hdr)) core_hdr[i + 1L] - 1L else length(lines)
    block <- lines[start:end]
    rowln <- grep("^\\s*[0-9]+\\s+[0-9]{4}\\s+", block, value = TRUE)
    if (!length(rowln)) next
    m <- lapply(rowln, function(L) as.numeric(strsplit(trimws(L), "\\s+")[[1]]))
    m <- m[vapply(m, length, integer(1)) >= 16L]
    if (!length(m)) next
    df <- as.data.frame(do.call(rbind, lapply(m, function(v) v[1:16])))
    names(df) <- cols
    res[[ids[i]]] <- df
  }
  res
}


# ---------------------------------------------------------------------------
# compare_to_dat
# Per-core comparison of detected results against a parsed .DAT. Reports the
# ring-count difference and, on the overlapping outer rings, the RMSE of ring
# width and ring-mean density. Cores are matched by base ID.
# ---------------------------------------------------------------------------
compare_to_dat <- function(results, dat) {
  dat_base <- sub("[abc]$", "", names(dat))
  out <- data.frame()
  for (cid in names(results)) {
    r_stats <- results[[cid]]$stats
    pick    <- which(dat_base == cid)
    if (!length(pick)) next
    d_n  <- sum(vapply(dat[pick], nrow, integer(1)))
    d_df <- do.call(rbind, dat[pick])

    n_ov <- min(nrow(r_stats), nrow(d_df))
    rw_r <- rev(r_stats$ring_width_mm)[seq_len(n_ov)]
    rw_d <- rev(d_df$ring_width_mm)[seq_len(n_ov)]
    rm_r <- rev(r_stats$ring_mean)[seq_len(n_ov)]
    rm_d <- rev(d_df$ring_mean)[seq_len(n_ov)]

    out <- rbind(out, data.frame(
      core_id       = cid,
      n_rings_R     = nrow(r_stats),
      n_rings_DAT   = d_n,
      diff          = nrow(r_stats) - d_n,
      width_rmse_mm = round(sqrt(mean((rw_r - rw_d)^2)), 2),
      ringmean_rmse = round(sqrt(mean((rm_r - rm_d)^2)), 1),
      stringsAsFactors = FALSE
    ))
  }
  out
}


# ---------------------------------------------------------------------------
# calibrate_prominence
# Search prominence_frac over a grid for the value whose detected ring count
# best matches target_n for one core. On ties the larger fraction is chosen.
# ---------------------------------------------------------------------------
calibrate_prominence <- function(density, target_n,
                                 step_mm         = 0.3,
                                 ew_lw_threshold = 500L,
                                 min_ring_mm     = 2,
                                 smooth_n        = 5L,
                                 grid            = seq(0.02, 0.30, by = 0.005)) {
  ns <- vapply(grid, function(f) {
    b <- detect_ring_boundaries(density, step_mm, ew_lw_threshold,
                                min_ring_mm, smooth_n, prominence_frac = f)
    length(b) + 1L
  }, integer(1))
  err  <- abs(ns - target_n)
  best <- which(err == min(err))
  bf   <- grid[best[length(best)]]
  list(prominence_frac = bf,
       n_rings         = ns[match(bf, grid)],
       target          = target_n,
       grid            = data.frame(prominence_frac = grid, n_rings = ns))
}


# ---------------------------------------------------------------------------
# calibrate_to_dat
# Calibrate prominence_frac per core against the ring counts in a parsed .DAT.
# ---------------------------------------------------------------------------
calibrate_to_dat <- function(cores, dat, air_threshold = 200L, ...) {
  dat_base <- sub("[abc]$", "", names(dat))
  out <- data.frame()
  for (cid in names(cores)) {
    pick <- which(dat_base == cid)
    if (!length(pick)) next
    target <- sum(vapply(dat[pick], nrow, integer(1)))
    d      <- trim_air_channels(cores[[cid]]$density, air_threshold)
    cal    <- calibrate_prominence(d, target, step_mm = cores[[cid]]$step_mm, ...)
    out <- rbind(out, data.frame(core_id = cid, target_n = target,
                                 prominence_frac = cal$prominence_frac,
                                 n_rings = cal$n_rings,
                                 stringsAsFactors = FALSE))
  }
  out
}


# ---------------------------------------------------------------------------
# apply_ring_edits
# Apply operator boundary corrections to a detected ring set and recompute
# statistics. edits is a named list keyed by ring_no, each value one of
# "merge-left", "merge-right", "split <ch>", or "keep". All edits are resolved
# to channel positions against the original numbering before any are applied.
# ---------------------------------------------------------------------------
apply_ring_edits <- function(density, boundaries, edits,
                             step_mm         = 0.3,
                             ew_lw_threshold = 500L,
                             rings_offset    = 0L) {
  b      <- as.integer(boundaries)
  n      <- length(density)
  starts <- c(1L, b)
  remove_ch <- integer(0); add_ch <- integer(0)

  for (rn in names(edits)) {
    k   <- as.integer(rn) - rings_offset
    act <- tolower(trimws(edits[[rn]]))
    if (act %in% c("merge-left", "merge_left", "mergeleft", "l")) {
      if (k >= 2L && k <= length(starts)) remove_ch <- c(remove_ch, starts[k])
    } else if (act %in% c("merge-right", "merge_right", "mergeright", "r")) {
      if (k >= 1L && (k + 1L) <= length(starts)) remove_ch <- c(remove_ch, starts[k + 1L])
    } else if (startsWith(act, "split") || startsWith(act, "s")) {
      ch <- as.integer(gsub("[^0-9]", "", act))
      if (!is.na(ch)) add_ch <- c(add_ch, ch)
    }
  }

  b  <- setdiff(b, remove_ch)
  b  <- sort(unique(c(b, add_ch)))
  b  <- b[b >= 2L & b <= n]
  nb <- detect_ring_boundaries(density, step_mm, ew_lw_threshold,
                               manual_boundaries = b)
  ring_statistics(density, nb, step_mm, ew_lw_threshold, rings_offset)
}


# ---------------------------------------------------------------------------
# review_suspects
# Step through the suspect rings of a detected core and resolve each. An
# interactive session prompts per ring; when decisions is supplied (a named
# list keyed by ring_no) it runs unattended. Returns corrected statistics.
# ---------------------------------------------------------------------------
review_suspects <- function(density, boundaries,
                            step_mm         = 0.3,
                            ew_lw_threshold = 500L,
                            rings_offset    = 0L,
                            decisions       = NULL) {
  stats <- ring_statistics(density, boundaries, step_mm, ew_lw_threshold, rings_offset)
  susp  <- which(stats$suspect)
  if (!length(susp)) return(stats)

  if (!is.null(decisions))
    return(apply_ring_edits(density, boundaries, decisions,
                            step_mm, ew_lw_threshold, rings_offset))
  if (!interactive()) return(stats)

  edits <- list()
  for (i in susp) {
    rn <- stats$ring_no[i]
    cat(sprintf("Ring %d  width %.1f mm  %s\n",
                rn, stats$ring_width_mm[i], stats$suspect_reason[i]))
    ans <- trimws(readline("  Enter=keep  l=merge-left  r=merge-right  s<ch>=split : "))
    if (ans %in% c("l", "left"))        edits[[as.character(rn)]] <- "merge-left"
    else if (ans %in% c("r", "right"))  edits[[as.character(rn)]] <- "merge-right"
    else if (startsWith(ans, "s"))      edits[[as.character(rn)]] <- paste0("split", gsub("[^0-9]", "", ans))
  }
  apply_ring_edits(density, boundaries, edits, step_mm, ew_lw_threshold, rings_offset)
}
