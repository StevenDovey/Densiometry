# ---------------------------------------------------------------------------
# FRI Direct-Scanning X-Ray Densitometer — data processing
#
# Reimplements the EDITOR program from:
#   Cown & Clement (1983) "A Wood Densitometer Using Direct Scanning with
#   X-Rays." Wood Sci. Technol. 17:91-99.   (System software upgraded 1990s.)
#
# Pipeline:  parse_scn()  ->  trim_air_channels()  ->
#            detect_ring_boundaries()  ->  ring_statistics()
#
# Top-level wrapper: process_scn()
#
# Ring detection is prominence-based (not a single fixed density cut) so that
# weak juvenile-wood latewood is still found, and ambiguous boundaries are
# flagged as "suspect" (possible false / intra-annual rings) for operator
# review — mirroring the interactive editing step of the DOS EDITOR.
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# parse_scn
# Read a raw .SCN file and return a named list of core objects.
# Two-piece scans (suffix "a"/"b") are automatically merged.
#
# SCN header field positions (after "####"):
#   [1] core_id  [2] field2  [3] critical_count  [4] attenuation_factor
#   [5] step_mm  [6] air_count [7] scan_code      [8] aux_field
#
# NOTE: field2 and scan_code are operator/acquisition metadata; the calendar
# year and pith ring index are assigned during editing and are NOT recoverable
# from the .SCN.  Rings are therefore labelled by position (1 = innermost
# captured ring) and the operator supplies pith offset / year afterwards.
# ---------------------------------------------------------------------------
parse_scn <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)

  header_idx <- grep("^####", lines)
  term_idx   <- grep("^\\*{4}$", lines)

  if (length(header_idx) != length(term_idx))
    stop("parse_scn: mismatch between '####' headers and '****' terminators in ", filepath)

  cores <- vector("list", length(header_idx))

  for (i in seq_along(header_idx)) {
    h     <- sub("^####\\s*", "", lines[header_idx[i]])
    parts <- strsplit(trimws(h), "\\s+")[[1]]

    core_id            <- parts[1]
    field2             <- as.integer(parts[2])
    critical_count     <- as.integer(parts[3])
    attenuation_factor <- as.numeric(parts[4])
    step_mm            <- as.numeric(parts[5])
    air_count          <- as.numeric(parts[6])
    scan_code          <- as.integer(parts[7])
    aux_field          <- if (length(parts) >= 8L) as.integer(parts[8]) else NA_integer_

    data_lines <- lines[(header_idx[i] + 1L):(term_idx[i] - 1L)]
    density    <- as.integer(unlist(strsplit(trimws(data_lines), "\\s+")))
    density    <- density[!is.na(density)]

    cores[[i]] <- list(
      core_id            = core_id,
      field2             = field2,
      critical_count     = critical_count,
      attenuation_factor = attenuation_factor,
      step_mm            = step_mm,
      air_count          = air_count,
      scan_code          = scan_code,
      aux_field          = aux_field,
      density            = density,
      n_channels         = length(density)
    )
    names(cores)[i] <- core_id
  }

  # Merge two-piece scans (pairs sharing the same base ID with "a"/"b" suffix).
  base_ids <- sub("[ab]$", "", names(cores))
  duplicated_bases <- unique(base_ids[duplicated(base_ids)])

  for (base in duplicated_bases) {
    pieces <- cores[base_ids == base]
    if (length(pieces) != 2L)
      warning("parse_scn: expected exactly 2 pieces for core ", base,
              ", got ", length(pieces), " — skipping merge")

    piece_a <- pieces[[which(endsWith(names(pieces), "a"))]]
    piece_b <- pieces[[which(endsWith(names(pieces), "b"))]]

    merged <- piece_a
    merged$core_id    <- base
    merged$density    <- c(piece_a$density, piece_b$density)
    merged$n_channels <- length(merged$density)
    merged$scan_code  <- piece_b$scan_code
    merged$air_count  <- mean(c(piece_a$air_count, piece_b$air_count))

    cores[names(pieces)] <- NULL
    cores[[base]] <- merged
  }

  cores
}


# ---------------------------------------------------------------------------
# trim_air_channels
# Remove leading low-density channels produced when the beam enters the wood.
# Only the leading edge is affected; the trailing edge stops scanning at air.
# ---------------------------------------------------------------------------
trim_air_channels <- function(density, threshold = 200L) {
  start <- which(density >= threshold)[1L]
  if (is.na(start)) {
    warning("trim_air_channels: no channel >= threshold; returning unchanged")
    return(density)
  }
  density[start:length(density)]
}


# ---------------------------------------------------------------------------
# .running_mean — centred running mean (base R, NA-safe at the ends).
# ---------------------------------------------------------------------------
.running_mean <- function(x, n = 5L) {
  if (n <= 1L) return(as.numeric(x))
  sm <- as.numeric(stats::filter(x, rep(1 / n, n), sides = 2))
  sm[is.na(sm)] <- x[is.na(sm)]
  sm
}


# ---------------------------------------------------------------------------
# .local_extrema — indices of local maxima (peaks) and minima (troughs).
# Plateaus are resolved to their leading edge.
# ---------------------------------------------------------------------------
.local_extrema <- function(x) {
  d <- sign(diff(x))
  # carry the last non-zero slope across plateaus
  for (i in which(d == 0)) d[i] <- if (i > 1L) d[i - 1L] else 0
  peaks   <- which(d[-length(d)] > 0 & d[-1] < 0) + 1L
  troughs <- which(d[-length(d)] < 0 & d[-1] > 0) + 1L
  list(peaks = peaks, troughs = troughs)
}


# ---------------------------------------------------------------------------
# detect_ring_boundaries
# Locate annual ring boundaries (latewood -> earlywood transitions) from the
# density trace.  Returns the integer channel positions that mark the FIRST
# channel of each new ring (rings 2..N); ring 1 always starts at channel 1.
#
# Algorithm:
#   1. Smooth (running mean) to suppress detector noise.
#   2. Find latewood peaks (local maxima) and earlywood troughs (local minima).
#   3. Keep peaks whose *prominence* (height above the higher adjacent trough)
#      exceeds an adaptive threshold = prominence_frac * (max - min).  This is
#      relative, so weak inner-ring latewood is retained where a fixed 500
#      cut-off would miss it.
#   4. Enforce a minimum ring width (min_ring_mm); merge closer peaks.
#   5. Place each boundary at the steepest density drop after a latewood peak.
#
# Attributes attached to the returned vector (all length = n_rings):
#   ew_only_flags  — TRUE where the ring never reached ew_lw_threshold
#   peak_channel   — channel of the ring's latewood peak
#   peak_density   — smoothed density at that peak
#   prominence     — peak prominence (kg/m3)
#   suspect        — TRUE where the boundary/ring is low-confidence
#   suspect_reason — short text reason (possible false ring, weak LW, narrow…)
#
# manual_boundaries: integer vector — when supplied, bypasses detection.
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
    attr(mb, "manual")        <- TRUE
    n_rings                   <- length(mb) + 1L
    ring_starts               <- c(1L, mb)
    ring_ends                 <- c(mb - 1L, n)
    ew_only <- vapply(seq_len(n_rings), function(k)
      !any(density[ring_starts[k]:ring_ends[k]] >= ew_lw_threshold), logical(1))
    attr(mb, "ew_only_flags") <- ew_only
    attr(mb, "suspect")        <- rep(FALSE, n_rings)
    attr(mb, "suspect_reason") <- rep("", n_rings)
    return(mb)
  }

  sm  <- .running_mean(density, smooth_n)
  ext <- .local_extrema(sm)
  peaks   <- ext$peaks
  troughs <- ext$troughs

  if (length(peaks) == 0L) {
    boundaries <- integer(0)
    attr(boundaries, "ew_only_flags")  <- TRUE
    attr(boundaries, "peak_channel")   <- which.max(sm)
    attr(boundaries, "peak_density")   <- max(sm)
    attr(boundaries, "prominence")     <- 0
    attr(boundaries, "suspect")        <- TRUE
    attr(boundaries, "suspect_reason") <- "no latewood peak detected"
    return(boundaries)
  }

  # Prominence of each peak relative to its nearest troughs on each side.
  span <- max(sm) - min(sm)
  thr  <- prominence_frac * span
  prom <- numeric(length(peaks))
  for (j in seq_along(peaks)) {
    p  <- peaks[j]
    lt <- troughs[troughs < p]; rt <- troughs[troughs > p]
    left_min  <- if (length(lt)) sm[max(lt)] else min(sm[1:p])
    right_min <- if (length(rt)) sm[min(rt)] else min(sm[p:n])
    prom[j]   <- sm[p] - max(left_min, right_min)
  }

  keep   <- prom >= thr
  peaks  <- peaks[keep]
  prom   <- prom[keep]
  if (length(peaks) == 0L) { peaks <- ext$peaks[which.max(prom)]; prom <- max(prom) }

  # Enforce minimum spacing between latewood peaks; keep the stronger of a pair.
  ord <- order(peaks); peaks <- peaks[ord]; prom <- prom[ord]
  repeat {
    if (length(peaks) < 2L) break
    gaps <- diff(peaks)
    too_close <- which(gaps < min_ch)
    if (length(too_close) == 0L) break
    k <- too_close[1L]
    drop <- if (prom[k] >= prom[k + 1L]) k + 1L else k
    peaks <- peaks[-drop]; prom <- prom[-drop]
  }

  n_rings  <- length(peaks)
  peak_den <- sm[peaks]

  # Boundary between ring k and k+1 = steepest density drop after peak k.
  boundaries <- integer(0)
  if (n_rings >= 2L) {
    for (k in seq_len(n_rings - 1L)) {
      p1 <- peaks[k]; p2 <- peaks[k + 1L]
      seg_trough <- p1 + which.min(sm[p1:p2]) - 1L          # earlywood low
      if (seg_trough <= p1 + 1L) {
        b <- p1 + 1L
      } else {
        dd <- diff(sm[p1:seg_trough])                       # descent profile
        b  <- p1 + which.min(dd)                            # steepest drop
      }
      boundaries <- c(boundaries, b)
    }
  }
  boundaries <- sort(unique(pmin(pmax(boundaries, 2L), n)))

  # Per-ring summaries aligned to ring_starts/ends.
  ring_starts <- c(1L, boundaries)
  ring_ends   <- c(boundaries - 1L, n)
  n_rings     <- length(ring_starts)

  ew_only <- logical(n_rings)
  for (k in seq_len(n_rings))
    ew_only[k] <- !any(density[ring_starts[k]:ring_ends[k]] >= ew_lw_threshold)

  # Flag suspect rings (candidate false / intra-annual or unreliable).
  widths_mm <- (ring_ends - ring_starts + 1L) * step_mm
  med_w     <- stats::median(widths_mm)
  suspect        <- logical(n_rings)
  suspect_reason <- character(n_rings)
  for (k in seq_len(n_rings)) {
    reasons <- character(0)
    pk_d <- if (k <= length(peak_den)) peak_den[k] else NA_real_
    pk_p <- if (k <= length(prom))     prom[k]     else NA_real_
    if (!is.na(pk_d) && pk_d < ew_lw_threshold)
      reasons <- c(reasons, "weak latewood (<thr)")
    if (!is.na(pk_p) && pk_p < 1.5 * thr)
      reasons <- c(reasons, "low prominence")
    if (widths_mm[k] < 0.45 * med_w)
      reasons <- c(reasons, "narrow ring")
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
# Compute per-ring summary statistics matching EDITOR program output, plus
# cross-sectional areas (cm^2) and false-ring suspect flags.
#
# Rings are labelled by position: ring_no = rings_offset + 1, 2, ... (1 =
# innermost captured ring when rings_offset = 0).  Year is intentionally NOT
# computed here — it is an operator-assigned column added downstream.
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

  pad <- function(v, fill) if (is.null(v) || length(v) != n_rings) rep(fill, n_rings) else v
  ew_only_flags  <- pad(ew_only_flags,  FALSE)
  suspect_flags  <- pad(suspect_flags,  FALSE)
  suspect_reason <- pad(suspect_reason, "")
  peak_density   <- pad(peak_density,   NA_integer_)
  prominence     <- pad(prominence,     NA_integer_)

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

    r_out <- outer_radius_mm[k] / 10            # cm
    r_in  <- inner_radius_mm[k] / 10            # cm

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
# format_editor_output
# Print a table to the console, ring-number labelled (no year).  Suspect
# rings are marked with "?" for operator attention.
# ---------------------------------------------------------------------------
format_editor_output <- function(stats, core_info) {
  cat(sprintf(
    "\n--- Core: %-6s  Step: %.1f mm  EW/LW thr: 500 kg/m3  Channels: %d  Length: %.1f mm ---\n",
    core_info$core_id, core_info$step_mm, core_info$n_channels,
    core_info$n_channels * core_info$step_mm
  ))
  n_susp <- sum(stats$suspect)
  cat(sprintf("    Rings detected: %d   Suspect (review): %d\n\n", nrow(stats), n_susp))

  header <- sprintf(
    "%-5s %-7s %-6s %-6s %-6s %-5s %-6s %-6s %-6s %-5s %-5s %-5s %-3s",
    "Ring", "OutRad", "Width", "EW", "LW", "%LW",
    "Mean", "EWden", "LWden", "Min", "Max", "Rng", "?"
  )
  cat(header, "\n")
  cat(strrep("-", nchar(header)), "\n")

  for (i in seq_len(nrow(stats))) {
    r   <- stats[i, ]
    lbl <- sprintf("%d%s", r$ring_no, ifelse(r$partial_ring, "*", ""))
    cat(sprintf(
      "%-5s %-7.1f %-6.1f %-6.1f %-6.1f %-5.0f %-6.0f %-6s %-6s %-5.0f %-5.0f %-5.0f %-3s\n",
      lbl, r$outer_radius_mm, r$ring_width_mm, r$ew_width_mm, r$lw_width_mm,
      r$pct_latewood, r$ring_mean,
      if (is.na(r$ew_density)) "---" else sprintf("%d", r$ew_density),
      if (is.na(r$lw_density)) "---" else sprintf("%d", r$lw_density),
      r$min_density, r$max_density, r$range_density,
      ifelse(r$suspect, "?", "")
    ))
  }
  if (n_susp > 0L) {
    cat("\n  Suspect rings (possible false / intra-annual — confirm or merge):\n")
    sidx <- which(stats$suspect)
    for (i in sidx)
      cat(sprintf("    ring %d: %s\n", stats$ring_no[i], stats$suspect_reason[i]))
  }
  cat("\n")
  invisible(NULL)
}


# ---------------------------------------------------------------------------
# plot_density_profile
# Annotated base-R density trace: latewood shaded, ring boundaries drawn,
# ring numbers labelled along the top, latewood peaks marked, and suspect
# (possible false) rings highlighted in red for operator review.
# Writes a PNG when `file` is supplied; otherwise draws to the current device.
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
  y_max <- max(density, na.rm = TRUE) + 80L

  op <- par(mar = c(4.2, 4.5, 3, 1)); on.exit(par(op), add = TRUE)
  plot(x_mm, density, type = "n",
       xlab = "Distance from inner edge (mm)",
       ylab = expression("Density (kg m"^{-3}*")"),
       main = paste0("Core ", core_id, " — density profile & ring boundaries"),
       ylim = c(0, y_max), las = 1)

  # Shade latewood (density >= threshold).
  lw_mask <- density >= ew_lw_threshold
  if (any(lw_mask)) {
    rl <- rle(lw_mask); ends <- cumsum(rl$lengths); starts <- ends - rl$lengths + 1L
    for (j in which(rl$values))
      rect(x_mm[starts[j]] - step_mm / 2, 0, x_mm[ends[j]] + step_mm / 2, y_max,
           col = "#ffe9b0", border = NA)
  }

  abline(h = ew_lw_threshold, col = "firebrick", lty = 2, lwd = 1)
  lines(x_mm, density, lwd = 0.8, col = "grey25")

  # Ring boundaries and ring-number labels.
  ring_starts <- c(1L, boundaries)
  ring_ends   <- c(boundaries - 1L, length(density))
  if (!is.null(boundaries) && length(boundaries) > 0L) {
    susp_b <- if (!is.null(stats)) stats$suspect[-1] else rep(FALSE, length(boundaries))
    abline(v = boundaries * step_mm,
           col = ifelse(susp_b, "red", "steelblue3"),
           lty = 3, lwd = ifelse(susp_b, 1.4, 0.9))
  }

  # Mark latewood peaks.
  pk <- attr(boundaries, "peak_channel")
  if (!is.null(pk)) points(pk * step_mm, density[pk], pch = 25, col = "darkgreen",
                           bg = "darkgreen", cex = 0.6)

  # Ring numbers along the top; suspects in red.
  if (!is.null(stats)) {
    mids <- ((ring_starts + ring_ends) / 2) * step_mm
    cols <- ifelse(stats$suspect, "red", "grey20")
    text(mids, y_max * 0.97, labels = stats$ring_no, cex = 0.6, col = cols, srt = 90)
  }

  legend("bottomright",
         legend = c("Density", sprintf("EW/LW thr (%d)", ew_lw_threshold),
                    "Latewood", "Ring boundary", "Suspect ring", "LW peak"),
         col = c("grey25", "firebrick", "#ffe9b0", "steelblue3", "red", "darkgreen"),
         lty = c(1, 2, NA, 3, 3, NA), pch = c(NA, NA, 15, NA, NA, 25),
         pt.bg = c(NA, NA, NA, NA, NA, "darkgreen"),
         lwd = c(0.8, 1, NA, 0.9, 1.4, NA),
         bty = "o", bg = "white", box.col = "grey70", cex = 0.75)

  invisible(NULL)
}


# ---------------------------------------------------------------------------
# process_scn
# Top-level: parse a .SCN file, run the full pipeline for every core, print
# ring-number-labelled tables, write annotated PNGs, and return results.
#
# manual_boundaries: named list; names = core IDs, values = integer vectors of
#   boundary channel positions for manual override of specific cores.
# plot_dir: directory to write per-core annotated PNGs (NULL = skip plots).
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

    format_editor_output(stats, core)

    if (!is.null(plot_dir)) {
      safe <- gsub("[^A-Za-z0-9_-]", "_", core_id)
      plot_density_profile(
        density         = d,
        boundaries      = bounds,
        stats           = stats,
        step_mm         = core$step_mm,
        ew_lw_threshold = ew_lw_threshold,
        core_id         = core_id,
        file            = file.path(plot_dir, paste0("AK6_core_", safe, ".png"))
      )
    }

    results[[core_id]] <- list(core = core, density = d,
                               boundaries = bounds, stats = stats)
  }

  invisible(results)
}


# ===========================================================================
# Operator tools: parity checking, per-core tuning, suspect-ring review
# ===========================================================================

# ---------------------------------------------------------------------------
# parse_dat
# Read an operator-edited .DAT file into a named list of per-core data frames
# (one row per ring).  Used as ground truth for parity checks and tuning.
# Two-piece cores keep their "a"/"b" suffix as written in the .DAT.
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
    m <- lapply(rowln, function(L) suppressWarnings(as.numeric(strsplit(trimws(L), "\\s+")[[1]])))
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
# Per-core comparison of detected results against a parsed .DAT.  Reports the
# ring-count difference and, on the overlapping outer rings, the RMSE of ring
# width (mm) and ring-mean density (kg/m3).  Cores are matched by base ID
# (two-piece "a"/"b" .DAT cores are summed for the merged scan core).
# ---------------------------------------------------------------------------
compare_to_dat <- function(results, dat) {
  dat_base <- sub("[ab]$", "", names(dat))
  out <- data.frame()
  for (cid in names(results)) {
    r_stats <- results[[cid]]$stats
    pick    <- which(dat_base == cid)
    if (!length(pick)) next
    d_n     <- sum(vapply(dat[pick], nrow, integer(1)))
    d_df    <- do.call(rbind, dat[pick])

    n_ov <- min(nrow(r_stats), nrow(d_df))
    # align on the OUTER rings (bark end), where both sequences are reliable
    rw_r <- rev(r_stats$ring_width_mm)[seq_len(n_ov)]
    rw_d <- rev(d_df$ring_width_mm)[seq_len(n_ov)]
    rm_r <- rev(r_stats$ring_mean)[seq_len(n_ov)]
    rm_d <- rev(d_df$ring_mean)[seq_len(n_ov)]

    out <- rbind(out, data.frame(
      core_id        = cid,
      n_rings_R      = nrow(r_stats),
      n_rings_DAT    = d_n,
      diff           = nrow(r_stats) - d_n,
      width_rmse_mm  = round(sqrt(mean((rw_r - rw_d)^2)), 2),
      ringmean_rmse  = round(sqrt(mean((rm_r - rm_d)^2)), 1),
      stringsAsFactors = FALSE
    ))
  }
  out
}


# ---------------------------------------------------------------------------
# calibrate_prominence
# Search prominence_frac (over a grid) for the value whose detected ring count
# best matches target_n for a single core's density trace.  On ties the LARGER
# fraction is chosen (fewer, more confident rings).  Returns the chosen value,
# the achieved count, and the full grid for inspection.
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
  bf   <- grid[best[length(best)]]            # largest frac among ties
  list(prominence_frac = bf,
       n_rings         = ns[match(bf, grid)],
       target          = target_n,
       grid            = data.frame(prominence_frac = grid, n_rings = ns))
}


# ---------------------------------------------------------------------------
# calibrate_to_dat
# Convenience wrapper: calibrate prominence_frac per core against the ring
# counts in a parsed .DAT.  Returns a data.frame of the best fraction and
# resulting count for every core.
# ---------------------------------------------------------------------------
calibrate_to_dat <- function(cores, dat, air_threshold = 200L, ...) {
  dat_base <- sub("[ab]$", "", names(dat))
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
# statistics.  `edits` is a named list keyed by ring_no; each value is one of:
#   "merge-left"  — dissolve the boundary at this ring's inner edge
#   "merge-right" — dissolve the boundary at this ring's outer edge
#   "split <ch>"  — insert a new boundary at channel <ch>
#   "keep"        — no change
# All edits are resolved to channel positions against the ORIGINAL numbering
# first, so several edits can be applied in one pass without index drift.
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
      ch <- suppressWarnings(as.integer(gsub("[^0-9]", "", act)))
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
# Step through the suspect (possible false / intra-annual) rings of a detected
# core and resolve each.  In an interactive session it prompts per ring; when
# `decisions` is supplied (named list keyed by ring_no) it runs unattended,
# which also makes the workflow scriptable and testable.
# Returns the corrected ring-statistics data.frame.
# ---------------------------------------------------------------------------
review_suspects <- function(density, boundaries,
                            step_mm         = 0.3,
                            ew_lw_threshold = 500L,
                            rings_offset    = 0L,
                            decisions       = NULL) {
  stats <- ring_statistics(density, boundaries, step_mm, ew_lw_threshold, rings_offset)
  susp  <- which(stats$suspect)
  if (!length(susp)) { message("No suspect rings to review."); return(invisible(stats)) }

  if (!is.null(decisions)) {
    return(apply_ring_edits(density, boundaries, decisions,
                            step_mm, ew_lw_threshold, rings_offset))
  }

  if (!interactive()) {
    message(length(susp), " suspect ring(s); pass `decisions` to resolve non-interactively.")
    return(invisible(stats))
  }

  edits <- list()
  for (i in susp) {
    rn <- stats$ring_no[i]
    cat(sprintf("\nRing %d  | width %.1f mm | LW peak %s | %s\n",
                rn, stats$ring_width_mm[i],
                ifelse(is.na(stats$lw_peak_density[i]), "-", stats$lw_peak_density[i]),
                stats$suspect_reason[i]))
    ans <- trimws(readline("  [Enter]=keep  l=merge-left  r=merge-right  s<ch>=split : "))
    if (ans %in% c("l", "left"))        edits[[as.character(rn)]] <- "merge-left"
    else if (ans %in% c("r", "right"))  edits[[as.character(rn)]] <- "merge-right"
    else if (startsWith(ans, "s"))      edits[[as.character(rn)]] <- paste0("split", gsub("[^0-9]", "", ans))
  }
  apply_ring_edits(density, boundaries, edits, step_mm, ew_lw_threshold, rings_offset)
}
