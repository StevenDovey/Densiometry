# ---------------------------------------------------------------------------
# FRI Direct-Scanning X-Ray Densitometer — data processing
#
# Reimplements the EDITOR program from:
#   Cown & Clement (1983) "A Wood Densitometer Using Direct Scanning with
#   X-Rays." Wood Sci. Technol. 17:91-99.
#
# Pipeline:  parse_scn()  ->  trim_air_channels()  ->
#            detect_ring_boundaries()  ->  ring_statistics()
#
# Top-level wrapper: process_scn()
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# parse_scn
# Read a raw .SCN file and return a named list of core objects.
# Two-piece scans (suffix "a"/"b") are automatically merged.
#
# SCN header field positions (after "####"):
#   [1] core_id  [2] rings_offset  [3] critical_count  [4] attenuation_factor
#   [5] step_mm  [6] air_count     [7] scan_year        [8] unknown_field
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

    core_id           <- parts[1]
    rings_offset      <- as.integer(parts[2])
    critical_count    <- as.integer(parts[3])
    attenuation_factor <- as.numeric(parts[4])
    step_mm           <- as.numeric(parts[5])
    air_count         <- as.numeric(parts[6])
    scan_year         <- as.integer(parts[7])
    unknown_field     <- if (length(parts) >= 8L) as.integer(parts[8]) else NA_integer_

    data_lines <- lines[(header_idx[i] + 1L):(term_idx[i] - 1L)]
    density    <- as.integer(unlist(strsplit(trimws(data_lines), "\\s+")))
    density    <- density[!is.na(density)]

    cores[[i]] <- list(
      core_id            = core_id,
      rings_offset       = rings_offset,
      critical_count     = critical_count,
      attenuation_factor = attenuation_factor,
      step_mm            = step_mm,
      air_count          = air_count,
      scan_year          = scan_year,
      unknown_field      = unknown_field,
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
    merged$scan_year  <- piece_b$scan_year
    merged$air_count  <- mean(c(piece_a$air_count, piece_b$air_count))

    # Remove the two piece entries and add the merged one.
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
# detect_ring_boundaries
# Return integer vector of channel positions that mark the FIRST channel of
# each new ring (rings 2, 3, ..., N).  Ring 1 always starts at channel 1.
#
# Algorithm:
#   1. 5-point centred running mean to suppress noise.
#   2. Detect downward crossings through ew_lw_threshold (LW -> EW transition).
#   3. Enforce minimum ring width (min_ring_mm) to suppress false rings.
#
# Attribute "ew_only_flags": logical vector of length n_rings, TRUE for rings
# whose density never reached ew_lw_threshold (earlywood-only).
#
# manual_boundaries: integer vector — when supplied, bypasses detection.
# ---------------------------------------------------------------------------
detect_ring_boundaries <- function(density,
                                   step_mm          = 0.3,
                                   ew_lw_threshold  = 500L,
                                   min_ring_mm      = 2,
                                   smooth_n         = 5L,
                                   manual_boundaries = NULL) {

  if (!is.null(manual_boundaries)) {
    mb <- sort(as.integer(manual_boundaries))
    mb <- mb[mb >= 2L & mb <= length(density)]
    attr(mb, "manual") <- TRUE
    return(mb)
  }

  n            <- length(density)
  min_channels <- ceiling(min_ring_mm / step_mm)

  # Step 1 — smooth
  smoothed <- as.numeric(stats::filter(density, rep(1 / smooth_n, smooth_n), sides = 2))
  smoothed[is.na(smoothed)] <- density[is.na(smoothed)]

  # Step 2 — downward crossings (TRUE -> FALSE through threshold)
  is_lw     <- smoothed >= ew_lw_threshold
  candidates <- which(is_lw[-n] & !is_lw[-1L]) + 1L

  # Step 3 — minimum ring width gate
  boundaries <- integer(0)
  last_start <- 1L

  for (b in candidates) {
    if ((b - last_start) >= min_channels) {
      boundaries <- c(boundaries, b)
      last_start <- b
    }
  }

  # Annotate rings that never reached latewood threshold.
  ring_starts <- c(1L, boundaries)
  ring_ends   <- c(boundaries - 1L, n)
  n_rings     <- length(ring_starts)

  ew_only <- logical(n_rings)
  for (k in seq_len(n_rings)) {
    ew_only[k] <- !any(density[ring_starts[k]:ring_ends[k]] >= ew_lw_threshold)
  }
  attr(boundaries, "ew_only_flags") <- ew_only

  boundaries
}


# ---------------------------------------------------------------------------
# ring_statistics
# Compute per-ring summary statistics matching EDITOR program output.
#
# Arguments:
#   density         — integer vector from trim_air_channels()
#   boundaries      — integer vector from detect_ring_boundaries()
#   step_mm         — mm per channel (from SCN header)
#   ew_lw_threshold — earlywood/latewood density boundary (kg/m3)
#   rings_offset    — rings from pith not captured (0 = scan starts at pith)
#   scan_year       — calendar year of outermost (most recent) ring
#
# Returns a data.frame with one row per ring.
# ---------------------------------------------------------------------------
ring_statistics <- function(density,
                            boundaries,
                            step_mm          = 0.3,
                            ew_lw_threshold  = 500L,
                            rings_offset     = 0L,
                            scan_year        = NA_integer_) {

  ew_only_flags <- attr(boundaries, "ew_only_flags")

  ring_starts <- c(1L, boundaries)
  ring_ends   <- c(boundaries - 1L, length(density))
  n_rings     <- length(ring_starts)

  if (is.null(ew_only_flags) || length(ew_only_flags) != n_rings)
    ew_only_flags <- rep(FALSE, n_rings)

  year_vec <- if (!is.na(scan_year)) {
    as.integer(scan_year) - (n_rings - seq_len(n_rings))
  } else {
    rep(NA_integer_, n_rings)
  }

  out <- data.frame(
    ring_from_pith  = rings_offset + seq_len(n_rings),
    year            = year_vec,
    outer_radius_mm = round(ring_ends * step_mm, 1),
    ring_width_mm   = round((ring_ends - ring_starts + 1L) * step_mm, 1),
    ew_width_mm     = NA_real_,
    lw_width_mm     = NA_real_,
    pct_latewood    = NA_real_,
    ring_mean       = NA_integer_,
    ew_density      = NA_integer_,
    lw_density      = NA_integer_,
    min_density     = NA_integer_,
    max_density     = NA_integer_,
    uniformity      = NA_integer_,
    range_density   = NA_integer_,
    partial_ring    = FALSE,
    ew_only         = ew_only_flags,
    stringsAsFactors = FALSE
  )

  # First ring is partial when scan doesn't start at pith.
  if (rings_offset > 0L) out$partial_ring[1L] <- TRUE

  for (k in seq_len(n_rings)) {
    d  <- density[ring_starts[k]:ring_ends[k]]
    ew <- d[d <  ew_lw_threshold]
    lw <- d[d >= ew_lw_threshold]

    out$ew_width_mm[k]   <- round(length(ew) * step_mm, 1)
    out$lw_width_mm[k]   <- round(length(lw) * step_mm, 1)
    out$pct_latewood[k]  <- round(100 * length(lw) / length(d))
    out$ring_mean[k]     <- round(mean(d))
    out$ew_density[k]    <- if (length(ew) > 0L) round(mean(ew)) else NA_integer_
    out$lw_density[k]    <- if (length(lw) > 0L) round(mean(lw)) else NA_integer_
    out$min_density[k]   <- min(d)
    out$max_density[k]   <- max(d)
    out$uniformity[k]    <- if (length(ew) > 0L && length(lw) > 0L)
                              round(mean(lw) - mean(ew)) else NA_integer_
    out$range_density[k] <- max(d) - min(d)
  }

  out
}


# ---------------------------------------------------------------------------
# format_editor_output
# Print a table to the console matching the EDITOR program output layout.
# ---------------------------------------------------------------------------
format_editor_output <- function(stats, core_info) {
  cat(sprintf(
    "\n--- Core: %-6s  Step: %.1f mm  Threshold: 500 kg/m³  Scan year: %d ---\n",
    core_info$core_id, core_info$step_mm, core_info$scan_year
  ))
  cat(sprintf(
    "    Air count: %.0f  Attenuation: %.2f  Channels: %d  Length: %.1f mm\n\n",
    core_info$air_count, core_info$attenuation_factor,
    core_info$n_channels, core_info$n_channels * core_info$step_mm
  ))

  header <- sprintf(
    "%-6s %-6s %-8s %-7s %-6s %-6s %-5s %-6s %-6s %-6s %-5s %-5s %-6s %-5s",
    "Ring", "Year", "OutRad", "Width", "EW", "LW", "%LW",
    "Mean", "EWden", "LWden", "Min", "Max", "Unif", "Range"
  )
  cat(header, "\n")
  cat(strrep("-", nchar(header)), "\n")

  for (i in seq_len(nrow(stats))) {
    r        <- stats[i, ]
    ring_lbl <- sprintf("%d%s", r$ring_from_pith, ifelse(r$partial_ring, "*", ""))
    lw_d     <- if (is.na(r$lw_density)) " ---" else sprintf("%-6d", r$lw_density)
    unif     <- if (is.na(r$uniformity)) " ---" else sprintf("%-6d", r$uniformity)

    cat(sprintf(
      "%-6s %-6s %-8.1f %-7.1f %-6.1f %-6.1f %-5.0f %-6.0f %-6s %-6s %-5.0f %-5.0f %-6s %-5.0f\n",
      ring_lbl,
      ifelse(is.na(r$year), "-", as.character(r$year)),
      r$outer_radius_mm,
      r$ring_width_mm,
      r$ew_width_mm,
      r$lw_width_mm,
      r$pct_latewood,
      r$ring_mean,
      if (is.na(r$ew_density)) " ---" else sprintf("%-6d", r$ew_density),
      lw_d,
      r$min_density,
      r$max_density,
      unif,
      r$range_density
    ))
  }
  cat("\n")
  invisible(NULL)
}


# ---------------------------------------------------------------------------
# plot_density_profile
# Base R density trace plot analogous to Fig. 7 of Cown & Clement (1983).
# ---------------------------------------------------------------------------
plot_density_profile <- function(density,
                                 boundaries      = NULL,
                                 step_mm         = 0.3,
                                 ew_lw_threshold = 500L,
                                 core_id         = "",
                                 scan_year       = NA) {

  x_mm  <- seq_along(density) * step_mm
  y_max <- max(density, na.rm = TRUE) + 50L

  plot(
    x_mm, density,
    type  = "l",
    lwd   = 0.8,
    col   = "grey30",
    xlab  = "Distance from pith (mm)",
    ylab  = expression("Density (kg m"^{-3}*")"),
    main  = paste0("Core ", core_id,
                   if (!is.na(scan_year)) paste0("  —  ", scan_year) else ""),
    ylim  = c(0, y_max),
    las   = 1
  )

  abline(h = ew_lw_threshold, col = "firebrick", lty = 2, lwd = 1)

  if (!is.null(boundaries) && length(boundaries) > 0L)
    abline(v = boundaries * step_mm, col = "steelblue3", lty = 3, lwd = 0.9)

  legend(
    "topleft",
    legend = c("Density", sprintf("EW/LW threshold (%d)", ew_lw_threshold), "Ring boundary"),
    col    = c("grey30", "firebrick", "steelblue3"),
    lty    = c(1, 2, 3),
    lwd    = c(0.8, 1, 0.9),
    bty    = "n",
    cex    = 0.8
  )

  invisible(NULL)
}


# ---------------------------------------------------------------------------
# process_scn
# Top-level function: parse a .SCN file, run the full pipeline for every
# core, print EDITOR-format tables, optionally plot, and return results.
#
# manual_boundaries: named list; names = core IDs, values = integer vectors
#   of boundary channel positions for manual override of specific cores.
# ---------------------------------------------------------------------------
process_scn <- function(filepath,
                        ew_lw_threshold   = 500L,
                        min_ring_mm       = 2,
                        smooth_n          = 5L,
                        air_threshold     = 200L,
                        manual_boundaries = NULL,
                        plot              = TRUE) {

  cores   <- parse_scn(filepath)
  results <- vector("list", length(cores))
  names(results) <- names(cores)

  for (core_id in names(cores)) {
    core <- cores[[core_id]]

    d <- trim_air_channels(core$density, threshold = air_threshold)

    mb <- manual_boundaries[[core_id]]

    bounds <- detect_ring_boundaries(
      density          = d,
      step_mm          = core$step_mm,
      ew_lw_threshold  = ew_lw_threshold,
      min_ring_mm      = min_ring_mm,
      smooth_n         = smooth_n,
      manual_boundaries = mb
    )

    stats <- ring_statistics(
      density         = d,
      boundaries      = bounds,
      step_mm         = core$step_mm,
      ew_lw_threshold = ew_lw_threshold,
      rings_offset    = core$rings_offset,
      scan_year       = core$scan_year
    )

    format_editor_output(stats, core)

    if (plot) {
      plot_density_profile(
        density         = d,
        boundaries      = bounds,
        step_mm         = core$step_mm,
        ew_lw_threshold = ew_lw_threshold,
        core_id         = core_id,
        scan_year       = core$scan_year
      )
    }

    results[[core_id]] <- list(core = core, boundaries = bounds, stats = stats)
  }

  invisible(results)
}
